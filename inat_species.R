library(tidyverse)
library(lubridate)
library(MASS)  # For glm.nb
library(rnaturalearth)
library(sf)
library(ggthemes)
library(VGAM)  # For vglm
library(gridExtra)
source("../inat_exp.R")
# load("./hawaii_inat.RData")
########################################################################################################################
# Subsetting Data
########################################################################################################################
region = "Hawaii"
bbox <- c(16.5, -179.5, 29.5, -152.5) # hawaii
# bbox = c(19.7121, -80.0365, 27.3775, -69.7489) # bahamas
# Subset `inat_data` and `inat_effort` based on the bounding box and extract month and year
inat_data_filtered <- inat_data %>%
  filter(latitude >= bbox[1], latitude <= bbox[3], longitude >= bbox[2], longitude <= bbox[4]) %>%
  mutate(
    latitude_bin = floor(latitude),
    longitude_bin = floor(longitude),
    time_observed = ymd_hms(time_observed_at),
    month_observed = month(time_observed),
    year_observed = year(time_observed),
    date_observed = make_date(year_observed, month_observed)
  ) %>%
  filter(!is.na(date_observed))

inat_effort_filtered <- inat_effort %>%
  filter(latitude >= bbox[1], latitude <= bbox[3], longitude >= bbox[2], longitude <= bbox[4]) %>%
  mutate(
    latitude_bin = floor(latitude),
    longitude_bin = floor(longitude),
    time_observed = ymd_hms(time_observed_at),
    month_observed = month(time_observed),
    year_observed = year(time_observed),
    date_observed = make_date(year_observed, month_observed)
  ) %>%
  filter(!is.na(date_observed))

#-------------------------------------------------------------------
# Remove zero's when same user observes a shark (monthly resolution) 
#-------------------------------------------------------------------

# Identify unique user_id and date_observed combinations for shark observations
shark_obs_combinations <- inat_data_filtered %>%
  dplyr::select(id, user_id, date_observed) %>%
  distinct()

# Mark shark observations in `inat_effort`
inat_effort_filtered <- inat_effort_filtered %>%
  mutate(is_shark_obs = if_else((user_id %in% shark_obs_combinations$user_id) & (date_observed %in% shark_obs_combinations$date_observed)
                                & !(id %in% shark_obs_combinations$id), TRUE, FALSE))

inat_effort_filtered = inat_effort_filtered[inat_effort_filtered$is_shark_obs==FALSE,]

#-------------------------------------------------------------------
# Remove aquarium and non-validated sightings
#-------------------------------------------------------------------
# remove non-research grade and aquarium sightings
# remove sightings where at least two people agree on the species, and no one disagrees (species-specific)
inat_data_filtered = subset(inat_data_filtered, quality_grade=="research" & 
                              captive_cultivated=="false" &
                              (num_identification_agreements > 0 & num_identification_disagreements < 1))

inat_effort_filtered = subset(inat_effort_filtered, quality_grade=="research" & captive_cultivated=="false")

#-------------------------------------------------------------------
# Choose a species
#-------------------------------------------------------------------
species_list <- unique(inat_data_filtered$scientific_name)
plots <- list()

for (species in species_list) {
  
  inat_data_filtered_species <- subset(inat_data_filtered, scientific_name == species)
  
  # Summarize observation effort and shark sightings by year and month
  grid_summary <- inat_data_filtered_species %>%
    group_by(year_observed, latitude_bin, longitude_bin) %>%
    summarise(shark_observations = n(), .groups = 'drop')
  
  effort_summary <- inat_effort_filtered %>%
    group_by(year_observed, latitude_bin, longitude_bin) %>%
    summarise(total_observations = n(), .groups = 'drop')
  
  # Combine effort and shark observations using full_join
  combined_summary <- full_join(grid_summary, effort_summary, by = c("year_observed", "latitude_bin", "longitude_bin")) %>%
    mutate(
      shark_observations = replace_na(shark_observations, 0),
      total_observations = replace_na(total_observations, 0)
    ) %>%
    mutate(
      total_observations = ifelse(total_observations <= shark_observations, shark_observations, total_observations), # add missed effort
      non_shark_observations = (total_observations - shark_observations), # calculate number of non-shark observations
      spue = (shark_observations / total_observations),  # Sighting Per Unit Effort (SPUE)
      log_effort = ifelse(total_observations == 0, 0, log(total_observations + 1)),
      log_non_shark = ifelse(non_shark_observations == 0, 0, log(non_shark_observations + 1)),
      spue = replace_na(spue, 0)
    )
  
  # Filter the combined_summary to exclude zero SPUE values
  combined_summary_filtered <- combined_summary %>%
    filter(shark_observations > 0)
  
  # Fit a zero-truncated negative binomial model using VGAM with tryCatch
  zt_nb_model <- tryCatch(
    {
      vglm(
        shark_observations ~ year_observed + offset(log_non_shark) + 
                              latitude_bin + longitude_bin,
        family = posnegbinomial(),
        data = combined_summary_filtered
      )
    },
    error = function(e) {
      message(paste("Error fitting model for species:", species, "\n", e))
      return(NULL)
    }
  )
  
  if(is.null(zt_nb_model)) {next}
  summary(zt_nb_model)
  # Predict and add predictions to the data frame
  combined_summary_filtered <- combined_summary_filtered %>%
    mutate(predicted_shark_observations = predict(zt_nb_model, type = "response"))
  
  # Calculate SPUE and confidence intervals 
  combined_summary_filtered <- combined_summary_filtered %>%
    mutate(
      spue_predicted = (100 * predicted_shark_observations / total_observations),
      lower_ci = qpois(0.025, lambda = 100 * predicted_shark_observations) / total_observations,
      upper_ci = qpois(0.975, lambda = 100 * predicted_shark_observations) / total_observations
    ) %>%
    mutate(residuals = shark_observations - predicted_shark_observations)
  
  spue_trend_plot_black_axes <- ggplot(combined_summary_filtered, aes(x = year_observed)) +
    geom_point(aes(y = log(spue_predicted)), color = "red") +
    geom_smooth(aes(y = log(spue_predicted)), method = "glm", color = "blue", se = FALSE) +
    geom_errorbar(aes(ymin = log(lower_ci), ymax = log(upper_ci)), width = 0.2) +
    labs(
      title = paste(species, "-", region, sep = " "),
      x = "Year",
      y = "Relative Abundance"
    ) +
    scale_x_continuous(breaks = seq(floor(min(combined_summary_filtered$year_observed)), 
                                    ceiling(max(combined_summary_filtered$year_observed)), 
                                    by = 5)) +
    theme_minimal() +
    theme(
      axis.line = element_line(color = "black"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
  
  plots[[species]] <- spue_trend_plot_black_axes
}

# Save all plots to a single PDF file
pdf(paste("./figures/",region,"_species_relative_abundance.pdf", sep=""), width = 11, height = 6)
for (species in species_list) {
  print(plots[[species]])
}
dev.off()


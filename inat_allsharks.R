library(tidyverse)
library(lubridate)
library(MASS)  # For glm.nb
library(rnaturalearth)
library(sf)
library(ggthemes)
library(VGAM)  # For vglm
source("./inat_exp.R")
# load("./hawaii_inat.RData")
########################################################################################################################
# Subsetting Data
########################################################################################################################
region = "hawaii"
bbox <- c(16.5, -179.5, 29.5, -152.5) # hawaii
# bbox <- c(19.7121, -80.0365, 27.3775, -69.7489) # bahamas
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

#------------------------------------------------------------
#     Summarize observations by date (monthly resolution)
#------------------------------------------------------------
# Summarize observation effort and shark sightings by year and month
grid_summary <- inat_data_filtered %>%
  group_by(date_observed) %>%
  summarise(shark_observations = n(), .groups = 'drop')

effort_summary <- inat_effort_filtered %>%
  group_by(date_observed) %>%
  summarise(total_observations = n(), .groups = 'drop')

# Create a sequence of all dates present in the effort dataset
all_dates <- inat_effort_filtered %>%
  reframe(date_observed = seq(min(date_observed), max(date_observed), by = "month"))

# Combine effort and shark observations using full_join
combined_summary <- full_join(all_dates, effort_summary, by = c("date_observed")) %>%
  left_join(grid_summary, by = c("date_observed")) %>%
  mutate(
    shark_observations = replace_na(shark_observations, 0),
    total_observations = replace_na(total_observations, 0)
  ) %>%
  mutate(
    total_observations = ifelse(total_observations<=shark_observations,shark_observations,total_observations), # add missed effort
    non_shark_observations = (total_observations - shark_observations), # calculate number of non-shark observations
    spue = (shark_observations / total_observations),  # Sighting Per Unit Effort (SPUE)
    log_effort = ifelse(total_observations==0, 0, log(total_observations+1)),
    log_non_shark = ifelse(non_shark_observations==0, 0, log(non_shark_observations+1)),
    month = month(date_observed),  # Extract the month from the date_observed
    spue = replace_na(spue, 0),
    year = year(date_observed)
  )

########################################################################################################################
# Initial Modeling
########################################################################################################################

# Filter the combined_summary to exclude zero SPUE values
combined_summary_filtered <- combined_summary %>%
  filter(shark_observations > 0)

# Fit a zero-truncated negative binomial model using VGAM
zt_nb_model <- vglm(
  shark_observations ~ date_observed + log_effort,
  family = posnegbinomial(),
  data = combined_summary_filtered
)

# Summary of the model
summary(zt_nb_model)
plot(zt_nb_model)
# Predict and add predictions to the data frame
combined_summary_filtered <- combined_summary_filtered %>%
  mutate(predicted_shark_observations = predict(zt_nb_model, type = "response"))

# Calculate SPUE and confidence intervals 
combined_summary_filtered <- combined_summary_filtered %>%
  mutate(
    spue_predicted = (100*predicted_shark_observations / total_observations),
    lower_ci = qpois(0.025, lambda = 100*predicted_shark_observations) / total_observations,
    upper_ci = qpois(0.975, lambda = 100*predicted_shark_observations) / total_observations
  ) %>%
  mutate(residuals = shark_observations - predicted_shark_observations)

# Plot observed vs. predicted shark observations with black axes
observed_vs_predicted_plot <- ggplot(combined_summary_filtered, aes(x = shark_observations, y = predicted_shark_observations)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "Observed vs. Predicted Shark Observations",
    x = "Observed Shark Observations",
    y = "Predicted Shark Observations"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black")
  )

print(observed_vs_predicted_plot)

ggsave(paste("./figures/", region, "_obspred.png", sep=""), plot = observed_vs_predicted_plot, width = 5, height = 3, dpi = 300)

# Plot the SPUE trend with black axes
spue_trend_plot_black_axes <- ggplot(combined_summary_filtered, aes(x = date_observed)) +
  geom_point(aes(y = log(spue_predicted)), color = "red") +
  geom_smooth(aes(y = log(spue_predicted)), method = "loess", color = "blue", se = FALSE) +
  geom_errorbar(aes(ymin = log(lower_ci), ymax = log(upper_ci)), width = 0.2) +
  labs(
    title = paste(region, "Relative Abundance", sep= " "),
    x = "Year",
    y = "Log(Sightings per 100 iNaturalist Users)"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Display the SPUE trend plot with black axes
print(spue_trend_plot_black_axes)

ggsave(paste("./figures/", region, "_spue.png", sep=""), plot = spue_trend_plot_black_axes, width = 6, height = 4, dpi = 300)

# Plot residuals
residuals_plot <- ggplot(combined_summary_filtered, aes(x = predicted_shark_observations, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(
    title = "Residuals of the Model",
    x = "Predicted Shark Observations",
    y = "Residuals"
  ) +
  theme_minimal()

print(residuals_plot)

ggsave(paste("./figures/", region, "_residual.png", sep=""), plot = residuals_plot, width = 5, height = 3, dpi = 300)

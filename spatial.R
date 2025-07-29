library(tidyverse)
library(lubridate)
library(VGAM)
library(ggplot2)
library(ggmap)
library(viridis)
source("./inat_exp.R")
# load("./hawaii_inat.RData")
########################################################################################################################
# Subsetting Data
########################################################################################################################
region = "Bahamas"
# bbox <- c(16.5, -179.5, 29.5, -152.5) # hawaii
bbox = c(19.7121, -80.0365, 27.3775, -69.7489) # bahamas
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
species = "Galeocerdo cuvier"

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

# Fit a zero-truncated negative binomial model using MASS with tryCatch
zt_nb_model <- tryCatch(
  {
    MASS::glm.nb(shark_observations ~ year_observed + latitude_bin * longitude_bin + offset(log_effort), 
                 data = combined_summary_filtered)
  },
  error = function(e) {
    message(paste("Error fitting model for species:", species, "\n", e))
    return(NULL)
  }
)
summary(zt_nb_model)
# Predict and add predictions to the data frame
combined_summary_filtered <- combined_summary_filtered %>%
  mutate(predicted_shark_observations = predict(zt_nb_model, type = "response"))

# Group by year and calculate mean predicted SPUE and confidence intervals
combined_summary_yearly <- combined_summary_filtered %>%
  group_by(year_observed) %>%
  summarise(
    mean_predicted_shark_observations = mean(predicted_shark_observations),
    mean_total_observations = mean(total_observations)
  ) %>%
  mutate(
    spue_predicted = (100 * mean_predicted_shark_observations / mean_total_observations),
    lower_ci = qpois(0.025, lambda = 100 * mean_predicted_shark_observations) / mean_total_observations,
    upper_ci = qpois(0.975, lambda = 100 * mean_predicted_shark_observations) / mean_total_observations
  )

# Calculate residuals
combined_summary_yearly <- combined_summary_yearly %>%
  mutate(residuals = mean_predicted_shark_observations - (spue_predicted * mean_total_observations / 100))

# Determine map bounds
lon_min <- bbox[2] 
lon_max <- bbox[4] 
lat_min <- bbox[1] 
lat_max <- bbox[3]
# Create a grid within the bounding box
grid <- expand.grid(
  longitude_bin = seq(lon_min, lon_max, by = 0.5),
  latitude_bin = seq(lat_min, lat_max, by = 0.5)
)
grid$year_observed <- max(combined_summary_filtered$year_observed, na.rm = TRUE)
grid$log_effort <- mean(combined_summary_filtered$log_effort, na.rm = TRUE)
grid$total_observations <- mean(combined_summary_filtered$total_observations, na.rm = TRUE)

# Predict abundance on the grid
grid$predicted_shark_observations <- predict(zt_nb_model, newdata = grid, type = "response")
grid$predicted_spue = grid$predicted_shark_observations / grid$total_observations
# grid$log_predicted_spue = ifelse(log(grid$predicted_spue)<0, 0, log(grid$predicted_spue))

# Create a kernel density heatmap using stat_density_2d
world <- map_data("world")

kernel_density_map <- ggplot() +
  geom_map(data = world, map = world,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "white", color = "black", size = 0.5) +
  geom_tile(data = grid, aes(x = longitude_bin, y = latitude_bin, fill = log(predicted_spue)), alpha = 0.8) +
  scale_fill_viridis_c(name = "Predicted Abundance") +
  geom_point(data = inat_data_filtered_species, 
             aes(x = longitude, y = latitude), 
             color = "red", size = 1, alpha = 0.6) +
  labs(
    title = paste("Predicted Abundance Landscape -", species),
    x = "Longitude",
    y = "Latitude"
  ) +
  xlim(lon_min-4, lon_max+4) +
  ylim(lat_min-4, lat_max+4) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Display the kernel density map plot
print(kernel_density_map)

library(tidyverse)
library(lubridate)
library(MASS)  # For glm.nb
library(rnaturalearth)
library(sf)
library(ggthemes)
library(cowplot)
library(VGAM)  # For vglm
source("./inat_exp.R")
load("./all_inat.RData")
########################################################################################################################
# Subsetting Data
########################################################################################################################
region = "bahamas"
# bbox <- c(16.5, -179.5, 29.5, -152.5) # hawaii
bbox <- c(19.7121, -80.0365, 27.3775, -69.7489) # bahamas
# bbox <- c(12.4621, -98.0365, 30.9, -65.0489) # Southwest Atlantic
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

# Can you compare the vglm and glm.nb models? Which seems better for this scenario and why?

# Fit a traditional zero-truncated negative binomial model using VGAM
zt_nb_model <- vglm(
  shark_observations ~ date_observed + log_effort,
  family = posnegbinomial(),
  data = combined_summary_filtered
)

# Fit a traditional zero-truncated negative binomial model using VGAM
zt_nb_model2 <- MASS::glm.nb(
  shark_observations ~ date_observed + log_effort, 
                  data = combined_summary_filtered)

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

# ggsave(paste("./figures/", region, "_obspred.png", sep=""), plot = observed_vs_predicted_plot, width = 5, height = 3, dpi = 300)

# Create the world map with a red box around the bounding box area
world <- map_data("world")
map_with_bbox <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey55", color = "black") +
  geom_rect(aes(xmin = bbox[2], xmax = bbox[4], ymin = bbox[1], ymax = bbox[3]), color = "red", fill = NA, size = 1) +
  coord_fixed(ratio = 1.3, xlim = c(-120, -50), ylim = c(0, 60)) +
  theme_void() + # Remove all axes and labels for simplicity
  theme(
    plot.background = element_rect(color = "black", size = 1.5)  # Black border around the inset map
  )
# Your existing SPUE trend plot
spue_trend_plot_black_axes <- ggplot(combined_summary_filtered, aes(x = date_observed)) +
  geom_point(aes(y = log(spue_predicted)), color = "red") +
  geom_smooth(aes(y = log(spue_predicted)), method = "loess", 
              color = "blue", se = FALSE, fullrange = TRUE, span = 1) +
  geom_errorbar(aes(ymin = log(lower_ci), ymax = log(upper_ci)), width = 0.2) +
  labs(
    title = paste("iNaturalist - ZTNB (Bahamas, 16 species)", sep = " "),
    x = "Year",
    y = "Relative Abundance"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
  )

# Combine the plots, inset the world map into the trend plot
final_plot <- ggdraw() +
  draw_plot(spue_trend_plot_black_axes) +
  draw_plot(map_with_bbox, x = -0.025, y = 0.1, width = 0.4, height = 0.4)  # Adjust position and size of the inset map

# Print the final plot
print(final_plot)

ggsave(paste("./figures/", region, "_spue2.png", sep=""), plot = final_plot, width = 16, height = 10, dpi = 300)

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

# ggsave(paste("./figures/", region, "_residual.png", sep=""), plot = residuals_plot, width = 5, height = 3, dpi = 300)

# Plot observed vs. predicted shark observations with black axes
observed_vs_predicted_spue <- ggplot(combined_summary_filtered, aes(x = spue_predicted/100, y = spue)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "Observed vs. Predicted SPUE",
    x = "Observed Shark SPUE",
    y = "Predicted Shark SPUE"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black")
  )

print(observed_vs_predicted_spue)

ggsave(paste("./figures/", region, "_spueVSpred.png", sep=""), plot = observed_vs_predicted_spue, width = 5, height = 10, dpi = 300)

########################################################################################################################
# Define the bounding box for the Bahamas region
bbox <- c(left = -80.0365, bottom = 19.7121, right = -69.7489, top = 27.3775)
bbox_rect <- c(19.7121, -80.0365, 27.3775, -69.7489) # bahamas
world <- map_data("world")
# Second map plot for inat_data_filtered
inat_obs_map = ggplot(inat_data_filtered, aes(x = longitude, y = latitude)) +
  stat_bin_hex(binwidth = 1, aes(fill = log(..count.. + 1)), color = "black") +  # Create hexagonal bins with a scale
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", name = "Log(Observations)") +  # Color scale for the bins
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey55", color = "black") +
  coord_fixed(ratio = 1, xlim = c(bbox[1]-5, bbox[3]+5), ylim = c(bbox[2]-5, bbox[4]+5)) +
  theme_minimal() +
  labs(
    title = "iNaturalist Shark Observations",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 16, color = "black"),
    plot.title = element_text(hjust = 0.5, size = 21, face = "bold"),  # Center and enlarge title
    panel.grid.major = element_line(color = "lightgrey", size = 0.5),  # Coordinate grid lines
    panel.grid.minor = element_line(color = "lightgrey", size = 0.25)  # Minor coordinate grid lines
  )

inat_effort_map = ggplot(inat_effort_filtered, aes(x = longitude, y = latitude)) +
  stat_bin_hex(binwidth = 1, aes(fill = log(..count.. + 1)), color = "black") +  # Create hexagonal bins with a scale
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", name = "Log(Observations)") +  # Color scale for the bins
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey55", color = "black") +
  geom_rect(aes(xmin = bbox_rect[2]-1, xmax = bbox_rect[4]+1, ymin = bbox_rect[1]-1, ymax = bbox_rect[3]+1), color = "red", fill = NA, size = 1) +
  coord_fixed(ratio = 1, xlim = c(bbox[1]-5, bbox[3]+5), ylim = c(bbox[2]-5, bbox[4]+5)) +
  theme_minimal() +
  labs(
    title = "iNaturalist User Activity",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 16, color = "black"),
    plot.title = element_text(hjust = 0.5, size = 21, face = "bold"),  # Center and enlarge title
    panel.grid.major = element_line(color = "lightgrey", size = 0.5),  # Coordinate grid lines
    panel.grid.minor = element_line(color = "lightgrey", size = 0.25)  # Minor coordinate grid lines
  )


ggsave(paste("./figures/", region, "_obsMAP.png", sep=""), plot = inat_obs_map, width = 9, height = 8, dpi = 300)
ggsave(paste("./figures/", region, "_effortMAP.png", sep=""), plot = inat_effort_map, width = 9, height = 8, dpi = 300)

########################################################################################################################
bbox <- c(left = -98.0365, bottom = 12.4621, right = -65.0489, top = 30.9)
world <- map_data("world")
# Second map plot for inat_data_filtered
inat_obs_map_sw = ggplot(inat_data_filtered, aes(x = longitude, y = latitude)) +
  stat_bin_hex(binwidth = 1, aes(fill = log(..count.. + 1)), color = "black") +  # Create hexagonal bins with a scale
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", name = "Log(Observations)") +  # Color scale for the bins
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey25", color = "gray60") +
  coord_fixed(ratio = 1, xlim = c(bbox[1]-5, bbox[3]+5), ylim = c(bbox[2]-5, bbox[4]+5)) +
  theme_minimal() +
  labs(
    title = "iNaturalist Shark Observations",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 16, color = "black"),
    plot.title = element_text(hjust = 0.5, size = 21, face = "bold"),  # Center and enlarge title
    panel.grid.major = element_line(color = "lightgrey", size = 0.5),  # Coordinate grid lines
    panel.grid.minor = element_line(color = "lightgrey", size = 0.25)  # Minor coordinate grid lines
  )

ggsave(paste("./figures/", region, "_obsMAP_sw.png", sep=""), plot = inat_obs_map_sw, width = 9, height = 8, dpi = 300)





library(sharkPulseR)
library(RPostgres)
library(ggmap)
library(dplyr)
library(sf)
library(maps)
library(ggplot2)
library(ggforce)
library(hexbin)

source("./cleanData.R")
# Set up the database connection
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "pelagic",
                 host = "sp2.cs.vt.edu",
                 port = 5432,
                 user = "spr",
                 password = "spr_pass")

# Fetch all valid scientific names from 'taxonomy3' where 'superorder' equals 'Selachimorpha'
valid_names_query <- "SELECT valid FROM taxonomy3 WHERE superorder = 'Selachimorpha'"
valid_names <- dbGetQuery(con, valid_names_query)
dbDisconnect(con)

# getSharkPulse
data = getSharkPulse(dbuser = "spr", dbpass = "spr_pass")
data = cleanData(data, "../project_inat")

# Filter sharkPulse for Social Network sightings
data = data %>%
  filter(!is.na(latitude) | !is.na(longitude)) %>%
  filter(!is.na(date)) %>%
  filter(source_type %in% c("Flickr", "Instagram", "iNaturalist")) %>%
  filter(species_name %in% valid_names$valid) 

####################################################################################

# Bin the data by latitude and longitude
bin_width <- 14  # Change this for finer or coarser bins

data_binned <- data %>%
  mutate(
    lat_bin = floor(latitude / bin_width) * bin_width,
    lon_bin = floor(longitude / bin_width) * bin_width
  ) %>%
  group_by(lat_bin, lon_bin, source_type) %>%
  summarise(count = n()) %>%
  ungroup()

# Calculate the proportions within each bin
data_pie <- data_binned %>%
  group_by(lat_bin, lon_bin) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(proportion = count / total)

# Load the world map data
world <- map_data("world")

# Create the map with enhanced aesthetics and save it as a PNG
map <- ggplot() +
  # Draw pie charts representing proportions
  geom_arc_bar(
    data = data_pie,
    aes(
      x0 = lon_bin, y0 = lat_bin,
      r0 = 0, r = 6,      # Adjust pie chart size here
      amount = proportion,
      fill = source_type
    ),
    stat = "pie"
  ) +
  # Draw the world map
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey55", color = "white") +
  # Customize color for different sources
  scale_fill_manual(values = c("Flickr" = "blue", "iNaturalist" = "green", "Instagram" = "red")) +
  
  # Add coordinate lines (grid)
  theme_minimal(base_size = 15) + 
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  
  # Add axis and background elements
  theme(
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 16, color = "black"),
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),  # Center and enlarge title
    panel.grid.major = element_line(color = "lightgrey", size = 0.5),  # Coordinate grid lines
    panel.grid.minor = element_line(color = "lightgrey", size = 0.25)  # Minor coordinate grid lines
  ) +
  labs(
    title = "sharkPulse Social Network Observations",
    x = "Longitude",
    y = "Latitude",
    fill = "Source"
  )

# Save the map to a PNG file
ggsave("./figures/shark_observations_map.png", plot = map, width = 12, height = 8, dpi = 300)

####################################################################################

# Summarize the data
data_summary <- data %>%
  group_by(source_type) %>%
  summarise(
    num_records = n(),
    num_unique_species = n_distinct(species_name)
  )

# Reshape the data for plotting
data_long <- data_summary %>%
  pivot_longer(cols = c(num_records, num_unique_species), names_to = "metric", values_to = "count") %>%
  mutate(metric = recode(metric, 
                       "num_records" = "Records", 
                       "num_unique_species" = "Species"))

# Faceted bar charts for each metric with updated labels
metric = ggplot(data_long, aes(x = source_type, y = count, fill = source_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5, size = 5) +  # Add count labels above bars
  facet_wrap(~metric, scales = "free_y") +  # Separate facets for each metric
  scale_fill_manual(values = c("Flickr" = "blue", "iNaturalist" = "green", "Instagram" = "red")) +
  labs(
    title = "",
    x = "",
    y = "Count"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    panel.grid = element_blank(),  # Remove background grid
    legend.position = "none",      # Remove legend
    strip.text = element_text(size = 18, face = "bold")  # Increase size of facet labels
  )

# Print the plot
print(metric)


# Save the map to a PNG file
ggsave("./figures/shark_observations_metrics.png", plot = metric, width = 12, height = 8, dpi = 300)

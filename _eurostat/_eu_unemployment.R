# Install necessary packages if not already installed
if (!requireNamespace("eurostat", quietly = TRUE)) install.packages("eurostat")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")

# Load the packages
library(eurostat)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyverse)
library(viridis)

# Fetch unemployment data from Eurostat
unemp_data <- get_eurostat("lfst_r_lfu3rt")

# View the structure of the data
str(unemp_data)

# Filter the data to include only entries where 'geo' has exactly four characters (nuts0), 'isced11' is 'ED5-8', and 'age' is 'Y15-29'
filtered_data <- unemp_data %>%
  filter(nchar(geo) == 2, isced11 == 'ED5-8', age == 'Y15-29')

# Clean the TIME_PERIOD column to keep only the year (first four characters)
filtered_data <- filtered_data %>%
  mutate(year = substr(TIME_PERIOD, 1, 4))

# View the cleaned data
head(filtered_data)

# Load the NUTS 2 shapefile for Europe using get_eurostat_geospatial
nuts2_shapefile <- get_eurostat_geospatial(
  output_class = "sf",
  resolution = "20",    # Resolution of 1:20 million
  nuts_level = "0",     # NUTS 0 level
  year = "2024",        # Use the latest available year
  crs = "4326",        # WGS84 projection
  make_valid = TRUE     # Ensure the geometries are valid
)

# View the structure of the NUTS 2 shapefile
str(nuts2_shapefile)

# Merge the unemployment data with the NUTS 2 shapefile
unemp_map_data <- filtered_data %>%
  left_join(nuts2_shapefile, by = c("geo" = "NUTS_ID"))

# Ensure the geometry column is an sf object
unemp_map_data <- st_as_sf(unemp_map_data, sf_column_name = "geometry")

# Filter out rows with NA values in the 'values' or 'geometry' columns
unemp_map_data <- unemp_map_data %>%
  filter(!is.na(values), !is.na(geometry))

# Check the filtered data
head(unemp_map_data)
str(unemp_map_data)

# Define the extent for geographical Europe
europe_extent <- c(xmin = -11, xmax = 40, ymin = 35, ymax = 72)  # Longitude and latitude bounds

# Plot the map with the adjusted extent
ggplot(unemp_map_data) +
  geom_sf(aes(fill = values)) +  # Fill regions based on the 'values' column
  scale_fill_viridis(
    name = "Unemployment Rate",  # Legend title
    option = "plasma",           # Color palette
    direction = -1,              # Reverse the color gradient
    na.value = "grey"            # Color for NA values (if any)
  ) +
  coord_sf(xlim = europe_extent[c(1, 2)], ylim = europe_extent[c(3, 4)]) +  # Set the extent
  theme_minimal() +              # Minimal theme for the plot
  theme(legend.position = "bottom") +  # Place the legend at the bottom
  labs(
    title = "Unemployment Rate in Europe (2024)",  # Plot title
    subtitle = "Data Source: Eurostat",           # Subtitle
    caption = "Created with R and ggplot2"       # Caption
  )

### include all countries on map

# Load the shapefile for all European countries
europe_shapefile <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent == "Europe")

# Merge the unemployment data with the shapefile for all European countries
unemp_map_data <- full_join(europe_shapefile, unemp_map_data, by = c("admin" = "geo"))

# Check the merged data
head(unemp_map_data)
str(unemp_map_data)

# Plot the map with countries without data in grey
ggplot(unemp_map_data) +
  geom_sf(aes(fill = values), color = "black", size = 0.25) +  # Fill regions based on the 'values' column
  scale_fill_viridis(
    name = "Unemployment Rate",  # Legend title
    option = "plasma",           # Color palette
    direction = -1,              # Reverse the color gradient
    na.value = "grey"            # Color for NA values (if any)
  ) +
  coord_sf(xlim = europe_extent[c(1, 2)], ylim = europe_extent[c(3, 4)]) +  # Set the extent
  theme_minimal() +              # Minimal theme for the plot
  theme(legend.position = "bottom") +  # Place the legend at the bottom
  labs(
    title = "Unemployment Rate in Europe (2024)",  # Plot title
    subtitle = "Data Source: Eurostat",           # Subtitle
    caption = "Created with R and ggplot2"       # Caption
  )


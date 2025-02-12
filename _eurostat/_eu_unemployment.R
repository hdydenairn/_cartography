# Install necessary packages if not already installed
if (!requireNamespace("eurostat", quietly = TRUE)) install.packages("eurostat")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("giscoR", quietly = TRUE)) install.packages("giscoR")
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")

# Load the packages
library(eurostat)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyverse)
library(giscoR)
library(viridis)

# Fetch unemployment data from Eurostat
unemp_data <- get_eurostat("lfst_r_lfu3rt")

# View the structure of the data
str(unemp_data)

# Filter the data to include only entries where 'geo' has exactly four characters, 'isced11' is 'ED5-8', and 'age' is 'Y15-29'
filtered_data <- unemp_data %>%
  filter(nchar(geo) == 4, isced11 == 'ED5-8', age == 'Y15-29')

# Clean the TIME_PERIOD column to keep only the year (first four characters)
filtered_data <- filtered_data %>%
  mutate(year = substr(TIME_PERIOD, 1, 4))

# View the cleaned data
head(filtered_data)

# Load the NUTS 2 shapefile for Europe
nuts2_shapefile <- gisco_get_nuts(
  year = "2024",       # Use the latest available year
  epsg = "4326",       # WGS84 projection
  resolution = "20",    # Resolution of 1:20 million
  spatialtype = "RG",   # Return regions as MULTIPOLYGON/POLYGON
  nuts_level = "2",    # NUTS 2 level
  verbose = TRUE       # Display information for debugging
)

# View the structure of the NUTS 2 shapefile
str(nuts2_shapefile)

# Merge the unemployment data with the NUTS 2 shapefile
unemp_map_data <- filtered_data %>%
  left_join(nuts2_shapefile, by = c("geo" = "NUTS_ID"))

# Convert the geometry column to an sf object
unemp_map_data <- st_as_sf(unemp_map_data, sf_column_name = "geometry")

# Filter out rows with NA values in the 'values' or 'geometry' columns
unemp_map_data <- unemp_map_data %>%
  filter(!is.na(values), !is.na(geometry))

# Check the filtered data
head(unemp_map_data)
str(unemp_map_data)

# Plot the map
ggplot(unemp_map_data) +
  geom_sf(aes(fill = values)) +  # Fill regions based on the 'values' column
  scale_fill_viridis(
    name = "Unemployment Rate",  # Legend title
    option = "plasma",           # Color palette
    direction = -1,              # Reverse the color gradient
    na.value = "grey"            # Color for NA values (if any)
  ) +
  theme_minimal() +              # Minimal theme for the plot
  theme(legend.position = "bottom") +  # Place the legend at the bottom
  labs(
    title = "Unemployment Rate in Europe (2024)",  # Plot title
    subtitle = "Data Source: Eurostat",           # Subtitle
    caption = "Created with R and ggplot2"       # Caption
  )
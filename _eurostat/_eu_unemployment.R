# Install necessary packages
install.packages(c("eurostat", "dplyr", "ggplot2", "sf", "tidyverse"))

# Load the packages
library(eurostat)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyverse)

# Fetch unemployment data from Eurostat
unemp_data <- get_eurostat("lfst_r_lfu3rt")

# View the structure of the data
str(unemp_data)

# Filter the data to include only entries where 'geo' has exactly four characters
filtered_data <- unemp_data %>%
  filter(nchar(geo) == 4)

# View the filtered data
head(filtered_data)

# Filter data for the most recent year
unemp_map_data <- unemp_map_data %>%
  filter(year == max(year))

# Merge with a shapefile for mapping
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge the unemployment data with the world map data
unemp_map_data <- unemp_map_data %>%
  left_join(world, by = c("country" = "admin"))

# Create the map
ggplot(unemp_map_data) +
  geom_sf(aes(fill = unemployment_rate)) +
  scale_fill_viridis(name = "Unemployment Rate", option = "plasma", direction = -1, na.value = "grey") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Unemployment Rate in Europe (2024)",
       subtitle = "Data Source: Eurostat",
       caption = "Created with R and ggplot2")

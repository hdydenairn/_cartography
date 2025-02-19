# hdydenairn.github.io

# Install necessary packages
install.packages(c("eurostat", "dplyr", "ggplot2", "sf", "tidyverse"))

# Load the packages
library(eurostat)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyverse)

# Fetch unemployment data from Eurostat
unemp_data <- get_eurostat(dataset_code = "une_rt_a",
                                filters = list(geo = NULL,
                                               sex = "T",
                                               age = "TOTAL",
                                               unit = "PC_ACT",
                                               time = "2024M10"))

# View the structure of the data
str(unemp_data)

# Select relevant columns and rename them for clarity
unemp_map_data <- unemp_data %>%
  select(geo, time, values) %>%
  rename(country = geo,
         year = time,
         unemployment_rate = values)

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

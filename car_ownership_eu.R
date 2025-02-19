# Install necessary packages if necessary
# install.packages(c("eurostat", "dplyr", "ggplot2", "sf", "tidyverse", "showtext"))

# Load libraries
library(eurostat)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyverse)
library(showtext)

# Search for relevant dataset
eurostat_search <- search_eurostat("Passenger cars")
print(eurostat_search$title)

# Get data on car ownership per thousand inhabitants
road_eqs_carhab <- get_eurostat("road_eqs_carhab", time_format = "num")

# Filter data for the latest available year
latest_year <- max(road_eqs_carhab$TIME_PERIOD)
data_filtered <- road_eqs_carhab %>%
  filter(TIME_PERIOD == latest_year) %>%
  select(geo, values)

# Get map data for European countries
europe_map <- get_eurostat_geospatial(resolution = "60", nuts_level = 0)

# Merge with car ownership data
data_merged <- europe_map %>%
  left_join(data_filtered, by = c("geo" = "geo"))

# Create visualization

# Add font
font_add_google("Lato", "econ")
showtext_auto()

# Plot
ggplot(data_merged) +
  geom_sf(aes(fill = values), color = "white") +
  scale_fill_viridis_c(name = "Cars per 1000", option = "magma") +
  coord_sf(xlim = c(-25, 45), ylim = c(35, 72)) + # Focus on geographical Europe
  theme(
    plot.title = element_text(hjust = 0.5, family = "econ", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, family = "econ"),
    plot.caption = element_text(hjust = 0.5, family = "econ", face = "italic"),
    legend.title = element_text(family = "econ"),
    legend.text = element_text(family = "econ"),
    panel.background = element_rect(fill = "lightgrey"),
    plot.background = element_rect(fill = "lightgrey"),
    panel.grid = element_blank(),    # Remove gridlines
    axis.text = element_blank(),     # Remove axis numbers
    axis.ticks = element_blank(),    # Remove axis ticks
    axis.title = element_blank()     # Remove axis titles
  ) +
  labs(
    title = paste("Car Ownership per 1000 People in Europe (", latest_year, ")", sep=""),
    subtitle = "Data source: Eurostat",
    caption = "Visualization by hdydenairn.github.io"
  )

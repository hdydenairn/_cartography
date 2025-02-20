# Install necessary packages if needed
# install.packages(c("eurostat", "dplyr", "ggplot2", "sf", "tidyverse", "showtext", "rnaturalearth", "rnaturalearthdata"))

# Load libraries
library(eurostat)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyverse)
library(showtext)
library(rnaturalearth)
library(rnaturalearthdata)

cars <- search_eurostat("cars")

# Load required libraries
library(tidyverse)
library(eurostat)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggplot2)
library(showtext)

# Get data on car ownership per thousand inhabitants
road_eqs_carhab <- get_eurostat("road_eqs_carhab", time_format = "num")

# Filter data for the 2022
data_filtered <- road_eqs_carhab %>%
  filter(TIME_PERIOD == 2022) %>%
  select(geo, values)

# Convert 'values' to numeric and handle any issues
data_filtered$values <- as.numeric(data_filtered$values)

# Check for missing values
sum(is.na(data_filtered$values))  # Identify NAs

# Fix country codes before merging
data_filtered <- data_filtered %>%
  filter(geo != "EU27_2020") %>%  # Remove EU-wide data
  mutate(geo = case_when(
    geo == "UK"  ~ "GB",  # Fix UK code
    geo == "EL"  ~ "GR",  # Fix Greece code
    TRUE ~ geo
  ))

# Load Natural Earth countries shapefile
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Fix missing France entry
world_map <- world_map %>%
  mutate(iso_a2 = ifelse(iso_a2 == "-99", "FR", iso_a2))

# Filter for Europe, Africa, and Asia
europe_map_extended <- world_map %>%
  filter(continent %in% c("Europe", "Africa", "Asia")) %>%
  select(iso_a2, geometry)

# Ensure both datasets use the same key for merging
data_merged <- europe_map_extended %>%
  left_join(data_filtered, by = c("iso_a2" = "geo"))

# Handle missing values by keeping NAs for visualization
data_merged$values[is.na(data_merged$values)] <- NA  

# Add custom font
font_add_google("Lato", "econ")
showtext_auto()

# Plot - Overlay map and data visualization
ggplot(data_merged) +
  geom_sf(aes(fill = values), color = "white") +
  scale_fill_viridis_c(
    name = "Cars per 1000", option = "magma", na.value = "grey"
  ) +  
  coord_sf(xlim = c(-25, 52), ylim = c(35, 72)) +  
  theme(
    plot.title = element_text(hjust = 0.5, family = "econ", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, family = "econ"),
    plot.caption = element_text(hjust = 0.5, family = "econ", face = "italic"),
    legend.title = element_text(family = "econ"),
    legend.text = element_text(family = "econ"),
    panel.background = element_rect(fill = "lightgrey"),
    plot.background = element_rect(fill = "lightgrey"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = c(0.85, 0.5),  # Moves the legend higher
    legend.box = "horizontal"
  ) +
  labs(
    title = paste("Car Ownership per 1000 People in Europe (", 2022, ")", sep=""),
    subtitle = "Data source: Eurostat",
    caption = "Visualization by hdydenairn.github.io"
  )
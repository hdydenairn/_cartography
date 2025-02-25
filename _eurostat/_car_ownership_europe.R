# Install necessary packages if needed
# install.packages(c("eurostat", "dplyr", "ggplot2", "sf", "tidyverse", "showtext", "rnaturalearth", "rnaturalearthdata"))

# Load required libraries
library(tidyverse)
library(eurostat)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggplot2)
library(showtext)

cars <- search_eurostat("cars")

# Get data on car ownership per thousand inhabitants
road_eqs_carhab <- get_eurostat("road_eqs_carhab", time_format = "num")

# Car Ownership 2022 ####

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

# Plot
ggplot(data_merged) +
  geom_sf(aes(fill = values), color = "white") +
  scale_fill_viridis_c(
    name = "Cars per 1000", option = "magma", na.value = "grey", direction = -1,
    limits = c(150, 850),  # Extend the legend range
    breaks = seq(150, 850, by = 100)  # Set tick marks at intervals
  ) +  
  coord_sf(xlim = c(-25, 52), ylim = c(35, 72)) +  
  theme(
    plot.title = element_text(hjust = 0.5, family = "econ", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, family = "econ"),
    plot.caption = element_text(hjust = 0.5, family = "econ", face = "italic"),
    legend.title = element_text(family = "econ"),
    legend.text = element_text(family = "econ"),
    legend.ticks = element_line(size = 0.5),  # Adjust legend tick appearance
    legend.ticks.length = unit(0.3, "cm"),  # Extend tick length slightly
    legend.axis.line = element_line(size = 0.6, color = "black"),  # Add axis line to legend
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


# Car Ownership Change 2017 - 2022 ####

# Filter data for 2017 and 2022
data_filtered <- road_eqs_carhab %>%
  filter(TIME_PERIOD %in% c(2017, 2022)) %>%
  select(geo, TIME_PERIOD, values)

# Convert 'values' to numeric
data_filtered$values <- as.numeric(data_filtered$values)

# Transform data for easier calculation
data_wide <- data_filtered %>%
  pivot_wider(names_from = TIME_PERIOD, values_from = values, names_prefix = "year_")

# Calculate percentage change
data_wide <- data_wide %>%
  mutate(pct_change = ((year_2022 - year_2017) / year_2017) * 100)

# Fix country codes
data_wide <- data_wide %>%
  filter(geo != "EU27_2020") %>%
  mutate(geo = case_when(
    geo == "UK"  ~ "GB",
    geo == "EL"  ~ "GR",
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

# Merge map with data
data_merged <- europe_map_extended %>%
  left_join(data_wide, by = c("iso_a2" = "geo"))

# Add custom font
font_add_google("Lato", "econ")
showtext_auto()

# Plot the percentage change
ggplot(data_merged) +
  geom_sf(aes(fill = pct_change), color = "white") +
  scale_fill_gradient2(
    name = "% Change", low = "red", mid = "white", high = "blue", 
    midpoint = 0, na.value = "grey"
  ) +
  coord_sf(xlim = c(-25, 52), ylim = c(35, 72)) +
  theme(
    plot.title = element_text(hjust = 0.5, family = "econ", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, family = "econ"),
    plot.caption = element_text(hjust = 0.5, family = "econ", face = "italic"),
    legend.title = element_text(family = "econ"),
    legend.text = element_text(family = "econ"),
    legend.position = c(0.85, 0.5),
    panel.background = element_rect(fill = "lightgrey"),
    plot.background = element_rect(fill = "lightgrey"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  labs(
    title = "Percentage Change in Car Ownership (2017-2022)",
    subtitle = "Data source: Eurostat",
    caption = "Visualization by hdydenairn.github.io"
  )

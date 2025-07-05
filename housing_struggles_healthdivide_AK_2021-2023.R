#Skylar Pae
#GMU
#Severe Housing Stressors & Health Divides in Alaska
#By Geographic Distribution
#22 June 2025

#Part 1:

#To highlight the distribution of severe housing issues across Alaska
#from 2021-2023, caused both by both climate and socioeconomic environment.

#Part 2:

#To spotlight how these severe housing issues caused by 
#environmental factors & socioeconomic stressors impact the population
#through preventable fatalities.

#These visuals are to showcase the importance of intervention for 
#substance abuse, and to encourage allocating funding
#for prevention programs.


# ALL Libraries Utilized for Project
library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(ggrepel)
library(ggspatial)
library(ggExtra)


#Housing Data: "Severe Housing Problems". Data USA - Alaska. https://datausa.io/profile/geo/alaska?alcoholDrugsTobaccoOptions=indicator_10&degree-majors=degree3&dietAndExerciseOptions=indicator_5&healthBehaviorsOptions=alcoholDrugsTobaccoOption&pums5RacesResident=pums5Race1#housing. Accessed 22 June 2025.
#OD, Excessive Drinking, & DWI Data: "Health Behaviors". Data USA - Alaska. https://datausa.io/profile/geo/alaska?alcoholDrugsTobaccoOptions=indicator_8&degree-majors=degree3&dietAndExerciseOptions=indicator_5&healthBehaviorsOptions=alcoholDrugsTobaccoOption&healthOutcomesOptions=qualityOfLife. Accessed 22 June 2025.

#US Boundaries: US Census Bureau. (2022). 2022 TIGER/Line Shapefiles: Public Use Microdata Areas, https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2022&layergroup=Public+Use+Microdata+Areas. Accessed 22 June 2025.


# IMPORT
housing_df <- read_csv("C:/Users/paesk/OneDrive/Documents/GMU/Housing Stressors in Alaska by Year_RStudio/Severe Housing Problems.csv")
dwi_df <- read_csv("C:/Users/paesk/OneDrive/Documents/GMU/Housing Stressors in Alaska by Year_RStudio/Health Behaviors_DWI.csv")
od_df <- read_csv("C:/Users/paesk/OneDrive/Documents/GMU/Housing Stressors in Alaska by Year_RStudio/Health Behaviors_DrugOD.csv")
binge_drink_df <- read_csv("C:/Users/paesk/OneDrive/Documents/GMU/Housing Stressors in Alaska by Year_RStudio/Health Behaviors_ExcessDrink.csv")

# IMPORT For Mapping
# NOTE: Be sure to have all ShapeFiles in working directory (.cpg, .dbf, .prj, .shx, .shp)
all_us_boundaries <- st_read("C:/Users/paesk/OneDrive/Documents/GMU/Housing Stressors in Alaska by Year_RStudio/tl_2022_us_county.shp")


# CREATE MAIN DF

years <- c(2021, 2022, 2023)

# Step 1 - Make function to clean DFs
processing_df <- function(df, agg_col, new_col) {
  df %>%
    filter(`ID State` == "04000US02") %>%
    mutate(id_county_char = as.character(`ID County`)) %>%
    mutate(county_ID_3digit = substr(id_county_char, 10, 12)) %>%
    mutate(state_county_ID_5digit = paste0("02", county_ID_3digit)) %>%
    filter(Year %in% years) %>%
    select(state_county_ID_5digit, Year, {{agg_col}}) %>%
    # Group by the 5-digit FIPS and Year for unique rows
    group_by(state_county_ID_5digit, Year) %>%
    summarise(
      !!sym(new_col) := mean({{agg_col}}, na.tm = TRUE),
      .groups = 'drop'
    ) %>%
    ungroup()
}

# Step 2 - Clean & Filter DFs
# Percent of Population per County experiencing Housing Problems
housing_df_clean <- processing_df(housing_df, `Severe Housing Problems`, "pct_pop_housingissues")

# Total DWI Fatalies / Year
dwi_df_clean <- processing_df(dwi_df, `Alcohol-Impaired Driving Deaths`, "pct_driving_dths_DWI")

# Total ODs / Year
od_df_clean <- processing_df(od_df, `Drug Overdose Deaths`, "total_od_cases")

# Total % of Pop experiencing Excessive Drinking/Alcohol Addiction
binge_drink_df_clean <- processing_df(binge_drink_df, `Excessive Drinking`, "pct_pop_alc_addict") # Replace


# Step 3 - Merge DFs
main_ak <- housing_df_clean %>%
  left_join(dwi_df_clean, by = c("state_county_ID_5digit", "Year")) %>%
  left_join(od_df_clean, by = c("state_county_ID_5digit", "Year")) %>%
  left_join(binge_drink_df_clean, by = c("state_county_ID_5digit", "Year"))

# Step 4 - Add Names from PUMA ID
ak_sf_geo <- all_us_boundaries %>%
  filter(STATEFP == "02") %>% # AK PUMA ID
  select(GEOID, NAME, geometry) #Columns to keep from all us boundaries

ak_main_df <- ak_sf_geo %>%
  left_join(main_ak, by = c("GEOID" = "state_county_ID_5digit")) %>%
  filter(Year %in% years | is.na(Year))


# VISUAL 1 - MAPPING % OF POP EXPERIENCING HOUSING ISSUES ACROSS AK

# Categorize Housing Issue Levels (Based on US Thresholds)
ak_cat_main_df <- ak_main_df %>%
  mutate(
    housing_stress_category = case_when(
      pct_pop_housingissues < 0.10 ~ "Low Housing Issues",
      pct_pop_housingissues >= 0.10 & pct_pop_housingissues < 0.20 ~ "Medium Housing Issues",
      pct_pop_housingissues >= 0.20 ~ "High Housing Issues",
      TRUE ~ NA_character_ 
    ),
    housing_stress_category = factor(housing_stress_category, levels = c("Low Housing Issues", "Medium Housing Issues", "High Housing Issues"))
  )

alaska_albers_crs <- st_crs(3338) # Force EPSG:3338 as is Alaska Albers Equal Area Conic

ak_main_df_projected <- ak_cat_main_df %>%
  st_transform(alaska_albers_crs)

# Color Palette for Boroughs
mainmap_colors <- c(
  "Low Housing Issues" = "lightgoldenrod1",
  "Medium Housing Issues" = "skyblue1",
  "High Housing Issues" = "darkorchid4"
)

# 2021 HOUSING Mapping
year_2021 <- 2021

map_data_2021_housing_only <- ak_main_df_projected %>%
  filter(Year == year_2021 | is.na(Year))

ak_map_2021 <- ggplot(map_data_2021_housing_only, aes(fill = housing_stress_category, geometry = geometry)) +
  geom_sf(color = "white", linewidth = 0.2) +
  scale_fill_manual(values = mainmap_colors, name = "Housing Issues Category") +
  #scale_fill_viridis_d(option = "plasma", direction = -1,
                       #na.value = "grey70",
                       #name = "Housing Issues Category",
                       #drop = FALSE) +
  # Label Defined Categories
  geom_text_repel(data = map_data_2021_housing_only %>% filter(!is.na(housing_stress_category)),
                  aes(label = NAME, geometry = geometry),
                  stat = "sf_coordinates",
                  size = 2.5, force = 2, max.overlaps = Inf,
                  min.segment.length = 0, bg.color = "white", bg.r = 0.05) +
  labs(
    title = paste0("Geographic Distribution of Housing Struggles in Alaska (", year_2021, ")"),
    subtitle = "Categorized by Percentage of Population Experiencing Housing Struggles",
    caption = "Data Source: Data USA - Alaska"
  ) +
  theme_minimal() +
  ggspatial::annotation_scale(location = "bl", bar_cols = c("grey60", "white"), text_family = "Arial") +
  ggspatial::annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering, height = unit(1.5, "cm"), width = unit(1.5, "cm")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    panel.grid.major = element_line(color = "transparent"),
    panel.grid.minor = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.caption = element_text(hjust = 0)
  ) +
  coord_sf(crs = alaska_albers_crs, expand = FALSE, datum = NA)

print(ak_map_2021)

ggsave(
  "Housing Struggles in Alaska 2021.png",
  plot = ak_map_2021,
  width = 12,
  height = 8,
  dpi = 300,
  bg='white'
)

# 2022 HOUSING Mapping
year_2022 <- 2022

map_data_2022_housing_only <- ak_main_df_projected %>%
  filter(Year == year_2022 | is.na(Year))

ak_map_2022 <- ggplot(map_data_2022_housing_only, aes(fill = housing_stress_category, geometry = geometry)) +
  geom_sf(color = "white", linewidth = 0.2) +
  scale_fill_manual(values = mainmap_colors, name = "Housing Issues Category") +
  #scale_fill_viridis_d(option = "viridis", direction = -1,
                       #na.value = "grey70",
                       #name = "Housing Issues Category",
                       #drop = FALSE) +
  # Label Defined Categories
  geom_text_repel(data = map_data_2022_housing_only %>% filter(!is.na(housing_stress_category)),
                  aes(label = NAME, geometry = geometry),
                  stat = "sf_coordinates",
                  size = 2.5, force = 2, max.overlaps = Inf,
                  min.segment.length = 0, bg.color = "white", bg.r = 0.05) +
  labs(
    title = paste0("Geographic Distribution of Housing Struggles in Alaska (", year_2022, ")"),
    subtitle = "Categorized by Percentage of Population Experiencing Housing Struggles",
    caption = "Data Source: Data USA - Alaska"
  ) +
  theme_minimal() +
  ggspatial::annotation_scale(location = "bl", bar_cols = c("grey60", "white"), text_family = "Arial") +
  ggspatial::annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering, height = unit(1.5, "cm"), width = unit(1.5, "cm")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    panel.grid.major = element_line(color = "transparent"),
    panel.grid.minor = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.caption = element_text(hjust = 0)
  ) +
  coord_sf(crs = alaska_albers_crs, expand = FALSE, datum = NA)

print(ak_map_2022)

ggsave(
  "Housing Struggles in Alaska 2022_Updated Colormap.png",
  plot = ak_map_2022,
  width = 12,
  height = 8,
  dpi = 300,
  bg='white'
)

# 2023 HOUSING Mapping
year_2023 <- 2023

map_data_2023_housing_only <- ak_main_df_projected %>%
  filter(Year == year_2023 | is.na(Year))

ak_map_2023 <- ggplot(map_data_2023_housing_only, aes(fill = housing_stress_category, geometry = geometry)) +
  geom_sf(color = "white", linewidth = 0.2) +
  scale_fill_manual(values = mainmap_colors, name = "Housing Issues Category") +
  #scale_fill_viridis_d(option = "viridis", direction = -1,
                       #na.value = "grey70",
                       #name = "Housing Issues Category",
                       #drop = FALSE) +
  # Label Defined Categories
  geom_text_repel(data = map_data_2023_housing_only %>% filter(!is.na(housing_stress_category)),
                  aes(label = NAME, geometry = geometry),
                  stat = "sf_coordinates",
                  size = 2.5, force = 2, max.overlaps = Inf,
                  min.segment.length = 0, bg.color = "white", bg.r = 0.05) +
  labs(
    title = paste0("Geographic Distribution of Housing Struggles in Alaska (", year_2023, ")"),
    subtitle = "Categorized by Percentage of Population Experiencing Housing Struggles",
    caption = "Data Source: Data USA - Alaska"
  ) +
  theme_minimal() +
  ggspatial::annotation_scale(location = "bl", bar_cols = c("grey60", "white"), text_family = "Arial") +
  ggspatial::annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering, height = unit(1.5, "cm"), width = unit(1.5, "cm")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    panel.grid.major = element_line(color = "transparent"),
    panel.grid.minor = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.caption = element_text(hjust = 0)
  ) +
  coord_sf(crs = alaska_albers_crs, expand = FALSE, datum = NA)

print(ak_map_2023)

ggsave(
  "Housing Struggles in Alaska 2023.png",
  plot = ak_map_2023,
  width = 12,
  height = 8,
  dpi = 300,
  bg='white'
)

# VISUAL 2 - HOUSING HEALTH DIVERGENCE ACROSS BOROUGHS

# Color Palette for Boroughs
custom_fill_colors <- c(
  "Low Housing Issues" = "chartreuse",
  "Medium Housing Issues" = "gold",
  "High Housing Issues" = "darkorange1"
)

# 2021 DIVERGENCE
map_2021_all_data <- ak_main_df_projected %>%
  filter(Year == year_2021) %>%
  filter(!is.na(pct_pop_housingissues) & !is.na(pct_driving_dths_DWI) & !is.na(total_od_cases) & !is.na(pct_pop_alc_addict)) %>%
  filter(!is.na(housing_stress_category))

ak_map_2021_divergence <- ggplot(map_2021_all_data, aes(fill = housing_stress_category, geometry = geometry)) +
  geom_sf(color = "white", linewidth = 0.2) +
  scale_fill_manual(values = custom_fill_colors, name = "Housing Issues Category") +
  #scale_fill_viridis_d(option = "plasma", direction = -1, name = "Housing Issues Category") + # Can use a different color scale here
  
  # Proportions
  geom_point(aes(geometry = geometry, size = total_od_cases, color = pct_driving_dths_DWI),
             stat = "sf_coordinates", alpha = 0.7) +
  scale_size_area(max_size = 12, name = "Total Drug Overdose Deaths") +
  scale_color_gradient(low = "darkblue", high = "firebrick", name = "% Driving Fatalities involving DWI") +
  #scale_color_viridis_c(option = "cividis", name = "% Driving Fatalities involving DWI") +
  
  geom_text_repel(data = map_2021_all_data %>% filter(total_od_cases > quantile(total_od_cases, 0.75, na.rm=TRUE)), # Label top 25% of OD cases
                  aes(label = NAME, geometry = geometry),
                  stat = "sf_coordinates",
                  size = 2.5, force = 2, max.overlaps = Inf,
                  min.segment.length = 0, bg.color = "white", bg.r = 0.05) +
  
  labs(
    title = paste0("Housing-Health Divide Across Alaska Boroughs (", year_2021, ")"),
    subtitle = "Intersections of Housing Stress, Drug Overdose Counts, and DWI Fatality Counts",
    caption = "Data: Data USA - Alaska"
  ) +
  theme_minimal() +
  ggspatial::annotation_scale(location = "bl", bar_cols = c("grey60", "white")) +
  ggspatial::annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    panel.grid.major = element_line(color = "transparent"),
    panel.grid.minor = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.caption = element_text(hjust = 0)
  ) +
  coord_sf(crs = alaska_albers_crs, expand = FALSE, datum = NA)

print(ak_map_2021_divergence)

ggsave(
  "Housing-Health Divide in Alaska 2021.png",
  plot = ak_map_2021_divergence,
  width = 12,
  height = 8,
  dpi = 300,
  bg='white'
)

# 2022 DIVERGENCE
map_2022_all_data <- ak_main_df_projected %>%
  filter(Year == year_2022) %>%
  filter(!is.na(pct_pop_housingissues) & !is.na(pct_driving_dths_DWI) & !is.na(total_od_cases) & !is.na(pct_pop_alc_addict)) %>%
  filter(!is.na(housing_stress_category))

ak_map_2022_divergence <- ggplot(map_2022_all_data, aes(fill = housing_stress_category, geometry = geometry)) +
  geom_sf(color = "white", linewidth = 0.2) +
  scale_fill_manual(values = custom_fill_colors, name = "Housing Issues Category") +
  #scale_fill_viridis_d(option = "plasma", direction = -1, name = "Housing Issues Category") + # Can use a different color scale here
  
  # Proportions
  geom_point(aes(geometry = geometry, size = total_od_cases, color = pct_driving_dths_DWI),
             stat = "sf_coordinates", alpha = 0.7) +
  scale_size_area(max_size = 12, name = "Total Drug Overdose Deaths") +
  scale_color_gradient(low = "darkblue", high = "firebrick", name = "% Driving Fatalities involving DWI") +
  #scale_color_viridis_c(option = "cividis", name = "% Driving Fatalities involving DWI") +
  
  geom_text_repel(data = map_2022_all_data %>% filter(total_od_cases > quantile(total_od_cases, 0.75, na.rm=TRUE)), # Label top 25% of OD cases
                  aes(label = NAME, geometry = geometry),
                  stat = "sf_coordinates",
                  size = 2.5, force = 2, max.overlaps = Inf,
                  min.segment.length = 0, bg.color = "white", bg.r = 0.05) +
  
  labs(
    title = paste0("Housing-Health Divide Across Alaska Boroughs (", year_2022, ")"),
    subtitle = "Intersections of Housing Stress, Drug Overdose Counts, and DWI Fataly Counts",
    caption = "Data: Data USA - Alaska"
  ) +
  theme_minimal() +
  ggspatial::annotation_scale(location = "bl", bar_cols = c("grey60", "white")) +
  ggspatial::annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    panel.grid.major = element_line(color = "transparent"),
    panel.grid.minor = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.caption = element_text(hjust = 0)
  ) +
  coord_sf(crs = alaska_albers_crs, expand = FALSE, datum = NA)

print(ak_map_2022_divergence)

ggsave(
  "Housing-Health Divide in Alaska 2022.png",
  plot = ak_map_2022_divergence,
  width = 12,
  height = 8,
  dpi = 300,
  bg='white'
)

# 2023 DIVERGENCE
map_2023_all_data <- ak_main_df_projected %>%
  filter(Year == year_2023) %>%
  filter(!is.na(pct_pop_housingissues) & !is.na(pct_driving_dths_DWI) & !is.na(total_od_cases) & !is.na(pct_pop_alc_addict)) %>%
  filter(!is.na(housing_stress_category))

ak_map_2023_divergence <- ggplot(map_2023_all_data, aes(fill = housing_stress_category, geometry = geometry)) +
  geom_sf(color = "white", linewidth = 0.2) +
  scale_fill_manual(values = custom_fill_colors, name = "Housing Issues Category") +
  #scale_fill_viridis_d(option = "viridis", direction=-1, name = "Housing Issues Category") + # Can use a different color scale here
  
  # Proportions
  geom_point(aes(geometry = geometry, size = total_od_cases, color = pct_driving_dths_DWI),
             stat = "sf_coordinates", alpha = 0.7) +
  scale_size_area(max_size = 12, name = "Total Drug Overdose Deaths") +
  scale_color_gradient(low = "darkblue", high = "firebrick", name = "% Driving Fatalities involving DWI") +
  #scale_color_viridis_c(option = "inferno", direction=-1, name = "% Driving Fatalities involving DWI") +
  
  geom_text_repel(data = map_2023_all_data %>% filter(total_od_cases > quantile(total_od_cases, 0.75, na.rm=TRUE)), # Label top 25% of OD cases
                  aes(label = NAME, geometry = geometry),
                  stat = "sf_coordinates",
                  size = 2.5, force = 2, max.overlaps = Inf,
                  min.segment.length = 0, bg.color = "white", bg.r = 0.05) +
  
  labs(
    title = paste0("Housing-Health Divide Across Alaska Boroughs (", year_2023, ")"),
    subtitle = "Intersections of Housing Issues, Drug Overdose Counts, and DWI Fatality Counts",
    caption = "Data: Data USA - Alaska"
  ) +
  theme_minimal() +
  ggspatial::annotation_scale(location = "bl", bar_cols = c("grey60", "white")) +
  ggspatial::annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    panel.grid.major = element_line(color = "transparent"),
    panel.grid.minor = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.caption = element_text(hjust = 0)
  ) +
  coord_sf(crs = alaska_albers_crs, expand = FALSE, datum = NA)

print(ak_map_2023_divergence)

ggsave(
  "Housing-Health Divide in Alaska 2023.png",
  plot = ak_map_2023_divergence,
  width = 12,
  height = 8,
  dpi = 300,
  bg='white'
)

# ALL WORKS CITED:

#"Severe Housing Problems". Data USA: Alaska. https://datausa.io/profile/geo/alaska?alcoholDrugsTobaccoOptions=indicator_10&degree-majors=degree3&dietAndExerciseOptions=indicator_5&healthBehaviorsOptions=alcoholDrugsTobaccoOption&pums5RacesResident=pums5Race1#housing. Accessed 22 June 2025.
#"Health Behaviors". Data USA - Alaska. https://datausa.io/profile/geo/alaska?alcoholDrugsTobaccoOptions=indicator_8&degree-majors=degree3&dietAndExerciseOptions=indicator_5&healthBehaviorsOptions=alcoholDrugsTobaccoOption&healthOutcomesOptions=qualityOfLife. Accessed 22 June 2025.
#US Census Bureau. (2022). 2022 TIGER/Line Shapefiles: Public Use Microdata Areas, https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2022&layergroup=Public+Use+Microdata+Areas. Accessed 22 June 2025.
#County Health Rankings & Roadmaps. (2022). "Severe Housing Problems", https://www.countyhealthrankings.org/health-data/community-conditions/physical-environment/housing-and-transportation/severe-housing-problems?year=2025. Accessed 22 June 2025
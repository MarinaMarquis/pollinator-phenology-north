#Quantifying flight period 
#Marina Marquis 


############################################################################################################

# Load Packages 
library(dplyr)
library(sf)
library(lubridate)
library(purrr)


# Read in data 
pollinators_grids <- readRDS("Data/pollinators_joined_with_grids_5.rds") #joined grid pollinators data
five_km_grids <- st_read("Data/Spatial Data/gridded map of NA24 region/NA24_gridded_map.geojson")


############################################################################################################


# Drop the geometry so R doesn't freak out 
pollinators_grids1 <- pollinators_grids %>%
  st_drop_geometry()

# Checking for species rows with no species names 
sum(is.na(pollinators_grids1$species))

# Found quite a few, looking at them more closely: 
pollinators_grids1 %>% 
  filter(is.na(species))

# These observations have genus but not species. I will remove them. 
pollinators_grids_clean <- pollinators_grids1 %>%
  filter(!is.na(species))

#Making sure it worked
sum(is.na(pollinators_grids_clean$species))


# Filter: only include grids with >= 3 species and at least 10 observations of each species 
filtered_5 <- pollinators_grids_clean %>%
  group_by(grid_id, species) %>%
  summarize(n = n(), .groups = 'drop') %>%
  filter(n >= 10) %>%
  group_by(grid_id) %>%
  filter(n_distinct(species) >= 3) %>%
  ungroup() %>%
  inner_join(pollinators_grids1, by = c("grid_id", "species"))

# Quick fact check 
#>= 3 species 
species_count_check <- filtered_5 %>%
  group_by(grid_id) %>%
  summarize(unique_species_count = n_distinct(species), .groups = 'drop')
#
if (all(species_count_check$unique_species_count >= 3)) {
  cat("All grids have at least 3 species.\n")
} else {
  cat("Some grids do not have at least 3 species.\n")
}

#>= 10 observations
observation_count_check <- filtered_5 %>%
  group_by(grid_id, species) %>%
  summarize(observation_count = n(), .groups = 'drop')
#
if (all(observation_count_check$observation_count >= 10)) {
  cat("All species have at least 10 observations.\n")
} else {
  cat("Some species do not have at least 10 observations.\n")
}


#All grids have at least 3 species and at least 10 observations. 

# Let's also make sure the date column is formatted correctly for analysis 
filtered_5$Date <- as.Date(filtered_5$eventDate)

# Look at the taxonomic groups present 
unique(filtered_5$genus)
unique(filtered_5$family)
unique(filtered_5$order)

# How many grids originally versus after filtering:

#Check
length(unique(five_km_grids$grid_id))  #24128 grids spanning eco-region NA24 (no data attached)
length(unique(pollinators_grids$grid_id)) #19218 grids that have observations in them before filtering
length(unique(filtered_5$grid_id)) #464 grids with observations after filtering


# Export the rds file 
saveRDS(filtered_5, "Data/filtered_5.rds")

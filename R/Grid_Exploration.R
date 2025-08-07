### Pollinator Grid Exploration
### Marina Marquis

########################################################################################################### 

# Load packages
library(tidyverse)
library(sf)
library(ggplot2)

# Read in data  
filtered_5 <- readRDS("Data/filtered_5.rds") # joined grid and pollinators data
grids_5 <- st_read("Data/Spatial Data/gridded map of NA24 region/NA24_gridded_map.geojson") #gridded map 
NA_24 <- st_read("Data/Spatial Data/ecoregion geojson/NA_24_clipped.geojson") #map of region (no grids)

########################################################################################################### 


# How many species? 1231
length(unique(filtered_5$species))

# How many species per grid cell?
species_num <- filtered_5 %>%
  group_by (grid_id)%>%
  summarise(species_n = n_distinct(species))
species_num

# Average species per grid cell: 28.95105
mean(species_num$species_n)


# Max/min species in a single grid cell: 
max(species_num$species_n) #367
min(species_num$species_n) #3

# Number of grids: 286
length(unique(species_num$grid_id))

# Histogram of number of species per grid cell 
ggplot(species_num, aes(x=species_n))+
  geom_histogram(fill="gray80", color="black")+
  labs(x="Number of Species per Grid Cell")

# Histogram of number of observations per grid cell 
obs_per_grid <- filtered_5 %>%
  count(grid_id, name = "obs_n")
obs_per_grid

# Plot the histogram
ggplot(obs_per_grid, aes(x = obs_n)) +
  geom_histogram(fill = "darkseagreen3", color = "black") +
  labs(x = "Number of Observations per Grid Cell", 
       y = "Frequency") +
  theme_classic()

ggsave("Figures/Number_Observations_per_grid_histogram.png", width=5, height=5, units="in")

# How many grid cells per species?  
grid_per_spec <- filtered_5 %>%
  group_by (species)%>%
  summarise(grid_per_spec = n_distinct(grid_id))
grid_per_spec

# Summarize this in a table 

# Empty df
df <- data.frame(
  Average_Species_Per_Cell = numeric(),  
  Most_Species_In_One_Cell = numeric(),
  Species_Found_In_Most_Cells=character()) 

# Row 
row <- data.frame(Average_Species_Per_Grid_Cell = 28.95105,  
                  Most_Species_In_One_Cell = 367, Species_Found_In_Most_Cells="Bombus impatiens")

# Combine them
df <- rbind(df, row)
df  
write.csv(df, "summary_table.csv", row.names = FALSE)  


########################################################################################################### 
### Map Data

# Join data with grids, then make it an sf object
map_grids <- species_num %>%
  left_join(., grids_5, by="grid_id") %>%  #grids_5 is an st 
  st_as_sf() #turn st into sf 


# Plot it 
ggplot()+
  geom_sf(data=map_grids, aes(fill=log10(species_n)))+ #add grids 
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) + 
  theme_bw()+
  labs(x="Number of Species per Grid Cell")

# Save as png
ggsave("Figures/number_species_per_grid.png", width=5, height=6, units="in")



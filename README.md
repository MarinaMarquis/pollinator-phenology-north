###### The scripts for the pollinator phenology should be run/read in the order detailed below. 


###### 1. create_grid_shapefiles_of_northeast.R
#         Input = one_earth-bioregions-2023.geojson
#         Output = 
#                 a) NA24_gridded_map.geojson 
#                 b) NA_24_clipped.geojson


###### 2. join_inaturalist_pollinators_with_grids.R
#         Input = 
#                 a) NA24_gridded_map.geojson
#                 b) iNaturalist_pollinator_observations.rds
#         Output = pollinators_joined_with_grids_5.rds


###### 3. Filtering Data for Phenological Estimates.R 
#         Input = 
#                 a) pollinators_joined_with_grids_5.rds
#                 b) NA24_gridded_map.geojson
#         Output = filtered_5.rds


###### 4. Grid_Exploration.R 
#         Input = filtered_5.rds
#                 a) pollinators_joined_with_grids_5.rds
#                 b) NA24_gridded_map.geojson
#                 c) NA_24_clipped.geojson
#         Output = 
#                 a) Number_Observations_per_grid_histogram.png
#                 b) summary_table.csv
#                 c) number_species_per_grid.png


###### 5. Phenological_Estimates_by_grid_by_species.R 
#         Input = filtered_5.csv
#         Output = phenology_estimates_by_grid_by_species.RDS


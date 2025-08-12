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


###### 6. link_GHMI_to_grids.R
#         Input = 
#                 a) mean_gHM.csv
#                 b) filtered_5.rds
#         Output = filtered_5_with_GHMI.csv


###### 7. prepare_data_for_analysis.R
#         Input = 
#                 a) filtered_5_with_GHMI.csv
#                 b) phenology_estimates_by_grid_by_species.RDS
#                 c) iNaturalist_pollinator_observations.rds
#         Output = phenology_estimates_data_for_analysis.rds


###### 8. Empirical_Data_Figures.R
#         Input =
#                a)  filtered_5.csv
#                b)  filtered_5_with_GHMI.csv
#                c)  NA24_gridded_map.geojson 
#                d)  NA_24_clipped.geojson
#                e)  mean_gHM.csv
#                f)  phenology_estimates_data_for_analysis.rds
#         Output = 
#                 a) frequency_Dione_vanillae_observations_over_time_grid_4145.png 
#                 b) frequency_Apis_mellifera_observations_over_time_grid_4145.png
#                 c) frequency_Danaus_plexippus_observations_over_time_grid_4145.png
#                 d) frequency_Erynnis_horatius_observations_over_time_grid_4145.png
#                 e) frequency_Apis_mellifera_observations_over_time_grid_7.png
#                 f) frequency_all_species_observations_over_time_grid_4145.png
#                 g) frequency_Apis_mellifera_observations_over_time_all_grids.png
#                 h) observation_frequency_over_time_by_family.png
#                 i) observation_frequency_over_time_by_order.png
#                 j) observations_with_landsat_variables.rds
#                 k) Lepodoptera_Observations_in_Low_and_High_GHMI.png
#                 l) Dione_vanillae_Observations_in_Low_and_High_GHMI.png
#                 m) Dione_vanillae_Observations_in_Low_and_High_GHMI_two_figures.png
#                 n) map_of_species_per_grid_cell.png
#                 o) map_of_observations_per_grid_cell.png
#                 p) GHMI_map_of_Florida.png
#                 q) Dione_vanillae_observations_across_grids.png
#                 r) Apis_mellifera_observations_across_grids.png
#                 s) Danaus_plexippus_observations_across_grids.png
#                 t) Anartia_jatrophae_observations_across_grids


###### 9. Phenology Figures.R
#         Input = phenology_estimates_data_for_analysis.rds
#         Output = 
#                 a) phenology_estimates_all_species_each_grid_with_landsat
#                 b) phenology_estimates_example_for_grid_390.png
#                 c) phenology_estimates_all_species_across_all_grids.png
#                 d) phenology_estimates_all_species_across_all_grids_separate_graphs.png
#                 e) phenology_estimates_Apis_mellifera_all_grids.png
#                 f) phenology_estimates_Automeris_io_all_grids.png
#                 g) Automeris_io_medians_Fl_map.png
#                 h) Dione_vanillae_medians_Fl_map.png
#                 i) Lepidoptera_medians_Fl_map.png
#                 j) Hymenoptera_medians_Fl_map.png
#                 k) Diptera_medians_Fl_map.png
#                 l) Coleoptera_medians_Fl_map.png
#                 m) medians_Fl_map.png
#                 n) mean_median_offset_duration_in_low_versus_high_ghmi.png
#                 o) total_duration_low_versus_high_urban_for_10_random_species.png
#                 p) onset_low_versus_high_urban_for_10_random_species.png
#                 q) onset_median_offset_in_low_versus_high_urban.png
#                 r) total_duration_in_low_versus_high_urban_for_10_random_leps.png
#                 s) total_duration_in_low_versus_high_urban_for_functional_groups_10_species.png
#                 t) total_duration_in_low_versus_high_urban_for_10_pre-selected_species.png
#                 u) onset_in_low_versus_high_urban_for_10_pre-selected_species.png
#                 v) offset_in_low_versus_high_urban_for_10_pre-selected_species.png 
#                 w) slope_of_species_duration_plot_20_random_species.png
#                 x) slope_of_ten_selected_species_duration_plot.png
#                 y) slope_of_ten_selected_species_onset_plot.png
#                 z) slope_of_ten_selected_species_offset_plot.png
#                 aa) slope_of_all_Lepidoptera_species_duration_plot.png
#                 bb) slope_of_species_duration_plot.png
#                 cc) slope_of_species_onset_plot.png
#                 dd) slope_of_species_offset_plot.png



###### 10. GAM_Analysis.R
#         Input = 
#                 a) phenology_estimates_data_for_analysis.rds
#                 b) phenology_estimates_by_grid_by_species.RDS
#                 c) iNaturalist_pollinator_observations.rds
#         Output = phenology_estimates_data_for_analysis.rds







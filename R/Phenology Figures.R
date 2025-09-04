###### Phenology-Specific Figures 
#### Marina Marquis
#### This script produces figures using the phenology data from the 
#   "Phenological_Estimates_by_grid_by_species.R" R script

#Packages: 
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(patchwork)
library(gridExtra)
library(grid)
library(broom)

phenology_estimates_all_species_each_grid_with_landsat <- readRDS("Data/phenology_estimates_data_for_analysis.rds")
grids_5 <- st_read("Data/Spatial Data/gridded map of NA24 region/NA24_gridded_map.geojson") #gridded map
NA_24 <- st_read("Data/Spatial Data/ecoregion geojson/NA_24_clipped.geojson") #map of bioregion NA24 (no grids)


##########################################################################################################################


############# Figure 1: looking at onset, offset, and duration for each species in a singular 
#grid cell 

# pick a grid cell
grid_ex <- phenology_estimates_all_species_each_grid_with_landsat %>%
  dplyr::filter(grid==8756) %>%
  dplyr::select(1, 3:6) %>%
  pivot_longer(!species, names_to="metric", values_to="day_of_year") %>%
  dplyr::filter(metric != "median")

ggplot(grid_ex, aes(x=species, y=day_of_year, color=metric))+
  geom_point()+
  coord_flip()+
  theme_bw()+
  ggtitle("Phenological estimates for all species in Grid #8756")

ggsave("Figures/phenology_estimates_example_for_grid_8756.png", width=6, height=6, units="in")


############# Figure 2: looking at onset, offset, and duration for each species in all 
#grid cells 

# Reshape the df to long format
phenology_long <- phenology_estimates_all_species_each_grid_with_landsat %>%
  pivot_longer(cols = c(onset, duration, offset), 
               names_to = "metric", 
               values_to = "day_of_year") %>%
  mutate(grid = factor(grid))
str(phenology_long)

# y-axis break intervals
unique_grids <- unique(phenology_long$grid)  
breaks <- unique_grids[seq(1, length(unique_grids), by = 10)]

# Plot it
ggplot(phenology_long, aes(x = day_of_year, y = factor(grid), color = metric)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(
    values = c("onset" = "blue", "duration" = "green", "offset" = "red"),
    labels = c("duration", "offset", "onset")
  ) +
  labs(
    title = "Onset, Offset, and Duration by Grid and Species",
    x = "Day of Year",
    y = "Grid ID",
    color = "Phenological Estimates"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(color = "gray80"),
    plot.margin = margin(10, 10, 10, 10) 
  )+
  scale_y_discrete(breaks = breaks) 

ggsave("Figures/phenology_estimates_all_species_across_all_grids.png", width=6, height=6, units="in")

#Note: grids move from south to north. So the larger the number, in general, the 
#more north the location.

 


############# Figure 3: looking at onset, offset, and duration for each species in all 
#grid cells, with each being its own graph  
ggplot(phenology_long, aes(x = day_of_year, y = factor(grid), color = metric)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(
    values = c("onset" = "blue", "duration" = "green", "offset" = "red"),
    labels = c("duration", "offset", "onset")
  ) +
  labs(
    title = "Onset, Offset, and Duration by Grid and Species",
    x = "Day of Year",
    y = "Grid ID",
    color = "Phenological Estimates"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(color = "gray80"),
    plot.margin = margin(10, 10, 10, 10) 
  )+
  scale_y_discrete(breaks = breaks) +
  facet_wrap(~ metric, scales = "free_x", nrow = 1) 


ggsave("Figures/phenology_estimates_all_species_across_all_grids_separate_graphs.png", width=6, height=6, units="in")




########### Figure 4: the same as figure 1, but for only Apis mellifera across all grids

#Apis mellifera 
species_name <- "Apis mellifera"  
#Only phenology columns (except for median)
metric_cols <- c("onset", "duration", "offset") 

# Apis melifera phenological estimates by grid 
phenology_species <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(species == species_name) %>%  
  select(grid, all_of(metric_cols)) %>%  # Keep only grid and metric columns
  pivot_longer(cols = all_of(metric_cols), names_to = "metric", values_to = "day_of_year")

# unique grid IDs for Apis melifera 
unique_grids <- unique(phenology_species$grid)

#intervals for the y-axis breaks: 10 
breaks <- unique_grids[seq(1, length(unique_grids), by = 10)]  

# Plot it
ggplot(phenology_species, aes(x = day_of_year, y = factor(grid), color = metric)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(
    values = c("onset" = "blue", "duration" = "green", "offset" = "red"),
    labels = c("Duration", "Offset", "Onset")
  ) +
  labs(
    title = paste("Onset, Offset, and Duration for", species_name, "by Grid"),
    x = "Day of Year",
    y = "Grid ID",
    color = "Phenological Estimates"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(color = "gray80"),
    plot.margin = margin(10, 10, 10, 10) 
  ) +
  scale_y_discrete(breaks = breaks)  # set breaks 

ggsave("Figures/phenology_estimates_Apis_mellifera_all_grids.png", width=6, height=6, units="in")



############## Figure 5: same thing but with Bombus impatiens

#set the name Bombus impatiens
Bombus_impatiens <- "Bombus impatiens"  

# Bombus impatiens phenological estimates by grid 
B_impatiens_estimates_by_grid <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(species == Bombus_impatiens) %>%  
  select(-species) %>%  # Remove the species column
  pivot_longer(cols = all_of(metric_cols), names_to = "metric", values_to = "day_of_year")


# Plot it
ggplot(B_impatiens_estimates_by_grid, aes(x = day_of_year, y = factor(grid), color = metric)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(
    values = c("onset" = "blue", "duration" = "green", "offset" = "red"),
    labels = c("Duration", "Offset", "Onset")
  ) +
  labs(
    title = paste("Onset, Offset, and Duration for", Bombus_impatiens, "by Grid"),
    x = "Day of Year",
    y = "Grid ID",
    color = "Phenological Estimates"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(color = "gray80"),
    plot.margin = margin(10, 10, 10, 10) 
  ) 

ggsave("Figures/phenology_estimates_Bombus_impatiens_all_grids.png", width=6, height=6, units="in")


############## Figure 6: showing offset of Bombus impatiens across mapped grids 


#want only offset for Bombus impatiens
B_impatiens_offset <- phenology_estimates_all_species_each_grid_with_landsat %>%
  select(species, grid, offset, mean_GHMI) %>%
  filter(species=="Bombus impatiens")
head(B_impatiens_offset) #check that it worked

#join data with grids, then make it an sf object
B_impatiens_offset_map <- B_impatiens_offset %>%
  left_join(., grids_5, by = c("grid" = "grid_id")) %>% #grids_5 is an st 
  st_as_sf() #turn st into sf 

# First plot  
p1 <- ggplot(B_impatiens_offset_map) +
  geom_sf(aes(fill = offset)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Median Flight Period") +
  theme_bw() +
  labs(title = "Offset of Flight Period of Bombus impatiens")


# Second plot 
p2 <- ggplot(B_impatiens_offset_map) +
  geom_sf(aes(fill = mean_GHMI)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Mean GHMI (Urbanization)") +
  theme_bw() +
  labs(title = "Urbanization (GHMI) Across Bioregion NA24")

# Combine the two maps side by side
final_plot <- p1 + p2 + plot_layout(ncol = 2)
final_plot

#save as png: 
ggsave("Figures/Bombus_impatiens_offset_Bioregion_NA24_map.png", width=6, height=6, units="in")




############## Figure 7: showing offset of Papilio glaucus across mapped grids with 
#comparison to GHMI

#want only offset for Papilio glaucus
P_glaucus_offset <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(species == "Papilio glaucus", !is.na(grid), !is.na(offset)) %>%
  select(species, grid, offset, mean_GHMI)
head(P_glaucus_offset) #check that it worked


#join data with grids, then make it an sf object
P_glaucus_offset_map <- P_glaucus_offset %>%
  left_join(., grids_5, by = c("grid" = "grid_id")) %>% #grids_5 is an st 
  st_as_sf() #turn st into sf 

# first plot
P_glaucus_p1 <- ggplot(P_glaucus_offset_map) +
  geom_sf(aes(fill = offset)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Median Flight Period") +
  theme_bw() +
  labs(title = "Offset of Flight Period of Papilio glaucus")

# Second plot 
P_glaucus_p2 <- ggplot(P_glaucus_offset_map) +
  geom_sf(aes(fill = mean_GHMI)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Mean GHMI (Urbanization)") +
  theme_bw() +
  labs(title = "Urbanization (GHMI) Across Bioregion NA24")

# Combine the two maps side by side
final_plot <- P_glaucus_p1 + P_glaucus_p2 + plot_layout(ncol = 2)
final_plot

#save as png: 
ggsave("Figures/Papilio_glaucuse_Offset_Bioregion_NA24_map.png", width=6, height=6, units="in")



#############Figure 8: showing offset of Lepidoptera across mapped grids with 
#comparison to GHMI


#Want only offset for Lepidoptera
Lepidoptera_offset <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order == "Lepidoptera", !is.na(grid), !is.na(offset)) %>%
  select(order, grid, offset, mean_GHMI)
head(Lepidoptera_offset) #check that it worked


#join data with grids, then make it an sf object
Lepidoptera_offset_map <- Lepidoptera_offset %>%
  left_join(., grids_5, by = c("grid" = "grid_id")) %>% #grids_5 is an st 
  st_as_sf() #turn st into sf 

# first plot
Lepidoptera_p1 <- ggplot(Lepidoptera_offset_map) +
  geom_sf(aes(fill = offset)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Offset of Flight Period") +
  theme_bw() +
  labs(title = "Offset of Flight Period of Lepidopterans")

# Second plot 
Lepidoptera_p2 <- ggplot(Lepidoptera_offset_map) +
  geom_sf(aes(fill = mean_GHMI)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Mean GHMI (Anthropogenic Change)") +
  theme_bw() +
  labs(title = "Anthropogenic Change (GHMI) Across Florida")

# Combine the two maps side by side
final_plot <- Lepidoptera_p1 + Lepidoptera_p2 + plot_layout(ncol = 2)
final_plot

#save as png: 
ggsave("Figures/Lepidoptera_offset_Bioregion_NA24_map.png", width=6, height=6, units="in")



#############Figure 9: showing median of Hymenoptera across mapped grids with 
#comparison to GHMI


#want only medians for Hymenoptera
Hymenoptera_medians <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order == "Hymenoptera", !is.na(grid), !is.na(median)) %>%
  select(order, grid, median, mean_GHMI)
head(Hymenoptera_medians) #check that it worked


#join data with grids, then make it an sf object
Hymenoptera_median_map <- Hymenoptera_medians %>%
  left_join(., grids_5, by = c("grid" = "grid_id")) %>% #grids_5 is an st 
  st_as_sf() #turn st into sf 

# first plot
Hymenoptera_p1 <- ggplot(Hymenoptera_median_map) +
  geom_sf(aes(fill = median)) +
  geom_sf(data = florida, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Median Flight Period") +
  theme_bw() +
  labs(title = "Median Flight Period of Hymenopterans")

# Second plot 
Hymenoptera_p2 <- ggplot(Hymenoptera_median_map) +
  geom_sf(aes(fill = mean_GHMI)) +
  geom_sf(data = florida, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Mean GHMI (Urbanization)") +
  theme_bw() +
  labs(title = "Urbanization Across Florida (GHMI)")

# Combine the two maps side by side
final_plot <- Hymenoptera_p1 + Hymenoptera_p2 + plot_layout(ncol = 2)
final_plot

#save as png: 
ggsave("Figures/Hymenoptera_medians_Fl_map.png", width=6, height=6, units="in")




#############Figure 10: showing median of Diptera across mapped grids with 
#comparison to GHMI


#want only medians for Diptera
Diptera_medians <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order == "Diptera", !is.na(grid), !is.na(median)) %>%
  select(order, grid, median, mean_GHMI)
head(Diptera_medians) #check that it worked


#join data with grids, then make it an sf object
Diptera_median_map <- Diptera_medians %>%
  left_join(., grids_5, by = c("grid" = "grid_id")) %>% #grids_5 is an st 
  st_as_sf() #turn st into sf 

# first plot
Diptera_p1 <- ggplot(Diptera_median_map) +
  geom_sf(aes(fill = median)) +
  geom_sf(data = florida, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Median Flight Period") +
  theme_bw() +
  labs(title = "Median Flight Period of Dipterans")

# Second plot 
Diptera_p2 <- ggplot(Diptera_median_map) +
  geom_sf(aes(fill = mean_GHMI)) +
  geom_sf(data = florida, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Mean GHMI (Urbanization)") +
  theme_bw() +
  labs(title = "Urbanization Across Florida (GHMI)")

# Combine the two maps side by side
final_plot <- Diptera_p1 + Diptera_p2 + plot_layout(ncol = 2)
final_plot

#save as png: 
ggsave("Figures/Diptera_medians_Fl_map.png", width=6, height=6, units="in")


####Marina Note: there's only one Dipteran grid. Consider taking it out of analysis. 


#############Figure 11: showing median of Coleoptera across mapped grids with 
#comparison to GHMI


#want only medians for Coleoptera
Coleoptera_medians <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order == "Coleoptera", !is.na(grid), !is.na(median)) %>%
  select(order, grid, median, mean_GHMI)
head(Coleoptera_medians) #check that it worked


#join data with grids, then make it an sf object
Coleoptera_median_map <- Coleoptera_medians %>%
  left_join(., grids_5, by = c("grid" = "grid_id")) %>% #grids_5 is an st 
  st_as_sf() #turn st into sf 

# first plot
Coleoptera_p1 <- ggplot(Coleoptera_median_map) +
  geom_sf(aes(fill = median)) +
  geom_sf(data = florida, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Median Flight Period") +
  theme_bw() +
  labs(title = "Median Flight Period of Coleopterans")

# Second plot 
Coleoptera_p2 <- ggplot(Coleoptera_median_map) +
  geom_sf(aes(fill = mean_GHMI)) +
  geom_sf(data = florida, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Mean GHMI (Urbanization)") +
  theme_bw() +
  labs(title = "Urbanization Across Florida (GHMI)")

# Combine the two maps side by side
final_plot <- Coleoptera_p1 + Coleoptera_p2 + plot_layout(ncol = 2)
final_plot

#save as png: 
ggsave("Figures/Coleoptera_medians_Fl_map.png", width=6, height=6, units="in")



#############Figure 12: showing median of all pollinators across mapped grids with 
#comparison to GHMI


#join data with grids, then make it an sf object
median_map <- phenology_estimates_all_species_each_grid_with_landsat %>%
  left_join(., grids_5, by = c("grid" = "grid_id")) %>% #grids_5 is an st 
  st_as_sf() #turn st into sf 

# first plot
median_p1 <- ggplot(median_map) +
  geom_sf(aes(fill = median)) +
  geom_sf(data = florida, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Median Flight Period") +
  theme_bw() +
  labs(title = "Median Flight Period of all Pollinators")

# Second plot 
median_p2 <- ggplot(median_map) +
  geom_sf(aes(fill = mean_GHMI)) +
  geom_sf(data = florida, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Mean GHMI (Urbanization)") +
  theme_bw() +
  labs(title = "Urbanization Across Florida (GHMI)")

# Combine the two maps side by side
final_plot <- median_p1 + median_p2 + plot_layout(ncol = 2)
final_plot

#save as png: 
ggsave("Figures/medians_Fl_map.png", width=6, height=6, units="in")






################ Figure 13: Onset, Median, and Offset In Low Versus High GHMI areas

phenology_estimates_all_species_each_grid_with_landsat %>%
  dplyr::select(1, 3:7) %>%
  pivot_longer(!c(species, mean_GHMI)) %>%
  ggplot(., aes(x=mean_GHMI, y=value, color=name))+
  geom_point()+
  theme_bw()+
  facet_wrap(~name, ncol=1, scales="free_y")

#save as png: 
ggsave("Figures/mean_median_offset_duration_in_low_versus_high_ghmi.png", width=6, height=6, units="in")






############## Figure 14: Total Duration of flight period in low versus high GHMI areas for 10 random species
# get example species
example_species <- phenology_estimates_all_species_each_grid_with_landsat %>%
  group_by(species) %>%
  summarize(N=n()) %>%
  dplyr::filter(N>10) %>%
  sample_n(10)

phenology_estimates_all_species_each_grid_with_landsat %>%
  dplyr::select(1:7) %>%
  dplyr::filter(species %in% example_species$species) %>%
  dplyr::select(-grid) %>%
  pivot_longer(!c(species, mean_GHMI)) %>%
  dplyr::filter(name=="duration") %>%
  ggplot(., aes(x=mean_GHMI, y=value, color=name))+
  geom_point()+
  theme_bw()+
  geom_smooth(method="lm")+
  facet_wrap(~species, ncol=2, scales="free_y")+
  labs(
    title = "Total Duration of 10 Random Species in Low Verus High GHMI Areas",
    x = "Mean GHMI",
    y = "Total Duration of Flight Period (Days)")+
  theme(legend.position = "none")
  
#Save it 
ggsave("Figures/total_duration_low_versus_high_urban_for_10_random_species.png", width=6, height=6, units="in")







############## Figure 15: Onset of flight period in low versus high GHMI areas for 10 random species
############### Plotting this for onset
phenology_estimates_all_species_each_grid_with_landsat %>%
  dplyr::select(1:7) %>%
  dplyr::filter(species %in% example_species$species) %>%
  dplyr::select(-grid) %>%
  pivot_longer(!c(species, mean_GHMI)) %>%
  dplyr::filter(name=="onset") %>%
  ggplot(., aes(x=mean_GHMI, y=value, color=name))+
  geom_point()+
  theme_bw()+
  geom_smooth(method="lm")+
  facet_wrap(~species, ncol=2, scales="free_y")+
  labs(
    title = "Onset of 10 Random Species in Low Verus High GHMI Areas",
    x = "Mean GHMI",
    y = "Onset (Days)")+
  theme(legend.position = "none")

#Save it 
ggsave("Figures/onset_low_versus_high_urban_for_10_random_species.png", width=6, height=6, units="in")






################ Figure 16: Average Onset, Median, and Offset In Low Versus High GHMI areas
 
average_per_GHMI <- phenology_estimates_all_species_each_grid_with_landsat %>%
  group_by(mean_GHMI) %>%
  summarise(
    avg_onset = mean(onset, na.rm = TRUE),
    avg_offset = mean(offset, na.rm = TRUE),
    avg_median = mean(median, na.rm = TRUE)
  )
average_per_GHMI

#plot 1: onset 
p1 <- ggplot(average_per_GHMI, aes(x = mean_GHMI, y = avg_onset)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Mean GHMI", y = "Onset") +
  theme(plot.title = element_text(hjust = 0.5))

#plot 2: median
p2 <- ggplot(average_per_GHMI, aes(x = mean_GHMI, y = avg_median)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Mean GHMI", y = "Median") +
  theme(plot.title = element_text(hjust = 0.5))

# plot 3: offset
p3 <- ggplot(average_per_GHMI, aes(x = mean_GHMI, y = avg_offset)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Mean GHMI", y = "Offset") +
  theme(plot.title = element_text(hjust = 0.5))


# Figure title
main_title <- textGrob("Average Flight Period Estimates Across an Urban Gradient (GHMI)", gp = gpar(fontsize = 16, fontface = "bold"))

#Plot them together
grid.arrange(main_title, p1, p2, p3, ncol = 1, heights = c(0.1, 1, 1, 1))  #3 rows, 1 column
grid.arrange(main_title, p1, p2, p3, 
             ncol = 3, layout_matrix = rbind(c(1, 1, 1), c(2, 3, 4)), 
             widths = c(1, 1, 1), heights = c(0.1, 1)) 
#1 row, 3 columns

#Save it 
ggsave("Figures/onset_median_offset_in_low_versus_high_urban.png", width=6, height=6, units="in")







############## Figure 17: Total Duration of flight period in low versus high GHMI areas for 10 random Lepidopterans

example_leps <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order=="Lepidoptera") %>%
  group_by(species) %>%
  summarize(N=n()) %>%
  dplyr::filter(N>10) %>%
  sample_n(10)

phenology_estimates_all_species_each_grid_with_landsat %>%
  dplyr::select(1:7) %>%
  dplyr::filter(species %in% example_leps$species) %>%
  dplyr::select(-grid) %>%
  pivot_longer(!c(species, mean_GHMI)) %>%
  dplyr::filter(name=="duration") %>%
  ggplot(., aes(x=mean_GHMI, y=value, color=name))+
  geom_point()+
  theme_bw()+
  geom_smooth(method="lm")+
  facet_wrap(~species, ncol=2, scales="free_y")+
  labs(
    title = "Total Duration of Flight Period of 10 Random Species in Low Verus High GHMI Areas",
    x = "Mean GHMI",
    y = "Total Duration (Days)")+
  theme(legend.position = "none")+
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10)  # adjust size as needed
  )

#Save it 
ggsave("Figures/total_duration_in_low_versus_high_urban_for_10_random_leps.png", width=6, height=6, units="in")








################# Figure 18: comparing total duration of 5 generalists and 5 specialists 

functional_groups_example <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(species=="Apis mellifera"|species=="Danaus gilippus"|species=="Dione vanillae"|species=="Eurema daira"|
           species=="Hemiargus ceraunus"|species=="Kricogonia lyside"|species=="Habropoda laboriosa"|
           species=="Dryas iulia"|species=="Eumaeus atala"|species=="Phyciodes phaon")%>%
  mutate(feeding_specialization = ifelse(species %in% c("Apis mellifera", "Danaus gilippus", "Dione vanillae", "Eurema daira", "Hemiargus ceraunus"), 
                                         "generalist", 
                                         "specialist"))
#Plot it
functional_groups_example %>%
  dplyr::select(-grid) %>%
  dplyr::select(where(is.numeric), species) %>%  
  pivot_longer(!c(species, mean_GHMI)) %>%
  dplyr::filter(name == "duration") %>%
  mutate(feeding_specialization = ifelse(species %in% c("Apis mellifera", "Danaus gilippus", "Dione vanillae", "Eurema daira", "Hemiargus ceraunus"), 
                                         "Generalist", "Specialist")) %>%
  ggplot(aes(x = mean_GHMI, y = value, color = name)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm") +
  facet_wrap(~species + feeding_specialization, ncol = 2, scales = "free_y")

#Save it 
ggsave("Figures/total_duration_in_low_versus_high_urban_for_functional_groups_10_species.png", width=6, height=6, units="in")






################# Figure 19: comparing total duration of 10 pre-selected species 


#Look at all the species in each Order (want a variety)
phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order=="Hymenoptera")%>%
  distinct(species)
phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order=="Diptera")%>%
  distinct(species)
phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order=="Coleoptera")%>%
  distinct(species)
phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order=="Lepidoptera")%>%
  distinct(species)


#filter for only these species 
ten_selected_species <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(species=="Halictus poeyi"|species=="Xylocopa virginica"|species=="Apis mellifera"|species=="Euphoria sepulcralis"|
           species=="Trigonopeltastes delta"|species=="Danaus plexippus"|species=="Polites vibex"|
           species=="Erynnis horatius"|species=="Phoebis sennae"|species=="Hylephila phyleus")


#Create a list of the common names
common_names <- c(
  "Halictus poeyi" = "Poey's Furrow Bee",
  "Xylocopa virginica" = "Eastern Carpenter Bee",
  "Apis mellifera" = "Western Honey Bee",
  "Euphoria sepulcralis" = "Dark Flower Scarab",
  "Trigonopeltastes delta" = "Delta Flower Scarab",
  "Polites vibex" = "Whirlabout Skipper",
  "Danaus plexippus" = "Monarch Butterfly",
  "Erynnis horatius" = "Horace's Duskywing",
  "Phoebis sennae" = "Cloudless Sulphur",
  "Hylephila phyleus" = "Fiery Skipper"
)


#Plot it
phenology_estimates_all_species_each_grid_with_landsat %>%
  dplyr::select(1:7) %>%
  dplyr::filter(species %in% names(common_names)) %>%
  dplyr::mutate(species = common_names[species]) %>%  # relabel species to common names
  dplyr::select(-grid) %>%
  pivot_longer(!c(species, mean_GHMI)) %>%
  dplyr::filter(name == "duration") %>%
  ggplot(aes(x = mean_GHMI, y = value, color = name)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm") +
  facet_wrap(~species, ncol = 2, scales = "free_y") +
  labs(
    title = "Total Duration of Flight Period of 10 Species in Low Versus High GHMI Areas",
    x = "Mean GHMI",
    y = "Total Duration (Days)"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10)
  )



#Save it 
ggsave("Figures/total_duration_in_low_versus_high_urban_for_10_pre-selected_species.png", width=6, height=6, units="in")







################# Figure 20: comparing onset of 10 pre-selected species 

#Plot it
phenology_estimates_all_species_each_grid_with_landsat %>%
  dplyr::select(1:7) %>%
  dplyr::filter(species %in% names(common_names)) %>%
  dplyr::mutate(species = common_names[species]) %>%  # relabel species to common names
  dplyr::select(-grid) %>%
  pivot_longer(!c(species, mean_GHMI)) %>%
  dplyr::filter(name == "onset") %>%
  ggplot(aes(x = mean_GHMI, y = value, color = name)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm") +
  facet_wrap(~species, ncol = 2, scales = "free_y") +
  labs(
    title = "Onset of Flight Period of 10 Species in Low Versus High GHMI Areas",
    x = "Mean GHMI",
    y = "Flight Period Onset (Days)"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10)
  )



#Save it 
ggsave("Figures/onset_in_low_versus_high_urban_for_10_pre-selected_species.png", width=6, height=6, units="in")








################# Figure 21: comparing offset of 10 pre-selected species 


#Plot it
phenology_estimates_all_species_each_grid_with_landsat %>%
  dplyr::select(1:7) %>%
  dplyr::filter(species %in% names(common_names)) %>%
  dplyr::mutate(species = common_names[species]) %>%  # relabel species to common names
  dplyr::select(-grid) %>%
  pivot_longer(!c(species, mean_GHMI)) %>%
  dplyr::filter(name == "offset") %>%
  ggplot(aes(x = mean_GHMI, y = value, color = name)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm") +
  facet_wrap(~species, ncol = 2, scales = "free_y") +
  labs(
    title = "Offset of Flight Period of 10 Species in Low Versus High GHMI Areas",
    x = "Mean GHMI",
    y = "Flight Period Offset (Days)"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10)
  )



#Save it 
ggsave("Figures/offset_in_low_versus_high_urban_for_10_pre-selected_species.png", width=6, height=6, units="in")








######## Figure 22: Plotting slopes of species' change in total duration across range of GHMI for 20
#        randomly selected species 

# Randomly sample 20 species
set.seed(123)  
random_species <- sample(unique(slopes_df$species), 20)

# Include only the randomly selected species
slopes_df_random <- slopes_df %>%
  filter(species %in% random_species)

# Get the original species names
species_names <- unique(phenology_estimates_all_species_each_grid_with_landsat$species)

# Match numeric species values to their corresponding names
slopes_df_random$species_name <- species_names[slopes_df_random$species]

#Plot the slopes with the species names on the y-axis
ggplot(slopes_df_random, aes(x = slope, y = species_name)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Slope of Total Flight Period Duration across a Range of GHMI values for 20 Randomly Selected Species", 
       x = "Slope of Duration vs GHMI", y = "Species") +
  theme(axis.text.y = element_text(size = 8))  # Adjust size of species names

#Save it 
ggsave("Figures/slope_of_species_duration_plot_20_random_species.png", width=6, height=6, units="in", bg = "transparent")


names(slopes_df)






######## Figure 23: Plotting slopes of species' change in total duration across range of GHMI for 10
#        example species  

# Calculate the slope for each species
slopes_df <- ten_selected_species %>%
  mutate(common_names = recode(species, !!!common_names))%>%
  group_by(species) %>%
  filter(!is.na(duration) & !is.na(mean_GHMI)) %>%  # Remove rows with NAs
  do({
    if (nrow(.) > 1) {  # Ensure there are at least two points to fit a model
      model <- lm(duration ~ mean_GHMI, data = .)  # Fit model for each species
      tidy_model <- tidy(model)  # Extract coefficients
      slope <- tidy_model %>% filter(term == "mean_GHMI") %>% pull(estimate)  # Get the slope
      data.frame(species = unique(.$species), slope = slope)  # Return species and slope
    } else {
      data.frame(species = unique(.$species), slope = NA)  # Return NA if not enough data
    }
  })



# Plot the slopes 
ggplot(slopes_df, aes(x = common_names, y = slope)) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Total Flight Duration Across a Range of GHMI (Urbanization) Values for 10 Species",
    x = "Species (Common Name)",
    y = "Slope of Duration versus GHMI"
  ) +
  coord_flip() +
  ylim(0, NA) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)
  )

#Save it 
ggsave("Figures/slope_of_ten_selected_species_duration_plot.png", width=6, height=6, units="in", bg = "transparent")







######## Figure 24: Plotting slopes of species' change in onset across range of GHMI for 10
#        example species  

# Calculate the slope for each species
slopes_df <- ten_selected_species %>%
  mutate(common_names = recode(species, !!!common_names))%>%
  group_by(species) %>%
  filter(!is.na(onset) & !is.na(mean_GHMI)) %>%  # Remove rows with NAs
  do({
    if (nrow(.) > 1) {  # Ensure there are at least two points to fit a model
      model <- lm(onset ~ mean_GHMI, data = .)  # Fit model for each species
      tidy_model <- tidy(model)  # Extract coefficients
      slope <- tidy_model %>% filter(term == "mean_GHMI") %>% pull(estimate)  # Get the slope
      data.frame(species = unique(.$species), slope = slope)  # Return species and slope
    } else {
      data.frame(species = unique(.$species), slope = NA)  # Return NA if not enough data
    }
  })



# Plot the slopes 
ggplot(slopes_df, aes(x = common_names, y = slope)) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Total Flight Onset Across a Range of GHMI (Urbanization) Values for 10 Species",
    x = "Species (Common Name)",
    y = "Slope of Onset versus GHMI"
  ) +
  coord_flip() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)
  )

#Save it 
ggsave("Figures/slope_of_ten_selected_species_onset_plot.png", width=6, height=6, units="in", bg = "transparent")





######## Figure 25: Plotting slopes of species' change in offset across range of GHMI for 10
#        example species  

# Calculate the slope for each species
slopes_df <- ten_selected_species %>%
  mutate(common_names = recode(species, !!!common_names))%>%
  group_by(species) %>%
  filter(!is.na(offset) & !is.na(mean_GHMI)) %>%  # Remove rows with NAs
  do({
    if (nrow(.) > 1) {  # Ensure there are at least two points to fit a model
      model <- lm(offset ~ mean_GHMI, data = .)  # Fit model for each species
      tidy_model <- tidy(model)  # Extract coefficients
      slope <- tidy_model %>% filter(term == "mean_GHMI") %>% pull(estimate)  # Get the slope
      data.frame(species = unique(.$species), slope = slope)  # Return species and slope
    } else {
      data.frame(species = unique(.$species), slope = NA)  # Return NA if not enough data
    }
  })



# Plot the slopes 
ggplot(slopes_df, aes(x = common_names, y = slope)) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Total Flight Offset Across a Range of GHMI (Urbanization) Values for 10 Species",
    x = "Species (Common Name)",
    y = "Slope of Offset versus GHMI"
  ) +
  coord_flip() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)
  )

#Save it 
ggsave("Figures/slope_of_ten_selected_species_offset_plot.png", width=6, height=6, units="in", bg = "transparent")






######## Figure 26: Plotting slopes of species' change in total duration across range of GHMI for 10
#        example species  

# Calculate the slope for each species
slopes_df <- ten_selected_species %>%
  group_by(species) %>%
  filter(!is.na(duration) & !is.na(mean_GHMI)) %>%  # Remove rows with NAs
  do({
    if (nrow(.) > 1) {  # Ensure there are at least two points to fit a model
      model <- lm(duration ~ mean_GHMI, data = .)  # Fit model for each species
      tidy_model <- tidy(model)  # Extract coefficients
      slope <- tidy_model %>% filter(term == "mean_GHMI") %>% pull(estimate)  # Get the slope
      data.frame(species = unique(.$species), slope = slope)  # Return species and slope
    } else {
      data.frame(species = unique(.$species), slope = NA)  # Return NA if not enough data
    }
  })



# Plot the slopes 
ggplot(slopes_df, aes(x = common_names, y = slope)) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Total Flight Duration Across a Range of GHMI (Urbanization) Values for 10 Species",
    x = "Species (Common Name)",
    y = "Slope of Duration versus GHMI"
  ) +
  coord_flip() +
  ylim(0, NA) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)
  )

#Save it 
ggsave("Figures/slope_of_ten_selected_species_duration_plot.png", width=6, height=6, units="in", bg = "transparent")



######## Figure 27: Plotting slopes of species' change in total duration across range of GHMI for all 
#        Lepidopterans 

# Calculate the slope for each species
slopes_df <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order=="Lepidoptera")%>%
  group_by(species) %>%
  filter(!is.na(duration) & !is.na(mean_GHMI)) %>%  # Remove rows with NAs
  do({
    if (nrow(.) > 1) {  # Ensure there are at least two points to fit a model
      model <- lm(duration ~ mean_GHMI, data = .)  # Fit model for each species
      tidy_model <- tidy(model)  # Extract coefficients
      slope <- tidy_model %>% filter(term == "mean_GHMI") %>% pull(estimate)  # Get the slope
      data.frame(species = unique(.$species), slope = slope)  # Return species and slope
    } else {
      data.frame(species = unique(.$species), slope = NA)  # Return NA if not enough data
    }
  })
# Convert species names to numeric indices
slopes_df$species <- as.numeric(factor(slopes_df$species))

#filter out any slopes greater than 100 or less than -100
slopes_df <- slopes_df %>%
  filter(slope > -50 & slope < 50)

# Plot the slopes 
ggplot(slopes_df, aes(x = species, y = slope)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.6) +  # red horizontal line (becomes vertical after coord_flip)
  theme_minimal() +
  labs(
    title = "Total Flight Duration Across a Range of GHMI (Urbanization) Values for All Lepidoptera Species",
    x = "Species Identification Number",
    y = "Slope of Duration versus GHMI"
  ) +
  coord_flip() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#Save it 
ggsave("Figures/slope_of_all_Lepidoptera_species_duration_plot.png", width=6, height=6, units="in", bg = "transparent")











######## Figure 28: Plotting slopes of species' change in total duration across range of GHMI for all species

# Calculate slope and SE per species
slopes_df <- phenology_estimates_all_species_each_grid_with_landsat %>%
  group_by(species) %>%
  filter(!is.na(duration) & !is.na(mean_GHMI)) %>%
  do({
    if (nrow(.) > 1) {
      model <- lm(duration ~ mean_GHMI, data = .)
      tidy_model <- broom::tidy(model)
      slope <- tidy_model %>% filter(term == "mean_GHMI") %>% pull(estimate)
      se <- tidy_model %>% filter(term == "mean_GHMI") %>% pull(std.error) #whiskers 
      data.frame(species = unique(.$species), slope = slope, se = se)
    } else {
      data.frame(species = unique(.$species), slope = NA, se = NA)
    }
  }) %>%
  filter(slope > -50 & slope < 50) %>%
  arrange(slope)

# Explicitly set factor levels of species in slope order
species_order <- unique(as.character(slopes_df$species))
slopes_df$species <- factor(slopes_df$species, levels = species_order)

# Plot
ggplot(slopes_df, aes(x = slope, y = species)) +
  geom_point() +
  geom_errorbarh(aes(xmin = slope - se, xmax = slope + se), height = 0.2) +
  geom_vline(xintercept = 0, color = "red") +
  theme_minimal() +
  labs(
    x = "Slope of Duration vs GHMI",
    y = "Species",
    title = "Total Duration of Flight Period Across a Range of GHMI (Urbanization) Values for All Species"
  ) +
  theme(axis.text.y = element_text(size = 8))


#Save it 
ggsave("Figures/slope_of_species_duration_plot.png", width=6, height=6, units="in", bg = "transparent")









######## Figure 29: Plotting slopes of species' onset across range of GHMI for all species


# Calculate slopes for onset
slopes_df <- phenology_estimates_all_species_each_grid_with_landsat %>%
  group_by(species) %>%
  filter(!is.na(onset) & !is.na(mean_GHMI)) %>%
  do({
    if (nrow(.) > 1) {
      model <- lm(onset ~ mean_GHMI, data = .)
      tidy_model <- broom::tidy(model)
      slope <- tidy_model %>% filter(term == "mean_GHMI") %>% pull(estimate)
      se <- tidy_model %>% filter(term == "mean_GHMI") %>% pull(std.error)
      data.frame(species = unique(.$species), slope = slope, se = se)
    } else {
      data.frame(species = unique(.$species), slope = NA, se = NA)
    }
  }) %>%
  filter(slope > -50 & slope < 50) %>%
  arrange(slope)

species_order <- unique(as.character(slopes_df$species))
slopes_df$species <- factor(slopes_df$species, levels = species_order)

# Plot
ggplot(slopes_df, aes(x = slope, y = species)) +
  geom_point() +
  geom_errorbarh(aes(xmin = slope - se, xmax = slope + se), height = 0.2) +
  geom_vline(xintercept = 0, color = "red", linewidth = 0.6) +
  theme_minimal() +
  labs(
    title = "Flight Period Onset Across a Range of GHMI (Urbanization) Values for All Species",
    x = "Slope of Onset vs. GHMI",
    y = "Species"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

#Save it 
ggsave("Figures/slope_of_species_onset_plot.png", width=6, height=6, units="in", bg = "transparent")









######## Figure 30: Plotting slopes of species' offset across range of GHMI for all species

# Calculate slopes for offset
slopes_df <- phenology_estimates_all_species_each_grid_with_landsat %>%
  group_by(species) %>%
  filter(!is.na(offset) & !is.na(mean_GHMI)) %>%
  do({
    if (nrow(.) > 1) {
      model <- lm(offset ~ mean_GHMI, data = .)
      tidy_model <- broom::tidy(model)
      slope <- tidy_model %>% filter(term == "mean_GHMI") %>% pull(estimate)
      se <- tidy_model %>% filter(term == "mean_GHMI") %>% pull(std.error)
      data.frame(species = unique(.$species), slope = slope, se = se)
    } else {
      data.frame(species = unique(.$species), slope = NA, se = NA)
    }
  }) %>%
  filter(slope > -50 & slope < 50) %>%
  arrange(slope)

species_order <- unique(as.character(slopes_df$species))
slopes_df$species <- factor(slopes_df$species, levels = species_order)

# Plot
ggplot(slopes_df, aes(x = slope, y = species)) +
  geom_point() +
  geom_errorbarh(aes(xmin = slope - se, xmax = slope + se), height = 0.2) +
  geom_vline(xintercept = 0, color = "red", linewidth = 0.6) +
  theme_minimal() +
  labs(
    title = "Flight Period Offset Across a Range of GHMI (Urbanization) Values for All Species",
    x = "Slope of Offset vs. GHMI",
    y = "Species"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )


#Save it 
ggsave("Figures/slope_of_species_offset_plot.png", width=6, height=6, units="in", bg = "transparent")









######## Figure 31: Average Total Duration values across a range of GHMI values for 
#        4 species 


# Filter for the 8 species we want to look at, italicize the names 
selected_species <- c(
  "Heliconius charithonia", "Eumaeus atala", "Phoebis sennae", "Polites vibex",
  "Papilio glaucus", "Polites otho", "Phyciodes tharos", "Euglossa dilemma"
)

italic_species_labels <- c(
  "Heliconius charithonia" = "italic('Heliconius charithonia')",
  "Eumaeus atala" = "italic('Eumaeus atala')",
  "Phoebis sennae" = "italic('Phoebis sennae')",
  "Polites vibex" = "italic('Polites vibex')",
  "Papilio glaucus" = "italic('Papilio glaucus')",
  "Polites otho" = "italic('Polites otho')",
  "Phyciodes tharos" = "italic('Phyciodes tharos')",
  "Euglossa dilemma" = "italic('Euglossa dilemma')"
)

eight_species <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(species %in% selected_species)
eight_species$species_label <- italic_species_labels[eight_species$species] 


# Calculate mean total duration and SE per GHMI value
plot_data <- eight_species %>%
  group_by(species, mean_GHMI) %>%
  summarise(
    mean_duration = mean(duration, na.rm = TRUE),
    se_duration = sd(duration, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Plot
ggplot() +
  geom_smooth(data = eight_species,
              aes(x = mean_GHMI, y = duration),
              method = "lm",
              se = FALSE,
              color = "red",
              linewidth = 0.7) +
  geom_point(data = plot_data,
             aes(x = mean_GHMI, y = mean_duration)) +
  facet_wrap(~species_label, ncol = 2, scales = "free", labeller = label_parsed) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.background = element_rect(color = "black", fill = "gray90", linewidth = 0.5),
    strip.text = element_text(size = 9),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  labs(
    title = "Mean Duration Across a Range of GHMI Values for 8 Species",
    x = "Global Human Modification Index (GHMI)",
    y = "Mean Total Duration (Days)"
  )

# Save it
ggsave("Figures/duration_across_ghmi_for_8_species.png", width = 8, height = 10, units = "in", bg = "transparent")























######## Figures 32-34: Average Flight Period Values Across a Range of GHMI values for the species
#        that showed GHMI to be a predictor of these flight period values (based on GAMs)



# Species lists by phenotype
species_onset <- c("Erynnis horatius", "Hylephila phyleus", "Phoebis sennae",
                   "Eurema daira", "Bombus pensylvanicus", "Polites otho")

species_offset <- c("Danaus plexippus", "Heliconius charithonia", "Eumaeus atala",
                    "Phyciodes tharos", "Syngamia florella", "Calpodes ethlius")

species_duration <- c("Heliconius charithonia", "Eumaeus atala", "Phoebis sennae",
                      "Polites vibex", "Papilio glaucus", "Polites otho",
                      "Phyciodes tharos", "Euglossa dilemma", "Trigonopeltastes delta")

# Common names vector (extend as needed)
common_names <- c(
  "Erynnis horatius" = "Horace's Duskywing",
  "Hylephila phyleus" = "Fiery Skipper",
  "Phoebis sennae" = "Cloudless Sulphur",
  "Eurema daira" = "Barred Yellow",
  "Bombus pensylvanicus" = "American Bumble Bee",
  "Polites otho" = "Southern Broken-Dash",
  "Danaus plexippus" = "Monarch Butterfly",
  "Heliconius charithonia" = "Zebra Longwing",
  "Eumaeus atala" = "Atala",
  "Phyciodes tharos" = "Pearl Crescent",
  "Syngamia florella" = "Orange-spotted Flower Moth",
  "Calpodes ethlius" = "Brazilian Skipper",
  "Polites vibex" = "Whirlabout",
  "Papilio glaucus" = "Eastern Tiger Swallowtail",
  "Euglossa dilemma" = "Dilemma Orchid Bee",
  "Trigonopeltastes delta" = "Delta Flower Scarab"
)



plot_phenology <- function(data, species_vec, common_names_vec, response_var, plot_title, ncol = 2, nrow = NULL) {
  
  # Filter species and add common names
  data_filtered <- data %>%
    filter(species %in% species_vec) %>%
    mutate(species_common = common_names_vec[species])
  
  # Calculate mean response per GHMI per species
  plot_data <- data_filtered %>%
    group_by(species_common, mean_GHMI) %>%
    summarise(
      mean_value = mean(.data[[response_var]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Determine facet rows if not provided
  if (is.null(nrow)) {
    nrow <- ceiling(length(species_vec) / ncol)
  }
  
  # Plot using tidy evaluation for aesthetics
  p <- ggplot() +
    geom_smooth(data = data_filtered,
                aes(x = mean_GHMI, y = .data[[response_var]]),
                method = "lm", se = FALSE,
                color = "red", linewidth = 0.7) +
    geom_point(data = plot_data,
               aes(x = mean_GHMI, y = mean_value)) +
    facet_wrap(~species_common, ncol = ncol, nrow = nrow, scales = "free") +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.background = element_rect(color = "black", fill = "gray90", linewidth = 0.5),
      strip.text = element_text(size = 9),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.spacing = unit(1, "lines"),
      plot.title = element_text(size = 12),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(size = 8),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA)
    ) +
    labs(
      title = plot_title,
      x = "GHMI",
      y = paste("Mean", tools::toTitleCase(response_var))
    )
  
  return(p)
}


# Replace with your full dataset name:
data_all <- phenology_estimates_all_species_each_grid_with_landsat

# Onset figure (6 species, 2 cols × 3 rows)
p_onset <- plot_phenology(
  data = data_all,
  species_vec = species_onset,
  common_names_vec = common_names,
  response_var = "onset",
  plot_title = "Mean Onset of Flight Period Across GHMI for Selected Species",
  ncol = 2, nrow = 3
)
print(p_onset)

# Offset figure (6 species, 2 cols × 3 rows)
p_offset <- plot_phenology(
  data = data_all,
  species_vec = species_offset,
  common_names_vec = common_names,
  response_var = "offset",
  plot_title = "Mean Offset of Flight Period Across GHMI for Selected Species",
  ncol = 2, nrow = 3
)
print(p_offset)

# Duration figure (9 species)
# Use 3 columns × 3 rows here to keep things balanced for 9 species
p_duration <- plot_phenology(
  data = data_all,
  species_vec = species_duration,
  common_names_vec = common_names,
  response_var = "duration",
  plot_title = "Mean Duration of Flight Period Across GHMI for Selected Species",
  ncol = 3, nrow = 3
)
print(p_duration)

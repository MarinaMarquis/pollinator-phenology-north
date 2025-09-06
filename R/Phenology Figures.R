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
  scale_fill_viridis_c(name = "Mean GHMI (Anthropogenic Change)") +
  theme_bw() +
  labs(title = "Anthropogenic Change Across Bioregion NA24")

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
  scale_fill_viridis_c(name = "Mean GHMI (Anthropogenic Change)") +
  theme_bw() +
  labs(title = "Anthropogenic Change Across Bioregion NA24")

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
  labs(title = "Anthropogenic Change Across Bioregion NA24")

# Combine the two maps side by side
final_plot <- Lepidoptera_p1 + Lepidoptera_p2 + plot_layout(ncol = 2)
final_plot

#save as png: 
ggsave("Figures/Lepidoptera_offset_Bioregion_NA24_map.png", width=6, height=6, units="in")



#############Figure 9: showing offset of Hymenoptera across mapped grids with 
#comparison to GHMI


#want only medians for Hymenoptera
Hymenoptera_offset <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order == "Hymenoptera", !is.na(grid), !is.na(offset)) %>%
  select(order, grid, offset, mean_GHMI)
head(Hymenoptera_offset) #check that it worked


#join data with grids, then make it an sf object
Hymenoptera_offset_map <- Hymenoptera_offset %>%
  left_join(., grids_5, by = c("grid" = "grid_id")) %>% #grids_5 is an st 
  st_as_sf() #turn st into sf 

# first plot
Hymenoptera_p1 <- ggplot(Hymenoptera_offset_map) +
  geom_sf(aes(fill = offset)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Offset of Flight Period") +
  theme_bw() +
  labs(title = "Offset of Flight Period of Hymenopterans")

# Second plot 
Hymenoptera_p2 <- ggplot(Hymenoptera_offset_map) +
  geom_sf(aes(fill = mean_GHMI)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Mean GHMI (Anthropogenic Change)") +
  theme_bw() +
  labs(title = "Anthropogenic Change Across Bioregion NA24")

# Combine the two maps side by side
final_plot <- Hymenoptera_p1 + Hymenoptera_p2 + plot_layout(ncol = 2)
final_plot

#save as png: 
ggsave("Figures/Hymenoptera_offset_Bioregion_NA24_map.png", width=6, height=6, units="in")




#############Figure 10: showing offset of Diptera across mapped grids with 
#comparison to GHMI


#want only offset for Diptera
Diptera_offset <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order == "Diptera", !is.na(grid), !is.na(offset)) %>%
  select(order, grid, offset, mean_GHMI)
head(Diptera_offset) #check that it worked


#join data with grids, then make it an sf object
Diptera_offset_map <- Diptera_offset %>%
  left_join(., grids_5, by = c("grid" = "grid_id")) %>% #grids_5 is an st 
  st_as_sf() #turn st into sf 

# first plot
Diptera_p1 <- ggplot(Diptera_offset_map) +
  geom_sf(aes(fill = offset)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Offset of Flight Period") +
  theme_bw() +
  labs(title = "Offset of Flight Period of Dipterans")

# Second plot 
Diptera_p2 <- ggplot(Diptera_offset_map) +
  geom_sf(aes(fill = mean_GHMI)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Mean GHMI (Anthropogenic Change)") +
  theme_bw() +
  labs(title = "Anthropogenic Change Across Bioregion NA24")

# Combine the two maps side by side
final_plot <- Diptera_p1 + Diptera_p2 + plot_layout(ncol = 2)
final_plot

#save as png: 
ggsave("Figures/Diptera_offset_Bioregion_NA24_map.png", width=6, height=6, units="in")




#############Figure 11: showing offset of Coleoptera across mapped grids with 
#comparison to GHMI


#want only offset for Coleoptera
Coleoptera_offset <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order == "Coleoptera", !is.na(grid), !is.na(offset)) %>%
  select(order, grid, offset, mean_GHMI)
head(Coleoptera_offset) #check that it worked


#join data with grids, then make it an sf object
Coleoptera_offset_map <- Coleoptera_offset %>%
  left_join(., grids_5, by = c("grid" = "grid_id")) %>% #grids_5 is an st 
  st_as_sf() #turn st into sf 

# first plot
Coleoptera_p1 <- ggplot(Coleoptera_offset_map) +
  geom_sf(aes(fill = offset)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Offset of Flight Period") +
  theme_bw() +
  labs(title = "Offset of Flight Period of Coleopterans")

# Second plot 
Coleoptera_p2 <- ggplot(Coleoptera_offset_map) +
  geom_sf(aes(fill = mean_GHMI)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Mean GHMI (Anthropogenic Change)") +
  theme_bw() +
  labs(title = "Anthropogenic Change Across Bioregion NA24")

# Combine the two maps side by side
final_plot <- Coleoptera_p1 + Coleoptera_p2 + plot_layout(ncol = 2)
final_plot

#save as png: 
ggsave("Figures/Coleoptera_offset_Bioregion_NA24_map.png", width=6, height=6, units="in")



#############Figure 12: showing offset of all pollinators across mapped grids with 
#comparison to GHMI


#join data with grids, then make it an sf object
offset_map <- phenology_estimates_all_species_each_grid_with_landsat %>%
  left_join(., grids_5, by = c("grid" = "grid_id")) %>% #grids_5 is an st 
  st_as_sf() #turn st into sf 

# first plot
offset_p1 <- ggplot(offset_map) +
  geom_sf(aes(fill = offset)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Offset of Flight Period") +
  theme_bw() +
  labs(title = "Offset of Flight Period of all Pollinators")

# Second plot 
offset_p2 <- ggplot(offset_map) +
  geom_sf(aes(fill = mean_GHMI)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Mean GHMI (Anthropogenic Change)") +
  theme_bw() +
  labs(title = "Anthropogenic Change Across Bioregion NA24")

# Combine the two maps side by side
final_plot <- offset_p1 + offset_p2 + plot_layout(ncol = 2)
final_plot

#save as png: 
ggsave("Figures/offset_Bioregion_NA24_map.png", width=6, height=6, units="in")






################ Figure 13: Onset, Offset, and Duration In Low Versus High GHMI areas

phenology_estimates_all_species_each_grid_with_landsat %>%
  dplyr::select(1, 3, 5:7) %>%
  pivot_longer(!c(species, mean_GHMI)) %>%
  ggplot(., aes(x=mean_GHMI, y=value, color=name))+
  geom_point()+
  theme_bw()+
  facet_wrap(~name, ncol=1, scales="free_y")

#save as png: 
ggsave("Figures/mean_offset_duration_in_low_versus_high_ghmi.png", width=6, height=6, units="in")





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






################ Figure 16: Average Onset, Offset, and Duration In Low Versus High GHMI areas
 
average_per_GHMI <- phenology_estimates_all_species_each_grid_with_landsat %>%
  group_by(mean_GHMI) %>%
  summarise(
    avg_onset = mean(onset, na.rm = TRUE),
    avg_offset = mean(offset, na.rm = TRUE),
    avg_duration = mean(offset, na.rm = TRUE)
  )
average_per_GHMI

#plot 1: onset 
p1 <- ggplot(average_per_GHMI, aes(x = mean_GHMI, y = avg_onset)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Mean GHMI", y = "Onset") +
  theme(plot.title = element_text(hjust = 0.5))

#plot 2: duration
p2 <- ggplot(average_per_GHMI, aes(x = mean_GHMI, y = avg_duration)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Mean GHMI", y = "Total Duration") +
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
ggsave("Figures/onset_offset_duration_in_low_versus_high_urban.png", width=6, height=6, units="in")







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
  filter(species=="Apis mellifera"|species=="Bombus impatiens"|species=="Vespula squamosa"|species=="Xylocopa virginica"|
           species=="Toxomerus geminatus"|species=="Clogmia albipunctatus"|species=="Eristalis tenax"|
           species=="Coleomegilla maculata"|species=="Papilio troilus"|species=="Urbanus proteus")


#Create a list of the common names
common_names <- c(
  "Bombus impatiens" = "Common Eastern Bumble Bee",
  "Vespula squamosa" = "Southern Yellowjacket",
  "Apis mellifera" = "Western Honey Bee",
  "Toxomerus geminatus" = "Eastern Calligrapher",
  "Clogmia albipunctatus" = "Delta Flower Scarab",
  "Eristalis tenax" = "Common Drone Fly",
  "Coleomegilla maculata" = "Spotted Pink Lady Beetle",
  "Papilio troilus" = "Spicebush Swallowtail",
  "Urbanus proteus" = "Long-tailed Skipper",
  "Xylocopa virginica" = "Eastern Carpenter Bee"
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

#Can also plot it like this, with scientific names
phenology_estimates_all_species_each_grid_with_landsat %>%
  dplyr::select(1:7) %>%
  dplyr::filter(species %in% unique(ten_selected_species$species)) %>%  
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

#Scientific names option
#Plot it
phenology_estimates_all_species_each_grid_with_landsat %>%
  dplyr::select(1:7) %>%
  dplyr::filter(species %in% unique(ten_selected_species$species)) %>%
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

#Plot with scientific names 
phenology_estimates_all_species_each_grid_with_landsat %>%
  dplyr::select(1:7) %>%
  dplyr::filter(species %in% unique(ten_selected_species$species)) %>%
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

# Calculate the slope for each species
slopes_df <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(!is.na(duration), !is.na(mean_GHMI)) %>%
  group_by(species) %>%
  summarise(
    slope = tryCatch(
      tidy(lm(duration ~ mean_GHMI, data = pick(duration, mean_GHMI))) %>%
        filter(term == "mean_GHMI") %>%
        pull(estimate),
      error = function(e) NA_real_
    ),
    .groups = "drop"
  )

# Randomly sample 20 species
set.seed(123)  
random_species <- sample(unique(slopes_df$species), 20)

# Include only the randomly selected species
slopes_df_random <- slopes_df %>%
  filter(species %in% random_species)

#Plot the slopes with the species names on the y-axis
ggplot(slopes_df_random, aes(x = slope, y = species)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Slope of Total Flight Period Duration across a Range of GHMI values for 20 Randomly Selected Species", 
       x = "Slope of Duration vs GHMI", y = "Species") +
  theme(axis.text.y = element_text(size = 8))  # Adjust size of species names

#Save it 
ggsave("Figures/slope_of_species_duration_plot_20_random_species.png", width=6, height=6, units="in", bg = "transparent")








######## Figure 23: Plotting slopes of species' change in total duration across range of GHMI for 10
#        example species  

# Calculate the slope for each species
slopes_df <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(!is.na(duration), !is.na(mean_GHMI)) %>%
  group_by(species) %>%
  summarise(
    slope = tryCatch(
      tidy(lm(duration ~ mean_GHMI, data = pick(duration, mean_GHMI))) %>%
        filter(term == "mean_GHMI") %>%
        pull(estimate),
      error = function(e) NA_real_
    ),
    .groups = "drop"
  )

# Randomly sample 10 species
set.seed(123)  
random_species <- sample(unique(slopes_df$species), 10)

# Include only the randomly selected species
slopes_df_random <- slopes_df %>%
  filter(species %in% random_species)

#Plot the slopes with the species names on the y-axis
ggplot(slopes_df_random, aes(x = slope, y = species)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Slope of Total Flight Period Duration across a Range of GHMI values for 10 Randomly Selected Species", 
       x = "Slope of Duration vs GHMI", y = "Species") +
  theme(axis.text.y = element_text(size = 8))  # Adjust size of species names

#Save it 
ggsave("Figures/slope_of_ten_selected_species_duration_plot.png", width=6, height=6, units="in", bg = "transparent")







######## Figure 24: Plotting slopes of species' change in onset across range of GHMI for 10
#        example species  

# Calculate the slope for each species
slopes_df <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(!is.na(onset), !is.na(mean_GHMI)) %>%
  group_by(species) %>%
  summarise(
    slope = tryCatch(
      tidy(lm(onset ~ mean_GHMI, data = pick(onset, mean_GHMI))) %>%
        filter(term == "mean_GHMI") %>%
        pull(estimate),
      error = function(e) NA_real_
    ),
    .groups = "drop"
  )

# Randomly sample 10 species
set.seed(123)  
random_species <- sample(unique(slopes_df$species), 10)

# Include only the randomly selected species
slopes_df_random <- slopes_df %>%
  filter(species %in% random_species)

#Plot the slopes with the species names on the y-axis
ggplot(slopes_df_random, aes(x = slope, y = species)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Slope of Total Flight Period Onset across a Range of GHMI values for 10 Randomly Selected Species", 
       x = "Slope of Onset vs GHMI", y = "Species") +
  theme(axis.text.y = element_text(size = 8))  # Adjust size of species names

#Save it 
ggsave("Figures/slope_of_ten_selected_species_onset_plot.png", width=6, height=6, units="in", bg = "transparent")





######## Figure 25: Plotting slopes of species' change in offset across range of GHMI for 10
#        example species  

# Calculate the slope for each species
slopes_df <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(!is.na(offset), !is.na(mean_GHMI)) %>%
  group_by(species) %>%
  summarise(
    slope = tryCatch(
      tidy(lm(offset ~ mean_GHMI, data = pick(offset, mean_GHMI))) %>%
        filter(term == "mean_GHMI") %>%
        pull(estimate),
      error = function(e) NA_real_
    ),
    .groups = "drop"
  )

# Randomly sample 10 species
set.seed(123)  
random_species <- sample(unique(slopes_df$species), 10)

# Include only the randomly selected species
slopes_df_random <- slopes_df %>%
  filter(species %in% random_species)

#Plot the slopes with the species names on the y-axis
ggplot(slopes_df_random, aes(x = slope, y = species)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Slope of Flight Period Offset across a Range of GHMI values for 10 Randomly Selected Species", 
       x = "Slope of Offset vs GHMI", y = "Species") +
  theme(axis.text.y = element_text(size = 8))  # Adjust size of species names

#Save it 
ggsave("Figures/slope_of_ten_selected_species_offset_plot.png", width=6, height=6, units="in", bg = "transparent")









######## Figure 26: Plotting slopes of species' change in total duration across range of GHMI for all 
#        Lepidopterans 

# Calculate the slope for each species
slopes_df <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order=="Lepidoptera")%>%
  group_by(species) %>%
  filter(!is.na(duration) & !is.na(mean_GHMI)) %>%  # Remove rows with NAs
  summarise(
    slope = tryCatch(
      tidy(lm(duration~mean_GHMI, data = pick(duration, mean_GHMI)))%>%
        filter(term == "mean_GHMI")%>%
        pull(estimate),
      error = function(e) NA_real_
    ),
    .groups = "drop"
  )
  

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
    title = "Total Flight Duration Across a Range of GHMI Values for All Lepidoptera Species",
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











######## Figure 27: Plotting slopes of species' change in total duration across range of GHMI for all species

# Calculate slope and SE per species
slopes_df <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(!is.na(duration), !is.na(mean_GHMI)) %>%
  group_split(species) %>%
  map_dfr(~ {
    sp <- unique(.x$species)
    mod <- tryCatch(lm(duration ~ mean_GHMI, data = .x), error = function(e) NULL)
    if (!is.null(mod)) {
      tidy(mod) %>%
        filter(term == "mean_GHMI") %>%
        transmute(species = sp, slope = estimate, se = std.error)
    }
  })


# Explicitly set factor levels of species in slope order
species_order <- unique(as.character(slopes_df$species))
slopes_df$species <- factor(slopes_df$species, levels = species_order)

# Give id numbers to the species so the plot doesn't look crowded 
slopes_df <- slopes_df %>%
  mutate(species_id = as.numeric(factor(species))) 

# Plot with numbers on the y-axis
ggplot(slopes_df, aes(x = slope, y = species_id)) +
  geom_point() +
  geom_errorbarh(aes(xmin = slope - se, xmax = slope + se), height = 0.2) +
  geom_vline(xintercept = 0, color = "red") +
  theme_minimal() +
  labs(
    x = "Slope of Duration vs GHMI",
    y = "Species ID",
    title = "Total Duration of Flight Period Across a Range of GHMI Values for All Species"
  ) +
  theme(axis.text.y = element_text(size = 6))

#Save it 
ggsave("Figures/slope_of_species_duration_plot.png", width=6, height=6, units="in", bg = "transparent")









######## Figure 28: Plotting slopes of species' onset across range of GHMI for all species


# Calculate slope and SE per species
slopes_df <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(!is.na(onset), !is.na(mean_GHMI)) %>%
  group_split(species) %>%
  map_dfr(~ {
    sp <- unique(.x$species)
    mod <- tryCatch(lm(onset ~ mean_GHMI, data = .x), error = function(e) NULL)
    if (!is.null(mod)) {
      tidy(mod) %>%
        filter(term == "mean_GHMI") %>%
        transmute(species = sp, slope = estimate, se = std.error)
    }
  })


# Explicitly set factor levels of species in slope order
species_order <- unique(as.character(slopes_df$species))
slopes_df$species <- factor(slopes_df$species, levels = species_order)

# Give id numbers to the species so the plot doesn't look crowded 
slopes_df <- slopes_df %>%
  mutate(species_id = as.numeric(factor(species))) 

# Plot with numbers on the y-axis
ggplot(slopes_df, aes(x = slope, y = species_id)) +
  geom_point() +
  geom_errorbarh(aes(xmin = slope - se, xmax = slope + se), height = 0.2) +
  geom_vline(xintercept = 0, color = "red") +
  theme_minimal() +
  labs(
    x = "Slope of Onset vs GHMI",
    y = "Species ID",
    title = "Onset of Flight Period Across a Range of GHMI Values for All Species"
  ) +
  theme(axis.text.y = element_text(size = 6))

#Save it 
ggsave("Figures/slope_of_species_onset_plot.png", width=6, height=6, units="in", bg = "transparent")









######## Figure 29: Plotting slopes of species' offset across range of GHMI for all species

# Calculate slope and SE per species
slopes_df <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(!is.na(offset), !is.na(mean_GHMI)) %>%
  group_split(species) %>%
  map_dfr(~ {
    sp <- unique(.x$species)
    mod <- tryCatch(lm(offset ~ mean_GHMI, data = .x), error = function(e) NULL)
    if (!is.null(mod)) {
      tidy(mod) %>%
        filter(term == "mean_GHMI") %>%
        transmute(species = sp, slope = estimate, se = std.error)
    }
  })


# Explicitly set factor levels of species in slope order
species_order <- unique(as.character(slopes_df$species))
slopes_df$species <- factor(slopes_df$species, levels = species_order)


# Give id numbers to the species so the plot doesn't look crowded 
slopes_df <- slopes_df %>%
  mutate(species_id = as.numeric(factor(species))) 

# Plot with numbers on the y-axis
ggplot(slopes_df, aes(x = slope, y = species_id)) +
  geom_point() +
  geom_errorbarh(aes(xmin = slope - se, xmax = slope + se), height = 0.2) +
  geom_vline(xintercept = 0, color = "red") +
  theme_minimal() +
  labs(
    x = "Slope of Offset vs GHMI",
    y = "Species ID",
    title = "Offset of Flight Period Across a Range of GHMI Values for All Species"
  ) +
  theme(axis.text.y = element_text(size = 6))

#Save it 
ggsave("Figures/slope_of_species_offset_plot.png", width=6, height=6, units="in", bg = "transparent")







#filter for only these species 
ten_selected_species <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(species=="Bombus impatiens"|species=="Xylocopa virginica"|
           species=="Toxomerus geminatus"|species=="Clogmia albipunctatus"|species=="Eristalis tenax"|
           species=="Coleomegilla maculata"|species=="Papilio troilus"|species=="Urbanus proteus")


#Create a list of the common names
common_names <- c(
  "Bombus impatiens" = "Common Eastern Bumble Bee",
  "Toxomerus geminatus" = "Eastern Calligrapher",
  "Clogmia albipunctatus" = "Delta Flower Scarab",
  "Eristalis tenax" = "Common Drone Fly",
  "Coleomegilla maculata" = "Spotted Pink Lady Beetle",
  "Papilio troilus" = "Spicebush Swallowtail",
  "Urbanus proteus" = "Long-tailed Skipper",
  "Xylocopa virginica" = "Eastern Carpenter Bee"
)

######## Figure 30: Average Total Duration values across a range of GHMI values for 
#        8 species 


# Filter for the 8 species we want to look at, italicize the names 
selected_species <- c(
  "Bombus impatiens", "Xylocopa virginica", "Toxomerus geminatus", "Clogmia albipunctatus", "Eristalis tenax",
  "Coleomegilla maculata", "Papilio troilus","Urbanus proteus"
)

italic_species_labels <- c(
  "Bombus impatiens" = "italic('Bombus impatiens')",
  "Toxomerus geminatus" = "italic('Toxomerus geminatus')",
  "Clogmia albipunctatus" = "italic('Clogmia albipunctatus')",
  "Eristalis tenax" = "italic('Eristalis tenax')",
  "Coleomegilla maculata" = "italic('Coleomegilla maculata')",
  "Papilio troilus" = "italic('Papilio troilus')",
  "Urbanus proteus" = "italic('Urbanus proteus')",
  "Xylocopa virginica" = "italic('Xylocopa virginica')"
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























######## Figures 31-33: Average Flight Period Values Across a Range of GHMI values for the species
#        that showed GHMI to be a predictor of these flight period values (based on GAMs)



# Species lists by phenotype
species_onset <- c("Xylocopa virginica", "Papilio troilus", "Eremnophila aureonotata",
                   "Eristalis tenax", "Vespula squamosa", "Clogmia albipunctatus",   
                   "Helicoverpa zea")

species_offset <- c("Bombus impatiens", "Papilio glaucus", "Danaus plexippus", "Epargyreus clarus",
                    "Phyciodes tharos", "Hylephila phyleus",        
                    "Pyrrharctia isabella", "Battus philenor", "Tetraopes tetrophthalmus",
                    "Noctua pronuba", "Euclea delphinii", "Limenitis arthemis")

species_duration <- c("Xylocopa virginica", "Apis mellifera", "Pyrrharctia isabella", 
                      "Papilio troilus", "Hypoprepia fucosa", "Noctua pronuba", 
                      "Clogmia albipunctatus")




plot_phenology <- function(data, species_vec, response_var, plot_title, ncol = 2, nrow = NULL) {
  
  # Filter species
  data_filtered <- data %>%
    filter(species %in% species_vec)
  
  # Calculate mean response per GHMI per species
  plot_data <- data_filtered %>%
    group_by(species, mean_GHMI) %>%
    summarise(
      mean_value = mean(.data[[response_var]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Determine facet rows if not provided
  if (is.null(nrow)) {
    nrow <- ceiling(length(species_vec) / ncol)
  }
  
  # Plot
  p <- ggplot() +
    geom_smooth(data = data_filtered,
                aes(x = mean_GHMI, y = .data[[response_var]]),
                method = "lm", se = FALSE,
                color = "red", linewidth = 0.7) +
    geom_point(data = plot_data,
               aes(x = mean_GHMI, y = mean_value)) +
    facet_wrap(~species, ncol = ncol, nrow = nrow, scales = "free") +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.background = element_rect(color = "black", fill = "gray90", linewidth = 0.5),
      strip.text = element_text(size = 9, face = "italic"),  # italic for scientific names
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

# Plot it 
p_onset <- plot_phenology(
  data = phenology_estimates_all_species_each_grid_with_landsat,
  species_vec = species_onset,
  response_var = "onset",
  plot_title = "Mean Onset of Flight Period Across GHMI for Selected Species",
  ncol = 4, nrow = 2
)
print(p_onset)

# Save it
ggsave("Figures/avg_onset_across_ghmi_for_sig_gam_species.png", width = 8, height = 10, units = "in", bg = "transparent")

p_offset <- plot_phenology(
  data = phenology_estimates_all_species_each_grid_with_landsat,
  species_vec = species_offset,
  response_var = "offset",
  plot_title = "Mean Offset of Flight Period Across GHMI for Selected Species",
  ncol = 4, nrow = 3
)
print(p_offset)

#Save it
ggsave("Figures/avg_offset_across_ghmi_for_sig_gam_species.png", width = 8, height = 10, units = "in", bg = "transparent")

p_duration <- plot_phenology(
  data = phenology_estimates_all_species_each_grid_with_landsat,
  species_vec = species_duration,
  response_var = "duration",
  plot_title = "Mean Duration of Flight Period Across GHMI for Selected Species",
  ncol = 4, nrow = 2
)
print(p_duration)

#Save it
ggsave("Figures/avg_duration_across_ghmi_for_sig_gam_species.png", width = 8, height = 10, units = "in", bg = "transparent")


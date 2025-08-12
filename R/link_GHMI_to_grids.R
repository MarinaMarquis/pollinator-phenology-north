#Marina Marquis
#Making a data frame with mean GHMI value for each grid in the data set  

#############################################################################################################


# Read in data 
GHMI <- read.csv("Data/Spatial Data/GHMI/mean_gHM.csv")
filtered_grids_with_observations <- readRDS("Data/filtered_5.rds")

# Load packages
library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)


#############################################################################################################

### We only want the unique grids from filtered_5 (no repeats) so that we can use this 
#   data frame in the future to simply match observations to grids to landsat variables 

# Combine these data frames by grid id

filtered_grids_with_observations <- filtered_grids_with_observations %>%
  select(grid_id) %>% 
  distinct() 

merged_df <- merge(filtered_grids_with_observations, GHMI[, c("grid_id", "mean")], by = "grid_id", all.x = TRUE)



# Export
write.csv(merged_df, "Data/filtered_5_with_GHMI.csv")


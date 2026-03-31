#### Investigating Temporal Bias in Phenology Estimates 
#Marina Marquis 
# We want to know whether sampling effort in certain years influenced our phenology
# estimates of species. This is because our data set ranges from 2009, when iNaturalist
# first launched, to 2024, when it was more widely used. In this script, we take a sub-sample
# of the 10 most abundance species in our data set and estimate their onset, offset, and
# total duration of activity for the years 2012-2020. We will then compare this to the
# phenological estimates we obtained from them using 2009-2024 observation data. 

# Load packages 
library(dplyr)
library(phenesse)
library(sf)
library(lubridate)
library(purrr)


# Read in the data 
phenology_estimates_all_species_each_grid <- readRDS('Data/phenology_estimates_by_grid_by_species.RDS')  
#phenology estimates for each species in each grid for the years 2009-2024
observations_with_landsat_variables <- readRDS("Data/observations_with_landsat_variables.rds")
#raw data of observations, GHMI, and grid cell. Filtered to only include 
#observations of species that will be used for analysis 




########################################################################################################


# Look at top 10 most frequently observed species in the data set: 
top_ten <- observations_with_landsat_variables %>%
  group_by(species)%>%
  summarise(n_obs = n())%>%
  arrange(desc(n_obs))%>%
  slice_head(n=10)
top_ten 

# Now filter data for only these species and years 2012-2020
top_species_data <- observations_with_landsat_variables %>%
  filter(species %in% top_ten$species, 
         year > 2011 & year < 2021) 



########################################################################################################

### Write a function to get estimates for any species
get_phenology_estimates_function <- function(species_name, grid_number) {
  
  message(paste0("Calculating phenology estimates for grid id: ", grid_number))
  message(paste0("Calculating phenology estimates for ", species_name))
  
  tryCatch({
    # Filter data to the relevant grid
    grid_data <- top_species_data %>%
      filter(grid_id == grid_number)
    
    # Get data for a given species
    data <- grid_data %>%
      filter(species == species_name) %>%
      dplyr::select(species, grid_id, eventDate) %>%
      mutate(day_of_year = as.integer(yday(eventDate))) %>%
      filter(day_of_year > 0)
    
    # Check if there are enough data points to run estimates
    if (nrow(data) < 3) {
      message("Not enough data to estimate phenology.")
      return(data.frame(species = species_name, 
                        grid = grid_number,
                        onset = NA, 
                        median = NA, 
                        offset = NA, 
                        duration = NA))
    }
    
    # Get onset (0.1 percentile)
    onset <- weib_percentile(observations = data$day_of_year, 
                             percentile = 0.1, iterations = 500)
    
    # Get offset (0.9 percentile)
    offset <- weib_percentile(observations = data$day_of_year, 
                              percentile = 0.9, iterations = 500)
    
    # Get median (0.5 percentile)
    median <- weib_percentile(observations = data$day_of_year, 
                              percentile = 0.5, iterations = 500)
    
    # Create summary dataframe
    summary_df <- data.frame(species = species_name, 
                             grid = grid_number, 
                             onset = onset, 
                             median = median,
                             offset = offset) %>%
      mutate(duration = offset - onset)
    
    return(summary_df)
  }, error = function(e) {
    # Return a message and an NA-filled row if an error occurs
    message(paste0("Error for species ", species_name, ": ", e$message))
    return(data.frame(species = species_name, 
                      grid = grid_number,
                      onset = NA, 
                      median = NA, 
                      offset = NA, 
                      duration = NA))
  })
}



########################################################################################################### 


### Apply this function over every species in each grid for subsetted 2012-2020 data 
phenology_estimates_all_species_each_grid <- top_species_data %>%
  distinct(species, grid_id) %>%  # Get unique species-grid combinations
  pmap_dfr(~get_phenology_estimates_function(.x, .y))  # Apply function to each combination


saveRDS(phenology_estimates_all_species_each_grid, "Data/phenology_estimates_sample_subset.RDS")

########################################################################################################### 

### Filter phenology data (estimated from observations from years 2009-2024) to only 
#   have estimates for the same 10 species 
phenology_estimates_all_species_each_grid_top_ten <- phenology_estimates_all_species_each_grid%>%
  filter(species %in% top_ten$species)
  

########################################################################################################### 
 
### Compare the phenology estimates obtained from observations from years 2009-2024 versus 
#   years 2012-2020











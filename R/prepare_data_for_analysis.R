# Marina Marquis
# This script is to combine data into data prepared for analysis!

#############################################################################################################

# Load Packages 
library(dplyr)
library(phenesse)
library(sf)
library(lubridate)
library(purrr)
library(moments)
library(diptest)


# Read in data  
filtered_5 <- readRDS("Data/filtered_5.rds") #observations used to make phenology estimates 
filtered_5_with_landsat <- read.csv("Data/filtered_5_with_GHMI.csv") # mean GHMI for each grid 
phenology_estimates_all_species_each_grid <- readRDS('Data/phenology_estimates_by_grid_by_species.RDS') #Phenology 
#estimates for each species in each grid 
taxonomy <- readRDS("Data/iNaturalist_pollinator_observations.rds") %>%
  dplyr::select(species, genus, family, order) %>%
  distinct() # Read in observations and get the higher level taxonomy

#############################################################################################################

### Merge phenology_estimates_all_species_each_grid with filtered_5_with_landsat so that 
#   the data frame with phenology estimates of each species in each grid also has mean GHMI for each grid

# Merge them into one data set with observations and mean GHMI per grid: 
phenology_estimates_all_species_each_grid_with_GHMI <- phenology_estimates_all_species_each_grid %>%
  left_join(filtered_5_with_landsat %>%
              select(grid_id, mean_GHMI = mean), by = c("grid" = "grid_id")) %>%
  left_join(., taxonomy, by="species")



# Filter to include only species that are found in at least six grids
phenology_estimates_all_species_each_grid_with_GHMI <- phenology_estimates_all_species_each_grid_with_GHMI %>%
  filter(species %in% (
    group_by(., species) %>%
      summarize(n_grids = n(), .groups = 'drop') %>%
      filter(n_grids >= 6) %>%
      pull(species)
  ))

# Check that it worked 
check <- phenology_estimates_all_species_each_grid_with_GHMI %>%
  distinct(species, grid) %>%
  group_by(species) %>%
  summarize(n_grids = n()) %>%
  arrange(n_grids)
print(check, n = Inf)



#############################################################################################################


#Label the functional groups of all of the species in the study, take out species that are not pollinators
#e.g., moths that don't have mouth parts or simply don't eat as adults)

unique(phenology_estimates_all_species_each_grid_with_GHMI$species) #get list of species 
phenology_estimates_all_species_each_grid_with_GHMI <- phenology_estimates_all_species_each_grid_with_GHMI %>%
  filter(species != "Actias luna",
         species !=  "Eacles imperialis",
         species != "Hyphantria cunea", 
         species != "Hypercompe scribonia",
         species != "Malacosoma americana",
         species != "Malacosoma disstria", 
         species != "Automeris io",
         species != "Promachus rufipes",
         species != "Chrysopilus thoracicus",
         species != "Delphinia picta",
         species != "Harmonia axyridis",
         species != "Polygonia interrogationis",
         species != "Solenopsis invicta", 
         species != "Asterocampa celtis", 
         species != "Cotinis nitida", 
         species != "Acrolophus popeanella", 
         species != "Apatelodes torrefacta", 
         species != "Dyspteris abortivaria", 
         species != "Eubaphe mendica"
  )
unique(phenology_estimates_all_species_each_grid_with_GHMI$species) #double check new species list

# Adding functional groups column
phenology_estimates_all_species_each_grid_with_GHMI <- phenology_estimates_all_species_each_grid_with_GHMI %>%
  mutate(functional_group = case_when(
    species %in% c(
      "Ascia monuste", "Apis mellifera", "Danaus gilippus", "Danaus plexippus",
      "Dione vanillae", "Eurema daira", "Hemiargus ceraunus", "Hylephila phyleus",
      "Junonia coenia", "Leptotes cassius", "Phoebis agarithe", "Polites otho",
      "Polygonus leo", "Cymaenes tripunctus", "Dryas iulia", "Eumaeus atala",
      "Heliconius charithonia", "Phocides pigmalion", "Urbanus proteus", 
      "Xylocopa micans", "Nathalis iole", "Papilio cresphontes", "Euglossa dilemma",
      "Oligoria maculata", "Parapoynx allionealis", "Polites baracoa", 
      "Syntomeida epilais", "Limenitis archippus", "Panoquina ocola",
      "Syngamia florella", "Lerema accius", "Papilio polyxenes", "Phyciodes phaon",
      "Battus polydamas", "Erynnis horatius", "Halictus poeyi", "Antheraea polyphemus",
      "Euphoria sepulcralis", "Xylophanes tersa", "Pyrausta tyralis", 
      "Agapostemon splendens", "Phoebis philea", "Phoebis sennae", "Calpodes ethlius",
      "Papilio palamedes", "Polites vibex", "Phyciodes tharos", "Copaeodes minima",
      "Burnsius albezens", "Euphyes arpa", "Bombus pensylvanicus", "Calycopis cecrops",
      "Strymon melinus", "Polites themistocles", "Papilio troilus", "Vanessa atalanta",
      "Lycia ypsilon", "Papilio glaucus", "Composia fidelissima", "Utetheisa ornatrix", "Nastra lherminier", "Panoquina panoquin",
      "Euptoieta claudia", "Thorybes pylades", "Erynnis zarucco", "Battus philenor",
      "Bombus impatiens", "Atlides halesus", "Trigonopeltastes delta", 
      "Xylocopa virginica", "Problema byssus"
    ) ~ "Polyphagous",
    
    species %in% c(
      "Kricogonia lyside", "Junonia neildi", "Anartia jatrophae", "Marpesia petreus"
    ) ~ "Monophagous",
    
    species %in% c(
      "Habropoda laboriosa", "Pyrisitia lisa", "Papilio polyxenes"
    ) ~ "Oligophagous",
    
    TRUE ~ "Unknown"
  ))



# When checking species functional groups, I found a mistake in one of the species. The species Urola nivalis
# mistakenly has Argyria listed under the genus column and Argyria nivalis under the species name. Argyria nivalis
# is not a real species, but Urola nivalis is. This correct species name is written under the 
# verbatimScientificName column. I'm not sure how that got messed up. However, I will change all of these
# instances so that they show the correct genus and species. 

phenology_estimates_all_species_each_grid_with_GHMI <- 
  phenology_estimates_all_species_each_grid_with_GHMI %>%
  mutate(
    change_condition = species == "Argyria nivalis" & genus == "Argyria",
    species = ifelse(change_condition, "Urola nivalis", species),
    genus   = ifelse(change_condition, "Urola", genus)
  ) %>%
  select(-change_condition)


# Look at the data 
length(na.omit(unique(phenology_estimates_all_species_each_grid_with_GHMI$grid)))
#look at how many grids we're left with: 285
length(na.omit(unique(phenology_estimates_all_species_each_grid_with_GHMI$family))) #number of families: 49
unique(phenology_estimates_all_species_each_grid_with_GHMI$order) #4 orders 



#############################################################################################################


# Let's see how many phenology estimates exceeded 365 days of year 
sum(phenology_estimates_all_species_each_grid_with_GHMI$onset > 365, na.rm = TRUE)
sum(phenology_estimates_all_species_each_grid_with_GHMI$offset > 365, na.rm = TRUE) #28 instances 
sum(phenology_estimates_all_species_each_grid_with_GHMI$duration > 365, na.rm = TRUE) #4 instances 

# We need to investigate these values
overestimates <- phenology_estimates_all_species_each_grid_with_GHMI %>%
  filter(offset > 365 | duration > 365)%>%
  print()

# Example species and grid from suspicious list, e.g. Hypena scabra at grid 68
species_of_interest <- "Hypena scabra"
grid_of_interest <- 68

# Subset the data
obs <- filtered_5 %>%
  filter(species == species_of_interest, grid_id == grid_of_interest) %>%
  mutate(day_of_year = as.integer(lubridate::yday(eventDate))) %>%
  filter(day_of_year > 0)

# Basic stats
summary(obs$day_of_year)
cat("Number of observations:", nrow(obs), "\n")
cat("Max observed day:", max(obs$day_of_year), "\n")

# Histogram
ggplot(obs, aes(x = day_of_year)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  ggtitle(paste("Day of year distribution for", species_of_interest, "in grid", grid_of_interest))

# Print phenology estimates for reference
phenology_estimates_all_species_each_grid_with_GHMI %>%
  filter(species == species_of_interest, grid == grid_of_interest) %>%
  select(onset, median, offset, duration) %>%
  print()

# For this sample species, n is high so that likely isn't the issue. We are likely getting an estimate over 
#365 because the data are left-skewed, so the Weibull distribution tries to fit this shape and goes over 
#the 365 day mark. Let's see if this is happening with the other weird estimates: 


# View results for suspicious species and grids only
suspicious_species_grids <- phenology_estimates_all_species_each_grid_with_GHMI %>%
  filter(offset > 365 | duration > 365) %>%
  select(species, grid) %>%
  distinct()


# Calculate skewness and sample size per species-grid combo
skewness_suspicious <- filtered_5 %>%
  filter(paste(species, grid_id) %in% paste(
    overestimates$species, overestimates$grid
  )) %>%
  mutate(day_of_year = as.integer(lubridate::yday(eventDate))) %>%
  filter(day_of_year > 0) %>%
  group_by(species, grid_id) %>%
  summarise(
    skewness = if(n() > 2) skewness(day_of_year, na.rm = TRUE) else NA_real_,
    n_obs = n(),
    .groups = "drop"
  )
print(skewness_suspicious, n=36)

#Negative skewness values indicate left-skewness (tail on left side). Positive skewness means right-skew.
#Most of the weird phenology estimates are left-skewed. Many of the largest left (negative) skews are 
#with relatively small sample sizes (n_obs).  So it looks like a low sample size and strong negative 
#skew = high risk of offset/duration > 365 days. Larger sample size can help, but left skewness alone can 
#push estimates past 365. We do see some moderate negative skews with moderate n. This suggests that
#sample size helps but skew can still produce weird estimates. 

# Define species-grid combos to investigate
right_skewed <- tibble(
  species = c("Clogmia albipunctatus", "Iridopsis defectaria"),
  grid_id = c(16896, 1217)
)

# Loop over each species-grid and make plots and summaries
for(i in seq_len(nrow(right_skewed))) {
  sp <- right_skewed$species[i]
  grid <- right_skewed$grid_id[i]
  
  message(paste0("Investigating ", sp, " in grid ", grid))
  
  obs <- filtered_5 %>%
    filter(species == sp, grid_id == grid) %>%
    mutate(day_of_year = as.integer(yday(eventDate))) %>%
    filter(day_of_year > 0)
  
  # Summary stats
  print(summary(obs$day_of_year))
  cat("Number of observations:", nrow(obs), "\n")
  
  # Histogram to check distribution shape
  p <- ggplot(obs, aes(x = day_of_year)) +
    geom_histogram(binwidth = 5, fill = "orange", color = "black") +
    ggtitle(paste0("Day of year distribution for ", sp, " in grid ", grid))
  print(p)
  
  # Extract phenology estimates for reference
  phenos <- phenology_estimates_all_species_each_grid_with_GHMI %>%
    filter(species == sp, grid == grid) %>%
    select(onset, median, offset, duration)
  print(phenos)
  
  cat("\n-----\n")
}

# The first species looks like it has a bimodal distribution, the second looks like it may have more
# year-round activity with a large peak around 200 day mark. Weibull assumes unimodality. I will now look
# at how many of these have bimodal distributions. 

bimodality_results <- filtered_5 %>%
  mutate(day_of_year = lubridate::yday(eventDate)) %>%
  filter(day_of_year > 0) %>%
  group_by(species, grid_id) %>%
  summarise(
    n_obs = n(),
    dip_p_value = if(n_obs > 10) dip.test(day_of_year)$p.value else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(is_bimodal = dip_p_value < 0.05)

bimodality_results %>% filter(is_bimodal)

# Now looking at how many of my suspicious estimates were bimodal
suspicious_bimodal <- overestimates %>%
  left_join(bimodality_results, 
            by = c("species" = "species", "grid" = "grid_id")) %>%
  filter(is_bimodal == TRUE)

# View how many suspicious estimates are also bimodal
print(suspicious_bimodal)
#8 of the 28 suspicious estimates were bimodal. 

# Now join with skewness and sample size info, just so we can compare 
suspicious_bimodal_with_skew <- suspicious_bimodal %>%
  left_join(skewness_suspicious, by = c("species" = "species", "grid" = "grid_id"))

# View result
print(suspicious_bimodal_with_skew)

# What I think we are seeing here: if the species counts in a grid are bimodial or skewed, or continuous 
# throughout the year, this can cause issues with how the Weibull distribution fits the data. This can 
# produce biologically impossible phenology estimates (exceeding 365 days of year). Here is a histogram 
# showing the distribution of all of the species that produce weird estimates in their grids, to show this:

obs_suspicious <- filtered_5 %>%
  mutate(day_of_year = lubridate::yday(eventDate)) %>%
  filter(day_of_year > 0) %>%
  semi_join(suspicious_species_grids, by = c("species" = "species", "grid_id" = "grid"))

# Create a combined label for faceting
obs_suspicious <- obs_suspicious %>%
  mutate(species_grid = paste0(species, " (Grid ", grid_id, ")"))

# Plot histograms faceted by species-grid
ggplot(obs_suspicious, aes(x = day_of_year)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  facet_wrap(~ species_grid, scales = "free_y") +
  labs(
    title = "Histogram of Observations by Day of Year for Suspicious Species-Grids",
    x = "Day of Year",
    y = "Observation Count"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



#############################################################################################################

#Save it: 
saveRDS(phenology_estimates_all_species_each_grid_with_GHMI, "Data/phenology_estimates_data_for_analysis.rds") 





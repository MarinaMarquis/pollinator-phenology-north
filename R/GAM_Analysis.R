# GAM for Pollinator Flight Period
# Brittany Mason
# June 3, 2025

########################################################################################################### 

# Load Packages 
library(readr)
library(tidyverse)
library(mgcv)
library(sf)
library(MuMIn)
set.seed(120)

# Read in data 
fp_data <- readRDS("Data/phenology_estimates_data_for_analysis.rds") #phenology estimate data
five_km_grids <- st_read("Data/Spatial Data/gridded map of NA24 region/NA24_gridded_map.geojson") #geoJSON of 
                                                                                                  #eco-region
                                                                                                  #NA24

########################################################################################################### 

# Now get the mean latitude and longitude for each grid
grids_centroids <- five_km_grids %>%
  st_centroid() %>%                                 
  mutate(lon = st_coordinates(.)[, 1],             
         lat = st_coordinates(.)[, 2]) %>%        
  st_drop_geometry() %>%                            
  select(grid_id, lon, lat) %>%
  group_by(grid_id) %>%
  summarise(lon = first(lon),
            lat = first(lat),
            .groups = "drop")

# Now add the mean lon and lat to our fp_data
fp_data <- left_join(fp_data, grids_centroids, by=c("grid"="grid_id"))%>%
  mutate(species = as.factor(species))

# Clean the data so we just have records with duration flight period
fp_data_duration <- fp_data %>%
  filter(complete.cases(duration)) %>%
  mutate(species = as.factor(species))

# Let's see the distribution of duration flight period
hist(fp_data_duration$duration)
# looks close to normal!

# Pull only relevant data for the models
fp_rel <- fp_data %>%
  dplyr::select(duration, onset, offset, mean_GHMI, lon, lat)  

# Lets get a summary of the data so we can see each value's distribution
summary(fp_rel)

# let's see if there is any multicolinearity
cor(fp_rel, method="pearson")
# I don't see anything too concerning here


########################################################################################################### 

# Explore data relationships ----------------------------------------------

# Now let's explore the distribution of each variable
fp_long <- fp_rel %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Plot histograms using facet_wrap
ggplot(fp_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  theme_classic() +
  labs(x = NULL, y = "Count", title = "Histograms of All Variables") +
  theme(strip.text = element_text(size = 10))
# since the response will determine the family that we add to the model, I am paying
# close attention to duration, onset, and offset. Onset and offset look close to 
# normally distributed, but duration looks positively skewed. We will keep that in mind
# when determining the modeling.


########################################################################################################### 

# GAM Model Testing -------------------------------------------------------

# We will do model testing on all data with species as a random effect, and
# on select species to see if the models pass the tests

# Let's find a species with a decent amount of data
fp_data_sp <- fp_data %>%
  group_by(species) %>%
  summarise(count=n()) %>%
  arrange(desc(count))%>%
  print(n=300)

# Bombus impatiens has the most data, so let's do that species for one
Bombus_impatiens <- fp_data %>%
  filter(species == "Bombus impatiens")

# Let's also do a species that has less data like Halictus ligatus
Halictus_ligatus <- fp_data %>% 
  filter(species == "Halictus ligatus")

## Duration -------------------------------------------------------

# Let's do some model testing to see which model will best fit the data
# we will start with modeling species as a random effect

# Start with a null model
gam_null <- gam(duration ~ 1 + s(lat, lon, k = 170, bs="tp") + s(species, bs="re"), 
                family = gaussian(),
                method = "REML",
                data=fp_data)
summary(gam_null)
gam.check(gam_null)


# Now add mean_GHMI
gam_1 <- gam(duration ~ mean_GHMI +
               s(lat, lon, k = 170, bs="tp") + 
               s(species, bs="re"), 
             family = gaussian(),
             method = "REML",
             data=fp_data)
summary(gam_1) #GHMI is a sig. predictor of duration (p=0.00876)
gam.check(gam_1)
gam.check(gam_1)$k.check
#The mean GHMI seems to explain very little deviance in the model, species and lat/long explain much 
#more variation

# Now let's see how they rank
AIC(gam_null)
AIC(gam_1)
# So in this case the null model performed better, so GHMI does not help explain duration

# Let's repeat for an individual species
gam_null_bi <- gam(duration ~ 1 + s(lat, lon, k = 10, bs="tp"), 
                   family = gaussian(),
                   method = "REML",
                   data=Bombus_impatiens)
summary(gam_null_bi)
gam.check(gam_null_bi)


# Now add mean_GHMI
gam_1_bi <- gam(duration ~ mean_GHMI +
                  s(lat, lon, bs="tp"), 
                family = gaussian(),
                method = "REML",
                data=Bombus_impatiens)
summary(gam_1_bi) #GHMI sig. predictor of duration (p=0.0306)
gam.check(gam_1_bi)
gam.check(gam_1_bi)$k.check
#The mean GHMI seems to explain very little deviance in the model

# Now let's see how they rank
# We will be using AICc due to the small sample size
AICc(gam_null_bi)
AICc(gam_1_bi)
#So in this case the adding GHMI improved model fit, but only slightly 

# Let's try one more time for the species with little data
gam_null_hl <- gam(duration ~ 1 + s(lat, lon, k=5, bs="tp"), 
                   family = gaussian(),
                   method = "REML",
                   data=Halictus_ligatus)
summary(gam_null_hl)
gam.check(gam_null_hl)
#In this case, we had to specify the k for lat/lon that was less than the degrees of freedom

# Now add mean_GHMI
gam_1_hl <- gam(duration ~ mean_GHMI +
                  s(lat, lon, k=5, bs="tp"), 
                family = gaussian(),
                method = "REML",
                data=Halictus_ligatus)
summary(gam_1_hl)
gam.check(gam_1_hl)
gam.check(gam_1_hl)$k.check
#The mean GHMI seems to explain very little deviance in the model

# Now let's see how they rank
AICc(gam_null_hl)
AICc(gam_1_hl)
# So in this case the null model performed better, so GHMI does not help explain duration

# What I have learned is that we will need to specify different k values for lat/lon based
# on the number of grids for the species and that GMHI is not likely to explain duration.






## Onset -------------------------------------------------------

# Let's take a closer look at the distribution of onset
hist(fp_data$onset)
#It looks normal 

# Start with modeling species as a random effect

# Start with a null model
gam_null_on <- gam(onset ~ 1 + s(lat, lon, k = 170, bs="tp") + s(species, bs="re"), 
                   family = Gamma(link = "log"),
                   method = "REML",
                   data=fp_data)
summary(gam_null_on)
gam.check(gam_null_on)
# I tried gaussian family at first, but the Gamma(link="log") has slightly better
# model fit so we will stick with that

# Now add mean_GHMI
gam_1_on <- gam(onset ~ mean_GHMI +
                  s(lat, lon, k = 170, bs="tp") + 
                  s(species, bs="re"), 
                family = Gamma(link = "log"),
                method = "REML",
                data=fp_data)
summary(gam_1_on)
gam.check(gam_1_on)
# The mean GHMI (not significant) seems to explain very little deviance in the model

# Now let's see how they rank
AIC(gam_null_on)
AIC(gam_1_on)
#In this case, adding GHMI does not better the fit of the model 

# Let's repeat for an individual species
gam_null_bi_on <- gam(onset ~ 1 + s(lat, lon, k = 10, bs="tp"), 
                      family = Gamma(link = "log"),
                      method = "REML",
                      data=Bombus_impatiens)
summary(gam_null_bi_on)
gam.check(gam_null_bi_on)


# Now add mean_GHMI
gam_1_bi_on <- gam(onset ~ mean_GHMI +
                     s(lat, lon, bs="tp"), 
                   family = Gamma(link = "log"),
                   method = "REML",
                   data=Bombus_impatiens)
summary(gam_1_bi_on) #GHMI not a sig. predictor of onset 
gam.check(gam_1_bi_on)
#While mean_GHMI is not significant, it does improve deviance explained

# Now let's see how they rank
AICc(gam_null_bi_on)
AICc(gam_1_bi_on)
#Adding GHMI decreases model fit 

# Let's try one more time for the species with little data
gam_null_hl_on <- gam(onset ~ 1 + s(lat, lon, k=5, bs="tp"), 
                      family = Gamma(link = "log"),,
                      method = "REML",
                      data=Halictus_ligatus)
summary(gam_null_hl_on)
gam.check(gam_null_hl_on)
#In this case, we had to specify the k for lat/lon that was less than the degrees of freedom

# Now add mean_GHMI
gam_1_hl_on <- gam(onset ~ mean_GHMI +
                     s(lat, lon, k=5, bs="tp"), 
                   family = Gamma(link = "log"),,
                   method = "REML",
                   data=Halictus_ligatus)
summary(gam_1_hl_on) #GHMI not a sig. predictor of onset 
gam.check(gam_1_hl_on)
#The GHMI does improve deviance explained in the model

# Now let's see how they rank
AICc(gam_null_hl_on)
AICc(gam_1_hl_on)
#The null model does better in this case








## Offset -------------------------------------------------------

# Let's take a closer look at the distribution of offset
hist(fp_data$offset)
#It looks normal, so guassian is probably going to be the best distribution

# Start with modeling species as a random effect

# Start with a null model
gam_null_off <- gam(offset ~ 1 + s(lat, lon, k = 170, bs="tp") + s(species, bs="re"), 
                    family = gaussian(),
                    method = "REML",
                    data=fp_data)
summary(gam_null_off)
gam.check(gam_null_off)


# Now add mean_GHMI
gam_1_off <- gam(offset ~ mean_GHMI +
                   s(lat, lon, k = 170, bs="tp") + 
                   s(species, bs="re"), 
                 family = gaussian(),
                 method = "REML",
                 data=fp_data)
summary(gam_1_off) #GHMI is a sig. predictor of offset
gam.check(gam_1_off)

# Now let's see how they rank
AIC(gam_null_off)
AIC(gam_1_off)
#Adding GHMI improves model fit 

# Let's repeat for an individual species
gam_null_bi_off <- gam(offset ~ 1 + s(lat, lon, k = 10, bs="tp"), 
                       family = gaussian(),
                       method = "REML",
                       data=Bombus_impatiens)
summary(gam_null_bi_off)
gam.check(gam_null_bi_off)


# Now add mean_GHMI
gam_1_bi_off <- gam(offset ~ mean_GHMI +
                      s(lat, lon, bs="tp"), 
                    family = gaussian(),
                    method = "REML",
                    data=Bombus_impatiens)
summary(gam_1_bi_off) #GHMI is a sig. predictor of onset 
gam.check(gam_1_bi_off)

# Now let's see how they rank
AICc(gam_null_bi_off)
AICc(gam_1_bi_off)
#GHMI does improve the model 

# Let's try one more time for the species with little data
gam_null_hl_off <- gam(offset ~ 1 + s(lat, lon, k=5, bs="tp"), 
                       family = gaussian(),
                       method = "REML",
                       data=Halictus_ligatus)
summary(gam_null_hl_off)
gam.check(gam_null_hl_off)
#In this case, we had to specify the k for lat/lon that was less than the degrees of freedom

# Now add mean_GHMI
gam_1_hl_off <- gam(offset ~ mean_GHMI +
                      s(lat, lon, k=5, bs="tp"), 
                    family = gaussian(),
                    method = "REML",
                    data=Halictus_ligatus)
summary(gam_1_hl_off) #GHMI is not a sig. predictor of onset 
gam.check(gam_1_hl_off)
# The GHMI does improve deviance explained in the model. Model seems to be overfitted. 

# Now let's see how they rank
AICc(gam_null_hl_off)
AICc(gam_1_hl_off)
#The null model does better in this case 





########################################################################################################### 
################# GAM models by species ---------------------------------------------------

# With all the knowledge from model testing, we are ready to create a function to 
# examine GAM models by species

# For each species, we will run 6 GAM models for duration, onset, and offset, where
# we have one NULL model and one model with GMHI. Then we will calculate the model outputs
# and the model weight of models of interest when compared to NULL models

gam_by_species <- function(species_name){
  
  # select the species
  species_interest <- species_name
  
  # filter the fp_data to that species
  fp_data_sp <- fp_data %>%
    filter(species == species_interest)
  
  # define the k value based on the number of data points we have
  k_val <- ifelse(nrow(fp_data_sp) <= 20, nrow(fp_data_sp)-1, 20)
  
  ### duration ###
  gam_null_dur <- gam(duration ~ 1 + s(lat, lon, k = k_val, bs="tp"), 
                      family = gaussian(),
                      method = "REML",
                      data=fp_data_sp)
  sum_gam_null_dur <- summary(gam_null_dur)
  
  gam_ghmi_dur <- gam(duration ~ mean_GHMI + s(lat, lon, k = k_val, bs="tp"), 
                      family = gaussian(),
                      method = "REML",
                      data=fp_data_sp)
  sum_gam_ghmi_dur <- summary(gam_ghmi_dur)
  pval_spatial_dur <- sum_gam_ghmi_dur$s.table["s(lat,lon)", "p-value"] # spatial smooth significance 
  
  # get model weights
  aic_val_dur <- c(AICc(gam_null_dur), AICc(gam_ghmi_dur))
  delta_aic_dur <- aic_val_dur - min(aic_val_dur)
  weights_dur <- exp(-0.5 * delta_aic_dur) / sum(exp(-0.5 * delta_aic_dur))
  
  
  ### onset ###
  gam_null_on <- gam(onset ~ 1 + s(lat, lon, k = k_val, bs="tp"), 
                     family = Gamma(link = "log"),
                     method = "REML",
                     data=fp_data_sp)
  sum_gam_null_on <- summary(gam_null_on)
  
  
  gam_ghmi_on <- gam(onset ~ mean_GHMI + s(lat, lon, k = k_val, bs="tp"), 
                     family = Gamma(link = "log"),
                     method = "REML",
                     data=fp_data_sp)
  sum_gam_ghmi_on <- summary(gam_ghmi_on)
  pval_spatial_on <- sum_gam_ghmi_on$s.table["s(lat,lon)", "p-value"] #spatial smooth significance 
  
  # get model weights
  aic_val_on <- c(AICc(gam_null_on), AICc(gam_ghmi_on))
  delta_aic_on <- aic_val_on - min(aic_val_on)
  weights_on <- exp(-0.5 * delta_aic_on) / sum(exp(-0.5 * delta_aic_on))
  
  ### offset ###
  gam_null_off <- gam(offset ~ 1 + s(lat, lon, k = k_val, bs="tp"), 
                      family = gaussian(),
                      method = "REML",
                      data=fp_data_sp)
  sum_gam_null_off <- summary(gam_null_off)
  
  gam_ghmi_off <- gam(offset ~ mean_GHMI + s(lat, lon, k = k_val, bs="tp"), 
                      family = gaussian(),
                      method = "REML",
                      data=fp_data_sp)
  sum_gam_ghmi_off <- summary(gam_ghmi_off)
  pval_spatial_off <- sum_gam_ghmi_off$s.table["s(lat,lon)", "p-value"] #spatial smooth significance 
  
  # get model weights
  aic_val_off <- c(AICc(gam_null_off), AICc(gam_ghmi_off))
  delta_aic_off <- aic_val_off - min(aic_val_off)
  weights_off <- exp(-0.5 * delta_aic_off) / sum(exp(-0.5 * delta_aic_off))
  
  # now create a table with outputs for the three response variables
  gam_table <- data.frame(species = c(species_interest, species_interest, species_interest),
                          model=c("duration", "onset", "offset"),
                          # linear coefficient estimate for GHMI
                          GHMI_estimate = c(sum_gam_ghmi_dur$p.table["mean_GHMI", "Estimate"],
                                            sum_gam_ghmi_on$p.table["mean_GHMI", "Estimate"],
                                            sum_gam_ghmi_off$p.table["mean_GHMI", "Estimate"]),
                          # standard error for GHMI
                          GHMI_se = c(sum_gam_ghmi_dur$p.table["mean_GHMI", "Std. Error"],
                                      sum_gam_ghmi_on$p.table["mean_GHMI", "Std. Error"],
                                      sum_gam_ghmi_off$p.table["mean_GHMI", "Std. Error"]),
                          # t value for GHMI
                          GHMI_tval = c(sum_gam_ghmi_dur$p.table["mean_GHMI", "t value"],
                                        sum_gam_ghmi_on$p.table["mean_GHMI", "t value"],
                                        sum_gam_ghmi_off$p.table["mean_GHMI", "t value"]),
                          # p-value for GHMI
                          GHMI_pval = c(sum_gam_ghmi_dur$p.table["mean_GHMI", "Pr(>|t|)"],
                                        sum_gam_ghmi_on$p.table["mean_GHMI", "Pr(>|t|)"],
                                        sum_gam_ghmi_off$p.table["mean_GHMI", "Pr(>|t|)"]),
                          # adjusted R2 for the model
                          adj_r2 = c(sum_gam_ghmi_dur$r.sq,
                                     sum_gam_ghmi_on$r.sq,
                                     sum_gam_ghmi_off$r.sq),
                          # deviance explained for the model
                          dev_exp = c(sum_gam_ghmi_dur$dev.expl,
                                      sum_gam_ghmi_on$dev.expl,
                                      sum_gam_ghmi_off$dev.expl),
                          # difference in deviance explained ghmi comp to null model
                          dev_exp_diff_comp_null = c(sum_gam_ghmi_dur$dev.expl-sum_gam_null_dur$dev.expl,
                                                     sum_gam_ghmi_on$dev.expl-sum_gam_null_on$dev.expl,
                                                     sum_gam_ghmi_off$dev.expl-sum_gam_null_off$dev.expl),
                          # sample size that was used in the model
                          sample_size = c(sum_gam_ghmi_dur$n,
                                          sum_gam_ghmi_on$n,
                                          sum_gam_ghmi_off$n),
                          model_weight_comp_null = c(weights_dur[2],
                                                     weights_on[2],
                                                     weights_off[2]),
                          spatial_pval = c(pval_spatial_dur, pval_spatial_on, pval_spatial_off)
  )
  
  return(list(
    summary_table = gam_table,
    models = list(
      duration = gam_ghmi_dur,
      onset = gam_ghmi_on,
      offset = gam_ghmi_off
    )
  ))
}

# get list of species 
count_sp <- fp_data %>%
  group_by(species) %>%
  summarise(count=n()) %>%
  arrange(desc(count))
species_list <- as.vector(count_sp[!count_sp$count<6,]$species)

# now use the function to get model outputs for all species
species_gam_full <- setNames(lapply(species_list, gam_by_species), species_list)

# Extract summary tables into a single dataframe
species_gam <- bind_rows(lapply(species_gam_full, function(x) x$summary_table))

# Save it 
write_csv(species_gam, "Data/GAM_results/gam_results_by_species.csv")











########################################################################################################### 
############################## Interpreting Model Outputs: 
### Filter for significant p-value results, to compare: 
species_gam_significant <- species_gam %>%
  filter(GHMI_pval < 0.05,
         model_weight_comp_null > 0.75,
         adj_r2 > 0)
unique(species_gam_significant$species) #44 species sig. 
unique(species_gam_significant$species[species_gam_significant$model=="onset"]) #19 sig. for onset 
unique(species_gam_significant$species[species_gam_significant$model=="offset"]) #23 sig. for offset
unique(species_gam_significant$species[species_gam_significant$model=="duration"]) #10 sig. for duration 


# =  species only onset 
#  = species duration only 
#  = 3 species offset only 

#  =  species onset and duration
#  = species offset and duration only 






### Looking at the geographic location (lat/long) smoother: 

# Count models per species by spatial effect
spatial_summary <- species_gam_significant %>%
  group_by(species) %>%
  summarize(
    models_with_spatial_effect = sum(spatial_pval < 0.05),
    models_without_spatial_effect = sum(spatial_pval >= 0.05),
    total_models = n()
  ) %>%
  arrange(desc(models_with_spatial_effect))

# Species where lat/long is not sig.
species_no_spatial_effect <- spatial_summary %>%
  filter(models_with_spatial_effect == 0) %>%
  pull(species)

print(species_no_spatial_effect)

#For the following 18 species, spatial location is not influencing phenology beyond what GHMI explains:
# "Acronicta americana", "Argyrotaenia velutinana", "Bombus impatiens", "Ceratomia catalpae",    
#"Eacles imperialis", "Epargyreus clarus", "Eremnophila aureonotata", "Eubaphe mendica",        
#"Helicoverpa zea", "Hyphantria cunea", "Hypsoropha hormos", "Macaria pustularia",     
#"Orgyia leucostigma", "Palthis angulalis", "Papilio troilus", "Phyciodes tharos",       
#"Protoboarmia porcelaria", "Pyrrharctia isabella"

#Species where lat/long is sig.
species_with_spatial_effect <- spatial_summary %>%
  filter(models_with_spatial_effect > 0) %>%
  pull(species)
print(species_with_spatial_effect)

# For the following 26 species, at lat/long is sig. influencing at least one phenology variables 
# beyond what GHMI explains: "Clogmia albipunctatus", "Malacosoma americana", "Xylocopa virginica", 
#"Actias luna", "Antheraea polyphemus", "Apis mellifera", "Asterocampa celtis", "Battus philenor",         
#"Camponotus chromaiodes", "Danaus plexippus", "Eristalis tenax", "Euclea delphinii",        
#"Eudryas grata", "Hylephila phyleus", "Hypoprepia fucosa", "Limenitis arthemis",      
#"Limenitis astyanax",  "Malacosoma disstria" , "Noctua pronuba", "Papilio glaucus",          
#"Phigalia strigataria", "Popillia japonica" , "Spodoptera ornithogalli", "Tetanolita mynesalis",     
#"Tetraopes tetrophthalmus", "Vespula squamosa"  


# Looking at which models have spatial significance for each species: 
sig_spatial_models <- species_gam_significant %>%
  filter(species %in% species_with_spatial_effect, spatial_pval < 0.05) %>%
  select(species, model, spatial_pval) %>%
  arrange(species, model)

sig_spatial_summary <- sig_spatial_models %>%
  group_by(species) %>%
  summarize(significant_models = paste(model, collapse = ", "))

print(sig_spatial_summary, n=26)

# For the following  13 species, at lat/long is sig. influencing offset beyond what GHMI explains: Antheraea polyphemus, 
# Asterocampa celtis, Battus philenor, Danaus plexippus, Euclea delphinii, Hylephila phyleus, Limenitis arthemis, 
# Malacosoma americana, Noctua pronuba, Papilio glaucus, Phigalia strigataria , Spodoptera ornithogalli, 
# Tetraopes tetrophthalmus

# For the following species, at lat/long is sig. influencing onset beyond what GHMI explains: Camponotus chromaiodes, 
#Clogmia albipunctatus, Eristalis tenax, Eudryas grata, Malacosoma americana, Malacosoma disstria , 
#Popillia japonica, Tetanolita mynesalis, Vespula squamosa, Xylocopa virginica

# For the following species, at lat/long is sig. influencing duration beyond what GHMI explains:  







### Looking at the effect of GHMI on flight period, and the direction of this effect 
effects <- species_gam_significant %>%
  select(species, model, GHMI_estimate)
print(effects)

# Example: Danaus plexippus, offset decreases by ~86 days for every 1-unit increase in GHMI. This means 
#that offset occurs ~86 days earlier in fully anthropogenized areas compared to natural areas 

#Look at GHMI effect for onset, offset, and duration
effects_onset <- effects %>%
  filter(model=="onset")%>%
  arrange(desc(GHMI_estimate))
print(effects_onset)

effects_offset <- effects %>%
  filter(model=="offset")%>%
  arrange(desc(GHMI_estimate))
print(effects_offset)

effects_duration <- effects %>%
  filter(model=="duration")%>%
  arrange(desc(GHMI_estimate))
print(effects_duration)


# Do a count of the effects and their direction 
interpret_ghmi_effect <- function(df) {
  df$GHMI_effect_interpretation <- NA_character_
  
  for (i in seq_len(nrow(df))) {
    est <- df$GHMI_estimate[i]
    model <- df$model[i]
    species <- df$species[i]
    
    if (model == "onset") {
      # For onset (Gamma + log link), exponentiate estimate to get multiplicative change
      perc_change <- (exp(est) - 1) * 100
      direction <- ifelse(perc_change < 0, "earlier", "later")
      perc_change_abs <- abs(round(perc_change, 1))
      interp <- paste0(
        species, ": Onset occurs about ", perc_change_abs, "% ", direction, " in fully urban vs natural areas."
      )
    } else {
      # For offset and duration (Gaussian), interpret estimate as days difference
      days_diff <- round(est, 1)
      direction <- ifelse(days_diff < 0, "earlier/shorter", "later/longer")
      days_diff_abs <- abs(days_diff)
      interp <- paste0(
        species, ": ", model, " is about ", days_diff_abs, " days ", direction, " in fully urban vs natural areas."
      )
    }
    
    df$GHMI_effect_interpretation[i] <- interp
  }
  
  return(df)
}



count_direction <- function(df) {
  # Create a helper column for direction based on estimate sign
  df$direction <- ifelse(df$GHMI_estimate < 0, "earlier_or_shorter", "later_or_longer")
  
  # Count how many species by model and direction
  summary_table <- table(df$model, df$direction)
  
  return(summary_table)
}

direction_counts <- count_direction(species_gam_significant)
print(direction_counts)










### Looking at model fit using adj R^2 and dev_exp. Since offset and duration are Gaussian, we'll use 
#adj R squared in addition to dev_exp, as a second check

species_gam_significant %>%
  group_by(model) %>%
  summarise(
    min_r2 = min(adj_r2, na.rm = TRUE),
    max_r2 = max(adj_r2, na.rm = TRUE),
    mean_r2 = mean(adj_r2, na.rm = TRUE),
    min_dev = min(dev_exp, na.rm = TRUE),
    max_dev = max(dev_exp, na.rm = TRUE),
    mean_dev = mean(dev_exp, na.rm = TRUE)
  )

#Offset models, on average, have slightly better R² and deviance explained than onset or duration.
#However, these models still have a large range of deviance explained (lots of variation). 


# Deviance and adjusted r^2 explained per model, with species listed 
species_gam_significant %>%
  group_by(model) %>%
  arrange(model, desc(dev_exp)) %>%
  select(model, species, dev_exp, adj_r2)%>%
  print(n=21)

# Now compare this list of models with the models that have sig. of the spatial smooth (lat/long): 
print(sig_spatial_summary)
#It looks like the onset models that do show spatial significance (Polites otho, Eurema daira, Phoebis sennae)
#are also the strongest fitting models based on deviance explained and adjusted R² 
#(all >0.45 deviance and adj R²)






### Now visualizing the significant models: 


# Separate species into what phenology variable GHMI sig. predicts, based on GAM models: 
onset_species <- c("Erynnis horatius", "Hylephila phyleus", "Phoebis sennae",
                   "Eurema daira", "Bombus pensylvanicus", "Polites otho")

offset_species <- c("Danaus plexippus", "Heliconius charithonia", "Eumaeus atala",
                    "Phyciodes tharos", "Syngamia florella", "Calpodes ethlius")

duration_species <- c("Heliconius charithonia", "Eumaeus atala", "Phoebis sennae",
                      "Polites vibex", "Papilio glaucus", "Polites otho",
                      "Phyciodes tharos", "Euglossa dilemma", "Trigonopeltastes delta")





# Build plot function
plot_model_group <- function(species_vec, model_name, ncol = 2) {
  plots <- lapply(species_vec, function(sp) {
    gam_obj <- species_gam_full[[sp]]$models[[model_name]]
    
    # Build a data frame over a range of GHMI values
    ghmi_vals <- tibble(
      mean_GHMI = seq(0, 1, length.out = 100),
      lat = mean(gam_obj$model$lat, na.rm = TRUE),  # use average lat/lon for other predictors
      lon = mean(gam_obj$model$lon, na.rm = TRUE)
    )
    
    # Predict partial effect
    preds <- predict(gam_obj, newdata = ghmi_vals, type = "link", se.fit = TRUE)
    
    plot_df <- ghmi_vals %>%
      mutate(
        fit = preds$fit,
        se = preds$se.fit,
        upper = fit + 2 * se,
        lower = fit - 2 * se
      )
    
    # Plot manually
    ggplot(plot_df, aes(x = mean_GHMI, y = fit)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
      labs(title = sp,
           subtitle = paste0("Effect of Urbanization on ", model_name),
           x = "GHMI (Urbanization Index)",
           y = "Partial Effect (link scale)") +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5),
        axis.title = element_text(size = 9)
      )
  })
  
  wrap_plots(plots, ncol = ncol)
}




plot_model_group(onset_species, "onset")
plot_model_group(offset_species, "offset")
plot_model_group(duration_species, "duration")


# Save the plots 
ggsave("Figures/GAM_onset_species.png", plot_model_group(onset_species, "onset"), 
       width = 8, height = 8, dpi = 300)

ggsave("Figures/GAM_offset_species.png", plot_model_group(offset_species, "offset"), 
       width = 8, height = 8, dpi = 300)

ggsave("Figures/GAM_duration_species.png", plot_model_group(duration_species, "duration"), 
       width = 9, height = 10, dpi = 300)






# Plotting spatial smooths to look at whether there is spatial significance for each of the 16 species:

# Create vectors for each model type
sig_spatial_onset <- sig_spatial_models %>% filter(model == "onset")
sig_spatial_offset <- sig_spatial_models %>% filter(model == "offset")
sig_spatial_duration <- sig_spatial_models %>% filter(model == "duration")

# Plot function 
plot_spatial_model_group <- function(df, ncol = 2) {
  plots <- lapply(seq_len(nrow(df)), function(i) {
    sp <- df$species[i]
    mod <- df$model[i]
    
    gam_obj <- species_gam_full[[sp]]$models[[mod]]
    
    smooth_labels <- gratia::smooths(gam_obj)
    spatial_index <- which(smooth_labels == "s(lat,lon)")
    
    if (length(spatial_index) == 0) {
      warning(paste("No spatial smooth for", sp, mod))
      return(NULL)
    }
    
    draw(gam_obj, select = spatial_index, residuals = FALSE, contour = TRUE) +
      labs(title = sp,
           subtitle = paste("Spatial Effect:", mod),
           fill = "Partial Effect") +
      scale_fill_viridis_c(option = "plasma") +  # optional: cleaner color scale
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5),
        legend.position = "right"
      )
  })
  
  wrap_plots(plots[lengths(plots) > 0], ncol = ncol)
}


# Plot them 
plot_spatial_model_group(sig_spatial_onset, ncol = 2)
plot_spatial_model_group(sig_spatial_offset, ncol = 2)
plot_spatial_model_group(sig_spatial_duration, ncol = 2)


#These plots show the partial effect of latitude and longitude on a phenological metric 
#(onset, offset, or duration), after accounting for GHMI (urbanization).The colors (partial effects)
#represent the relative effect of space (lat/long) on the response variable (onset, offset, or duration). 
# Cool colors (e.g., blue) might indicate regions where the flight period is earlier or is 
#shorter than average. Warm colors (e.g., yellow/red) indicate later or longer values.The contour 
#lines show where the spatial effect is constant — like elevation on a topographic map.Tight contours =
#spatial effect changes rapidly (i.e., localized hot/cold spots).Broad smooth gradients = gradual 
#spatial variation.



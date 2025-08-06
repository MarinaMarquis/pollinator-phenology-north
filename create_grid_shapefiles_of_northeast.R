### Marina Marquis
### 8/6/25
### In this file, I clip the ecoregions geoJSON file so that I have a geoJSON file of just eco-region NA24. 
#   I then create a bounding box of this region to filter the GBIF pollinator occurrence data to this general
#   area. Lastly, I create a gridded map of eco-region NA24, with each grid being 5 x 5 km. 



###############################################################################################################

# Read in packages 
library(sf)
library(tidyverse)

# Read in data 
ecoregions <- st_read("Data/Spatial Data/ecoregion geojson/one_earth-bioregions-2023.geojson") # ecoregion geoJSON

###############################################################################################################


# Plot ecoregions just to see how it looks 
plot(ecoregions)

# Filter to only ecoregion NA24 
NA_24 <- ecoregions %>%
  dplyr::filter(Bioregions == "NA24")

# Look at it 
plot(NA_24)

# Save the NA_24 polygon as a GeoJSON file
st_write(NA_24, "Data/Spatial Data/ecoregion geojson/NA_24_clipped.geojson", driver = "GeoJSON", delete_dsn = TRUE)

# Create a bbox of the clipped geojson so that we can use it to extract insect pollinator occurrence data 
# from GBIF in the general area 
bbox <- st_bbox(NA_24)
print(bbox)
bbox_wkt <- sprintf(
  "POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
  bbox["xmin"], bbox["ymin"],  # bottom-left
  bbox["xmin"], bbox["ymax"],  # top-left
  bbox["xmax"], bbox["ymax"],  # top-right
  bbox["xmax"], bbox["ymin"],  # bottom-right
  bbox["xmin"], bbox["ymin"]   # close polygon (bottom-left again)
)

cat(bbox_wkt)

###############################################################################################################

### Create 5 km grids across the entire clipped geoJSON of eco-region NA24




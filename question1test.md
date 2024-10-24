 #code test 
 #temporary code
library(tidyverse)
#remotes::install_github("propertypricebn/bruneimap")
library(bruneimap)
library(ggrepel)
library(kernlab)
library(osrm)
library(osmdata)
library (osmdata)
library(readxl)
packageVersion ("osmdata")

h <- 
  opq("brunei") %>%
  add_osm_feature(
    key = "amenity",
    value = "hospital"
  ) %>%
  osmdata_sf()
glimpse(h$osm_polygons)

hospitalsh_sf <-
  h$osm_polygons |>
  as_tibble() |>  # these two lines convert to tibble-like object
  st_as_sf() |> 
  select(osm_id, name, amenity) |>
  st_centroid()  # obtains X,Y coordinates of centroids
hospitalsh_sf <- hospitalsh_sf %>%
  filter(!is.na(amenity))

glimpse(hospitalsh_sf)
view(hospitalsh_sf)

# Read Excel file
hospitalexcel <- read_excel("excel coordinates.xlsx")

# Check for NAs
na_latitude <- sum(is.na(hospitalexcel$Latitude))
na_longitude <- sum(is.na(hospitalexcel$Longitude))

if (na_latitude > 0 | na_longitude > 0) {
  print(paste("NAs found in Latitude:", na_latitude, "Longitude:", na_longitude))
}

# Convert to numeric if necessary
hospitalexcel$Latitude <- as.numeric(as.character(hospitalexcel$Latitude))
hospitalexcel$Longitude <- as.numeric(as.character(hospitalexcel$Longitude))

# Remove rows with NAs in Latitude and Longitude
hospitalexcel <- hospitalexcel %>% drop_na(Latitude, Longitude)
view(hospitalexcel)
# Convert to sf object
hpex <- st_as_sf(hospitalexcel, coords = c("Longitude", "Latitude"), crs = 4326, remove = TRUE) 
view(hpex)



plotm <- ggplot() +
  geom_sf(data = mkm_sf, aes(fill = district), alpha = 0.3) +
  geom_sf(data = st_filter(hospitalsh_sf, mkm_sf), size = 2, color = "black") +  # Existing hospitals
  geom_sf(data = st_filter(hpex, mkm_sf), size = 2, color = "black") + #Hosptials from Excel
  labs(title = "Hospitals around Brunei",
       fill = "District")
plotm

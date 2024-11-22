# Spatial data analysis of Brunei data

## Mosques coverage in Brunei Darussalam

library(tidyverse)
#remotes::install_github("propertypricebn/bruneimap")
library(ggrepel)
library(kernlab)
library(osrm)
library(osmdata)

### Brunei map with district outline

ggplot(dis_sf) +
  geom_sf()

### Bounding box for Brunei Muara

dis_bbox <- st_bbox(dis_sf)

z <-
  opq(dis_bbox) |>
  add_osm_feature(
    key = c("amenity", "religion") ,
    value = c("place_of_worship" , "muslim")
  ) |>
  osmdata_sf()

mosques_sf1 <-
  z$osm_polygons |>
  as_tibble() |>  # these two lines convert to tibble-like object
  st_as_sf() |> 
  select(osm_id, name) |>
  drop_na() |>
  st_centroid()  # obtains X,Y coordinates of centroids
print(mosques_sf1)

### slice unwanted rows 

mosques_sf2 <- slice(.data = mosques_sf1 , -c(18,31,32,34,35,58,83,84,91,99,100)) %>%
  
### Plotting mosques on Brunei map

ggplot() +
  geom_sf(data = dis_sf, aes(fill = name), alpha = 0.3) +
  geom_sf(data = mosques_sf2, size = 1.5)
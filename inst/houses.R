library(bruneimap)
library(osmdata)
library(tidyverse)
theme_set(theme_void())

# Query road network data for Brunei
houses <-
  opq(bbox = "brunei") |>
  add_osm_feature(
    key = "building",
    value = c("residential", "house", "apartments", "detached", "terrace", "bungalow", "semidetached_house", "stilt_house")
  ) |>
  osmdata_sf()

houses_poly <- st_intersection(houses$osm_polygons, brn_sf)
houses_pts  <- st_intersection(houses$osm_points, brn_sf)

ggplot(houses_pts) +
  geom_sf(data = mkm_sf) +
  geom_sf(data = bruneimap::fr_sf, fill = "forestgreen", col = NA) +
  geom_sf(size = 0.1, alpha = 0.1)

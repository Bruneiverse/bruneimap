library(bruneimap)
library(osmdata)
library(tidyverse)
theme_set(theme_void())

bb <- getbb("Brunei")

# Query road network data for the area
road_data <-
  opq(bbox = bb) |>
  add_osm_feature(key = "highway") |>
  osmdata_sf()

# Access the road lines as an `sf` object
road_lines <-
  as_tibble(road_data$osm_lines) |>
  st_set_geometry(road_data$osm_lines$geometry)

# Trim the data set
road_lines_filtered <- road_lines |>
  dplyr::select(
    osm_id, name, highway, access, lanes, oneway, surface, maxspeed, bicycle,
    foot, motor_vehicle, bridge, tunnel, geometry
  )

road_lines_filtered |>
  filter(grepl("Jalan Tungku", name)) |>
  ggplot() +
  # geom_sf(data = brn_sf) +
  geom_sf(aes(col = bridge))

# Brunei roads
brn_rd <-
  road_lines_filtered |>
  filter(grepl("motorway|trunk|primary|secondary|tertiary|unclassified|residential", highway)) |>
  st_intersection(brn_sf) |>
  st_union(filter(road_lines_filtered, grepl("Jambatan Temburong", name)))

# Gadong Roads
gadonga <-
  kpg_sf |>
  filter(grepl("Gadong A", mukim))

total <- summarise(gadonga)

gadonga_rd <- st_intersection(road_lines, gadonga)

ggplot() +
  geom_sf(data = kpg_sf, aes(fill = mukim), alpha = 0.5) +
  geom_sf(data = road_lines |> filter(grepl("Jambatan Temburong", name))) +
  scale_fill_viridis_d(option = "turbo", name = NULL)

ggplot() +
  geom_sf(data = kpg_sf, aes(fill = mukim), col = NA, alpha = 0.5) +
  geom_sf(data = brn_rd) +
  scale_fill_viridis_d(option = "turbo", name = NULL) +
  theme(legend.position = "none")

ggplot() +
  geom_sf(data = gadonga, alpha = 0.5, linetype = "solid") +
  geom_sf(data = gadonga_rd |>
            filter(grepl("motorway|trunk|primary|secondary|tertiary|unclassified|residential", highway)),
          aes(col = as.numeric(maxspeed))) +
  scale_colour_viridis_c(name = "Speed\nLimit")
  scale_color_viridis_d(option = "turbo", name = NULL)

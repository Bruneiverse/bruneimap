library(bruneimap)
library(osmdata)
library(MetricGraph)
library(tidyverse)
theme_set(theme_void())

# Query road network data for Brunei
road_data <-
  opq(bbox = "brunei") |>
  add_osm_feature(key = "highway") |>
  osmdata_sf()

# Access the road lines as an `sf` object
road_lines <-
  as_tibble(road_data$osm_lines) |>
  st_set_geometry(road_data$osm_lines$geometry) |>
  janitor::clean_names()

# Trim to ensure only roads in Brunei are included
brn_rd <-
  filter(road_lines, !grepl("Jambatan Temburong", name)) |>
  st_intersection(brn_sf$geometry) |>
  bind_rows(filter(road_lines, grepl("Jambatan Temburong", name)))

# Add to district, mukims, and kampongs
dis_rd <-
  brn_rd |>
  st_intersection(select(dis_sf, bm_id = id, district, geometry)) |>
  bind_rows(filter(brn_rd, grepl("Jambatan Temburong", name)))

mkm_rd <-
  brn_rd |>
  st_intersection(select(mkm_sf, id, mukim, district, geometry)) |>
  bind_rows(filter(brn_rd, grepl("Jambatan Temburong", name)))

kpg_rd <-
  brn_rd |>
  st_intersection(select(kpg_sf, id, kampong, mukim, district, geometry)) |>
  bind_rows(filter(brn_rd, grepl("Jambatan Temburong", name)))

place <- "Kiarong"
ggplot() +
  geom_sf(data = filter(kpg_sf, grepl(!!place, kampong))) +
  geom_sf(data = filter(kpg_rd, grepl(!!place, kampong)))

pl <-
  map(
    unique(filter(kpg_sf, grepl(!!place, mukim))$kampong),
    \(x) {
      ggplot() +
        geom_sf(data = filter(kpg_sf, kampong == x)) +
        geom_sf(data = filter(kpg_rd, kampong == x)) +
        ggtitle(x)
  })
cowplot::plot_grid(plotlist = pl)

## ----- Mukim Berakas A and distance to masjids -------------------------------
place <- "Berakas A"
ggplot() +
  geom_sf(data = filter(mkm_sf, grepl(!!place, mukim))) +
  geom_sf(
    data = mkm_rd |>
      filter(grepl(!!place, mukim)) |>
      filter(grepl("motorway|trunk|primary|secondary|tertiary|unclassified|residential", highway))
  ) +
  geom_point(
    data = filter(masjid, grepl(!!place, mukim)),
    aes(latitude, longitude),
    col = "red3"
  )

place <- "Berakas A"
data <- kpg_rd |>
  filter(grepl(!!place, mukim)) |>
  filter(grepl("motorway|trunk|primary|secondary|tertiary|unclassified|residential", highway)) |>
  st_cast("LINESTRING") |>
  filter(st_geometry_type(geometry) == "LINESTRING" &
           sapply(st_geometry(geometry), function(geom) nrow(st_coordinates(geom))) >= 2)
graph <- metric_graph$new(data, merge_close_vertices = FALSE)
graph$plot(vertex_size = 0.5)


graph$build_mesh(h = 100 / 1000)  # 100m mesh
graph$plot(mesh = TRUE)


# locations of masjids in Berakas A
mb <- filter(masjid, grepl("Berakas A", mukim)) |> drop_na(latitude, longitude)

mesh <- tibble(
  lon = graph$mesh$V[, 1],
  lat = graph$mesh$V[, 2]
)
mesh_list <-
  mesh |>
  mutate(group = ceiling(row_number() / 100)) |>
  group_split(group) |>
  lapply(\(x) select(x, -group))
mesh_dist <- map(
  mesh_list,
  \(x) {
    out <- osrmTable(
      src = x,
      dst = select(mb, lon = latitude, lat = longitude), #|> column_to_rownames("name")
      measure = "distance"
    )
    out$distances
  }
)
mesh_dist <- as_tibble(do.call(rbind, mesh_dist))
mesh_dist <-
  mesh_dist |>
  rowwise() |>
  mutate(d = min(c_across(everything()), na.rm = TRUE)) |>
  ungroup()

d <- exp(- 0.5 * mesh_dist$d / 1000)

p <- ggplot(data = filter(mkm_sf, grepl(!!place, mukim))) + geom_sf()

graph$plot_function(
  X = d,
  vertex_size = 0,
  edge_width = 0.6,
  scale_color = ggplot2::scale_color_viridis_c(option = "magma")
) +
  geom_sf(data = filter(kpg_sf, grepl(!!place, mukim)), fill = NA) +
  geom_point(data = mb, aes(latitude, longitude), col = "black", size = 2) +
  # geom_emoji(data = mb, aes(latitude, longitude), size = 0.03, emoji = "1f54c") +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Distance to Masjids in Berakas A")

# select(
#   osm_id, name, highway, access, lanes, oneway, surface, maxspeed, bicycle,
#   foot, motor_vehicle, bridge, tunnel, geometry
# )
#
# filter(grepl("motorway|trunk|primary|secondary|tertiary|unclassified|residential",
#              highway)) |>

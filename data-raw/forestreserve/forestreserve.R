library(tidyverse)
library(bruneimap)
theme_set(theme_bw())
library(sf)
library(ggrepel)

# Create map from shapefile
total_area <- st_area(st_union(bruneimap::dis_sf))
fr_sf <-
  st_read(here::here("data-raw/forestreserve/data/fr_map.shp")) |>
  mutate(
    area = st_area(st_make_valid(geometry)),
    prop = as.numeric(area / total_area),
    category = case_when(
      FR_N == "Forest Plantation Area" ~ "Plantation Area",
      grepl("Ext.", FR_N) ~ "Proposed",
      grepl("Belait Peat Swamp", FR_N) ~ "Proposed",
      TRUE ~ "Gazetted"
    ),
    FR_N = gsub("Fr.", "FR.", FR_N),
  ) |>
  as_tibble() |>
  st_as_sf() |>
  select(id, name = FR_N, category, area, prop, geometry)

# Check fr_sf
fr_sf |>
  dplyr::filter(category != "Plantation Area") |>
  mutate(name = gsub(" FR.", "", name)) |>
  ggplot() +
  geom_sf(aes(fill = category), col = "black", alpha = 0.7) +
  geom_sf(data = dis_sf, fill = NA, col = "black", linewidth = 1) +
  geom_label_repel(
    aes(label = name, geometry = geometry),
    stat = "sf_coordinates",
    box.padding = 1,
    point.padding = 0.5,
    min.segment.length = unit(0, 'lines'),
    size = 3,
    segment.curvature = -0.2,    # Adds curvature to the line
    segment.angle = 20,          # Adjusts angle of the line
    segment.length = unit(2, "lines")  # Increases line length
  ) +
  scale_fill_manual(values = c("forestgreen", "limegreen")) +
  labs(x = NULL, y = NULL)

# Export data
usethis::use_data(fr_sf, overwrite = TRUE, compress = "xz")

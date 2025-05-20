library(tidyverse)
library(bruneimap)
here::i_am("inst/rivers.R")

# Download data (46.5 MB)
if (!file.exists(here::here("inst/HydroRIVERS_v10_au_shp/HydroRIVERS_v10_au.shp"))) {
  download.file(
    url = "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_au_shp.zip",
    destfile = here::here("inst/HydroRIVERS_v10_au_shp.zip")
  )
  unzip(here::here("inst/HydroRIVERS_v10_au_shp.zip"), exdir = here::here("inst/"))
}

# Load data
rivers_sf <- sf::read_sf(here::here("inst/HydroRIVERS_v10_au_shp/HydroRIVERS_v10_au.shp"))

# Keep only those bounded by Brunei shape (takes a while)
bbox <- st_bbox(bruneimap::brn_sf)
bnrivers_sf <-
  st_crop(rivers_sf, bbox) |>
  st_intersection(bruneimap::brn_sf)

# Quick check
print(bnrivers_sf)
ggplot(bnrivers_sf) + geom_sf()

# Make a nice plot (background must be black)
bnrivers_sf |>
  filter(ORD_CLAS %in% 1:3) |>
  ggplot() +
  # geom_sf(data = bruneimap::brn_sf, col = "gray", fill = NA) +
  geom_sf(col = "white", aes(alpha = ORD_CLAS, linewidth = ORD_CLAS)) +
  scale_alpha_continuous(range = c(1, 0.5)) +
  scale_linewidth_continuous(range = c(0.8, 0.5)) +
  labs(title = "Rivers in Brunei") +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "black", colour = "black"))
 # change background to black


# Calculate a log-transformed discharge (adding a small constant to avoid log(0))

brn_boundaries <- st_boundary(brn_sf)

bnrivers_sf |>
  mutate(log_dis = log(DIS_AV_CMS + 1)) |>
  # Optionally filter to remove extremely small reaches if needed
  filter(DIS_AV_CMS > 0.1) |>
  ggplot() +
  # Add the background base map for context
  geom_sf(data = brn_boundaries, fill = NA, color = "yellow", alpha = 0.5, size = 1) +
  # Map the log-transformed discharge to both linewidth and alpha
  geom_sf(aes(linewidth = log_dis, alpha = log_dis), color = "white") +
  # Adjust the scales to expand the range of aesthetics
  scale_linewidth_continuous(range = c(0.3, 1.1)) +
  scale_alpha_continuous(range = c(0.4, 1)) +
  labs(title = "Rivers in Brunei",
       subtitle = "Major rivers highlighted by average discharge") +
  theme_void() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background  = element_rect(fill = "black", color = "black"),
    plot.title       = element_text(color = "white", size = 20, face = "bold"),
    plot.subtitle    = element_text(color = "white", size = 14)
  )

library(tidyverse)
library(geojsonsf)
# library(ggsflabel)
library(gghighlight)
library(spdep)
theme_set(theme_bw())

# Load geojson files -----------------------------------------------------------

# Data obtained from https://github.com/thewheat/brunei_map
import_geojsn_sf <- function(path) {
  geojson_sf(path) %>%
    sf::st_make_valid(.) %>%
    sf::st_cast(., "MULTIPOLYGON") %>%
    mutate(name = stringr::str_to_title(name)) %>%
    # Add centroids
    bind_cols(st_centroid(.) %>% st_coordinates() %>% suppressWarnings())
}

c(dis_sf = "datasrc/map_data/geojson/districts_latlon.txt.geojson",
  mkm_sf = "datasrc/map_data/geojson/mukims_latlon.txt.geojson",
  kpg_sf = "datasrc/map_data/geojson/kampongs_latlon.txt.geojson") %>%
  purrr::map(import_geojsn_sf) %>%
  list2env(envir = .GlobalEnv)

# Clean up ---------------------------------------------------------------------

# The kampong data set contains mukim, but the mukim does not have the district
# information. So, manually add this

# Save mukim data
# as_tibble(mkm_sf) %>%
#   select(-geometry) %>%
#   arrange(id) %>%
#   write_csv(file = "datasrc/map_data/mukimdata.csv")
# EDIT THE mukimdata.csv manually! <- I just add a column indicating which
# district it belongs to

# mukimdata.csv has the districts already
mukimdata <- read_csv("datasrc/map_data/mukimdata.csv") %>%
  select(name, district) %>%
  mutate(district = stringr::str_to_title(district))

# Add districts to kpg_sf and mkm_sf
kpg_sf <-
  kpg_sf %>%
  mutate(mukim = stringr::str_to_title(mukim)) %>%
  left_join(mukimdata, by = c("mukim" = "name")) %>%
  rename(kampong = name)

mkm_sf <-
  mkm_sf %>%
  left_join(mukimdata, by = "name") %>%
  rename(mukim = name)

# Missing values
mkm_sf[mkm_sf$id == 0, ]$mukim <- "Mukim Kota Batu"  # this is Pulau Berambang
mkm_sf[mkm_sf$id == 0, ]$length <- NA
mkm_sf[mkm_sf$id == 0, ]$district <- "Brunei Muara"

# Stkrj
kpg_sf <-
  kpg_sf %>%
  mutate(kampong = gsub("Stkrj", "STKRJ", kampong))

# Pekan Seria
kpg_sf[kpg_sf$id == 396, "kampong"] <- "Pekan Seria Kawasan 2"
kpg_sf[kpg_sf$id == 386, "kampong"] <- "Pekan Seria Kawasan 1"

# Export to Rdata --------------------------------------------------------------
usethis::use_data(dis_sf, overwrite = TRUE, compress = "xz")
usethis::use_data(mkm_sf, overwrite = TRUE, compress = "xz")
usethis::use_data(kpg_sf, overwrite = TRUE, compress = "xz")

# Create additional data frame for kampong, mukim and district information
bnkpg_df <- as_tibble(kpg_sf) %>%
  select(id, kampong, mukim, district) %>%
  # drop_na() %>%
  arrange(id)

usethis::use_data(bnkpg_df, overwrite = TRUE, compress = "xz")

# # Examples plot
# ggplot(dist) +
#   geom_sf()
#
# ggplot(mukim) +
#   geom_sf(aes(fill = length)) +
#   scale_fill_viridis_c()
#
# ggplot(filter(kpg, district == "BRUNEI MUARA")) +
#   geom_sf(aes(fill = area)) +
#   scale_fill_viridis_c()
#
# save(dist, mukim, kpg, kmd, file = "brunei_map.RData")

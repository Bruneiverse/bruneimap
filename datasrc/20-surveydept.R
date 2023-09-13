library(tidyverse)
library(sf)
library(spdep)
theme_set(theme_bw())

# Load geojson files -----------------------------------------------------------

# Data obtained from https://github.com/thewheat/brunei_map 13/9/23: I found
# that importing using sf package messes up the geometries. So instead, I opted
# to import the GeoJSON files using geopandas library in Python, and convert it
# to shape file. The shape files are then imported here.
# import_geojsn_sf <- function(path) {
#   geojson_sf(path) %>%
#     sf::st_make_valid(.) %>%
#     sf::st_cast(., "MULTIPOLYGON") %>%
#     mutate(name = stringr::str_to_title(name)) %>%
#     # Add centroids
#     bind_cols(st_centroid(.) %>%
#     st_coordinates() %>%
#     suppressWarnings()) %>%
#     sf::st_as_sf() %>%
#     as_tibble() %>%
#     sf::st_as_sf()
# }

my_import_fn <- function(x) {
  sf::read_sf(x) %>%
    # To title case
    mutate(name = stringr::str_to_title(name)) %>%
    # Add centroids
    bind_cols(st_centroid(.) %>% st_coordinates()) %>%
    arrange(id)
}

c(dis_sf = "datasrc/map_data/geojson_shp/district.shp",
  mkm_sf = "datasrc/map_data/geojson_shp/mukim.shp",
  kpg_sf = "datasrc/map_data/geojson_shp/kampong.shp") %>%
  purrr::map(my_import_fn) %>%
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

# Pulau Berambang (in Mukim Kota Batu)
tmp <-
  mkm_sf %>%
  filter(mukim == "Mukim Kota Batu" | id == 0)
cmb_poly <- st_union(tmp$geometry)
mkm_sf[mkm_sf$id == 41, ]$geometry <- cmb_poly
mkm_sf <- mkm_sf[-(mkm_sf$id == 0), ]

# Kampong data set
duplkpg <- kpg_sf %>%
  group_by(kampong) %>%
  summarise(n = n()) %>%
  filter(n > 1)

kpg_sf %>%
  filter(kampong %in% duplkpg$kampong) %>%
  select(id, kampong, mukim, district) %>%
  arrange(kampong) %>%
  drop_na() %>%
  st_drop_geometry() %>%
  print(n = 100)
# # A tibble: 23 × 4
#       id kampong                  mukim                district
# *  <dbl> <chr>                    <chr>                <chr>
# 1    375 Hutan Simpan Andulau     Mukim Liang          Belait
# 2    401 Hutan Simpan Andulau     Mukim Ukong          Tutong
# 3      7 Hutan Simpan Batu Apoi   Mukim Amo            Temburong
# 4     13 Hutan Simpan Batu Apoi   Mukim Bokok          Temburong
# 5    267 Hutan Simpan Bukit Ladan Mukim Rambai         Tutong
# 6    415 Hutan Simpan Bukit Ladan Mukim Lamunin        Tutong
# 7    259 Kg. Kilugus              Mukim Lumapas        Brunei Muara
# 8    260 Kg. Kilugus              Mukim Lumapas        Brunei Muara
# 9    192 Kg. Menengah             Mukim Bangar         Temburong
# 10   280 Kg. Menengah             Mukim Lamunin        Tutong
# 11   180 Kg. Panchor              Mukim Mentiri        Brunei Muara
# 12   257 Kg. Panchor              Mukim Lumapas        Brunei Muara
# 13   178 Kg. Parit                Mukim Pangkalan Batu Brunei Muara
# 14   179 Kg. Parit                Mukim Kianggeh       Brunei Muara
# 15   185 Kg. Parit                Mukim Amo            Temburong
# 16   188 Kg. Peliunan             Mukim Bangar         Temburong
# 17   214 Kg. Peliunan             Mukim Batu Apoi      Temburong
# 18   220 Kg. Simbatang            Mukim Batu Apoi      Temburong
# 19   230 Kg. Simbatang            Mukim Bokok          Temburong
# 20   139 Kg. Subok                Mukim Kota Batu      Brunei Muara
# 21   201 Kg. Subok                Mukim Bangar         Temburong
# 22     6 Kg. Sumbiling Lama       Mukim Amo            Temburong
# 23   254 Kg. Sumbiling Lama       Mukim Sungai Kedayan Brunei Muara
idx <- kpg_sf$id
kpg_sf$kampong[idx == 192] <- "Kg. Menengah Bangar"
kpg_sf$kampong[idx == 280] <- "Kg. Menengah Lamunin"
kpg_sf$kampong[idx == 180] <- "Kg. Panchor Mentiri"
kpg_sf$kampong[idx == 257] <- "Kg. Panchor Lumapas"
kpg_sf$kampong[idx == 178] <- "Kg. Parit Pangkalan Batu"
kpg_sf$kampong[idx == 179] <- "Kg. Parit Kianggeh"
kpg_sf$kampong[idx == 185] <- "Kg. Parit Amo"
kpg_sf$kampong[idx == 188] <- "Kg. Peliunan Bangar"
kpg_sf$kampong[idx == 214] <- "Kg. Peliunan Batu Apoi"
kpg_sf$kampong[idx == 220] <- "Kg. Simbatang Batu Apoi"
kpg_sf$kampong[idx == 230] <- "Kg. Simbatang Bokok"
kpg_sf$kampong[idx == 139] <- "Kg. Subok Kota Batu"
kpg_sf$kampong[idx == 201] <- "Kg. Subok Bangar"
kpg_sf$kampong[idx == 6]   <- "Kg. Sumbiling Lama Amo"
kpg_sf$kampong[idx == 254] <- "Kg. Sumbiling Lama Sungai Kedayan"

# Kilugus
tmp <-
  kpg_sf %>%
  filter(kampong == "Kg. Kilugus")
cmb_poly <- st_union(tmp$geometry)
kpg_sf[kpg_sf$id == 259, ]$geometry <- cmb_poly
kpg_sf <- filter(kpg_sf, id != 260)

# Kawasan Bahaya
kpg_sf[kpg_sf$id == 11, ]$kampong <- "Kawasan Bahaya Mukim Bokok"

# Redo the centroids
mkm_sf <-
  mkm_sf %>%
  select(-X, -Y) %>%
  bind_cols(st_centroid(.) %>% st_coordinates())

kpg_sf <-
  kpg_sf %>%
  select(-X, -Y) %>%
  bind_cols(st_centroid(.) %>% st_coordinates())

# Stkrj
kpg_sf <-
  kpg_sf %>%
  mutate(kampong = gsub("Stkrj", "STKRJ", kampong))

# Pekan Seria
kpg_sf[kpg_sf$id == 396, "kampong"] <- "Pekan Seria Kawasan 2"
kpg_sf[kpg_sf$id == 386, "kampong"] <- "Pekan Seria Kawasan 1"

# What about the NAs?
kpg_sf %>%
  filter(is.na(kampong)) %>%
  print(n = Inf)
# Pelumpong
kpg_sf[kpg_sf$id == 884, ]$kampong  <- "Tanjong Pelumpong"
kpg_sf[kpg_sf$id == 884, ]$district <- "Brunei Muara"
# Pulau Muara Besar
kpg_sf[kpg_sf$id == 883, ]$kampong  <- "Pulau Muara Besar"
kpg_sf[kpg_sf$id == 883, ]$district <- "Brunei Muara"
# Pulau Baru-Baru
kpg_sf[kpg_sf$id == 908, ]$kampong  <- "Pulau Baru-Baru"
kpg_sf[kpg_sf$id == 908, ]$district <- "Brunei Muara"
# Pulau Selirong
tmp <- filter(kpg_sf, id %in% c(911, 913, 914))
cmb_poly <- st_union(tmp$geometry)
kpg_sf[kpg_sf$id == 911, ]$geometry <- cmb_poly
kpg_sf <- filter(kpg_sf, !(id %in% c(913, 914)))
kpg_sf[kpg_sf$id == 911, ]$kampong  <- "Pulau Selirong"
kpg_sf[kpg_sf$id == 911, ]$district <- "Temburong"
# Pulau Berbunut
kpg_sf[kpg_sf$id == 910, ]$kampong  <- "Pulau Berbunut"
kpg_sf[kpg_sf$id == 910, ]$district <- "Temburong"
# Pulau Pepatan
kpg_sf[kpg_sf$id == 983, ]$kampong  <- "Pulau Pepatan"
kpg_sf[kpg_sf$id == 983, ]$district <- "Brunei Muara"
# Pulau Siarau
kpg_sf[kpg_sf$id == 915, ]$kampong  <- "Pulau Siarau"
kpg_sf[kpg_sf$id == 915, ]$district <- "Temburong"
# Pulau Tarap
kpg_sf[kpg_sf$id == 925, ]$kampong  <- "Pulau Tarap"
kpg_sf[kpg_sf$id == 925, ]$district <- "Temburong"
# Pulau Selanjak
kpg_sf[kpg_sf$id == 926, ]$kampong  <- "Pulau Selanjak"
kpg_sf[kpg_sf$id == 926, ]$district <- "Temburong"
# Pulau Bedukang
kpg_sf[kpg_sf$id == 882, ]$kampong  <- "Pulau Bedukang"
kpg_sf[kpg_sf$id == 882, ]$district <- "Brunei Muara"
# Pulau Si Mangga Besar
kpg_sf[kpg_sf$id == 893, ]$kampong  <- "Pulau Si Mangga Besar"
kpg_sf[kpg_sf$id == 893, ]$district <- "Brunei Muara"
# Pulau Suhung Damit
kpg_sf[kpg_sf$id == 894, ]$kampong  <- "Pulau Suhung Damit"
kpg_sf[kpg_sf$id == 894, ]$district <- "Brunei Muara"
# Pulau Chermin
kpg_sf[kpg_sf$id == 901, ]$kampong  <- "Pulau Chermin"
kpg_sf[kpg_sf$id == 901, ]$district <- "Brunei Muara"
# Pulau Kaingaran
kpg_sf[kpg_sf$id == 902, ]$kampong  <- "Pulau Kaingaran"
kpg_sf[kpg_sf$id == 902, ]$district <- "Brunei Muara"
# Kg. Serasa
tmp <- filter(kpg_sf, id %in% c(131, 932))
cmb_poly <- st_union(tmp$geometry)
kpg_sf[kpg_sf$id == 131, ]$geometry <- cmb_poly
kpg_sf <- filter(kpg_sf, !(id %in% c(932)))
# Pulau Sibungor
kpg_sf[kpg_sf$id == 917, ]$kampong  <- "Pulau Sibungor"
kpg_sf[kpg_sf$id == 917, ]$district <- "Brunei Muara"
# Hutan di Katok
tmp <- filter(kpg_sf, id %in% c(247, 249))
cmb_poly <- st_union(tmp$geometry)
kpg_sf[kpg_sf$id == 247, ]$geometry <- cmb_poly
kpg_sf <- filter(kpg_sf, !(id %in% c(249)))
# Pulau Setawat
kpg_sf[kpg_sf$id == 933, ]$kampong  <- "Pulau Setawat"
kpg_sf[kpg_sf$id == 933, ]$district <- "Tutong"
# Pulau Bakuku
tmp <- filter(kpg_sf, id %in% c(921:924))
cmb_poly <- st_union(tmp$geometry)
kpg_sf[kpg_sf$id == 921, ]$geometry <- cmb_poly
kpg_sf <- filter(kpg_sf, !(id %in% c(922:924)))
kpg_sf[kpg_sf$id == 921, ]$kampong  <- "Pulau Bakuku"
kpg_sf[kpg_sf$id == 921, ]$district <- "Tutong"
# Badas
kpg_sf[kpg_sf$id == 370, ]$kampong  <- "Badas Liang"
# Kuala Balai
kpg_sf[kpg_sf$id == 299, ]$kampong  <- "Hutan Simpan Kuala Balai"

# Delete these ids:
# - 920 island does not exist
# - 930 duplicate with 921
# - 928 duplicate with 922
# - 927 duplicate with 923
# - 929 duplicate with 924
kpg_sf <- filter(kpg_sf, !(id %in% c(920, 930, 928, 927, 929)))

# Final clean ups
dis_sf <-
  select(dis_sf, -code) %>%
  mutate(perimeter = st_length(st_boundary(geometry)),
         area = st_area(geometry))
mkm_sf <-
  select(mkm_sf, id, mukim, district, geometry, X, Y) %>%
  mutate(perimeter = st_length(st_boundary(geometry)),
         area = st_area(geometry))
kpg_sf <-
  select(kpg_sf, id, kampong, mukim, district, geometry, X, Y) %>%
  mutate(perimeter = st_length(st_boundary(geometry)),
         area = st_area(geometry))

# Export to Rdata --------------------------------------------------------------
usethis::use_data(dis_sf, overwrite = TRUE, compress = "xz")
usethis::use_data(mkm_sf, overwrite = TRUE, compress = "xz")
usethis::use_data(kpg_sf, overwrite = TRUE, compress = "xz")

# Create additional data frame for kampong, mukim and district information
bnkpg_df <- as_tibble(kpg_sf) %>%
  select(id, kampong, mukim, district) %>%
  # drop_na() %>%
  arrange(id)

write_csv(bnkpg_df, file = "datasrc/bn_kpg_level_data.csv")

usethis::use_data(bnkpg_df, overwrite = TRUE, compress = "xz")

# # Examples plot
# ggplot(dis_sf) +
#   geom_sf()
#
# ggplot(mkm_sf) +
#   geom_sf(aes(fill = as.numeric(perimeter))) +
#   scale_fill_viridis_c()
#
# ggplot(filter(kpg_sf, district == "Brunei Muara")) +
#   geom_sf(aes(fill = as.numeric(area))) +
#   scale_fill_viridis_c()

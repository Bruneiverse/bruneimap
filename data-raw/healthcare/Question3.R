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
library(ggsci)
library(patchwork)

#File for road network
brd <- 
  read_sf("hotosm_brn_roads_lines_geojson.geojson") |>
  st_transform(4326)  # SET THE CRS (WGS84)
glimpse(brd)

table(brd$highway)

brd_mjr <- 
  brd |>
  filter(highway %in% c("motorway", "trunk", "primary", "secondary")) 
brd_mjr

#Plot for road network
ggplot() +
  geom_sf(data = brn_sf) +
  geom_sf(data = brd_mjr, aes(col = highway), size = 0.5) +
  #scale_colour_viridis_d(option = "turbo")
  ggsci::scale_colour_npg()

#Plotting
roadn <- ggplot() +
  geom_sf(data = dis_sf) +
  geom_sf(data = brd_mjr, aes(col = highway), size = 0.3) +
  #scale_colour_viridis_d(option = "turbo") +
  ggsci::scale_colour_npg() +
  geom_sf(data = st_filter(hospitalsh_sf, mkm_sf), size = 2, color = "black") +  # Existing hospitals
  #geom_sf(data = st_filter(hpex, mkm_sf), size = 3, color = "red", fill = "yellow") +  # Hospitals from Excel
  geom_sf(data = st_filter(hpex, mkm_sf), size = 2, color = "black", fill = "black") +
  labs(title = "Hospitals around Brunei",
       fill = "District") 

roadn #district has no color but easier to see highways

mew <- ggplot() +
  geom_sf(data = mkm_sf, aes(fill = district), alpha = 0.4) +
  geom_sf(data = st_filter(hospitalsh_sf, mkm_sf), size = 2, color = "black") +  # Existing hospitals
  #geom_sf(data = st_filter(hpex, mkm_sf), size = 3, color = "red", fill = "yellow") +  # Hospitals from Excel
  geom_sf(data = st_filter(hpex, mkm_sf), size = 2, color = "black", fill = "black") +
  labs(title = "Hospitals around Brunei",
       fill = "District") +
  geom_sf(data = brd_mjr, aes(col = highway), size = 0.5) +
  scale_colour_viridis_d(option = "turbo") +
  ggsci::scale_colour_npg()

mew #district has color but too many lines bcoz of mukim boundaries
roadn + mew + plot_layout(ncol=2)

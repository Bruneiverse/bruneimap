**Code for Qs 4**

*File for road network*
```r
brd <- 
  read_sf("hotosm_brn_roads_lines_geojson.geojson") |>
  st_transform(4326)  # SET THE CRS!!! (WGS84)
glimpse(brd)

table(brd$highway)

brd_mjr <- 
  brd |>
  filter(highway %in% c("motorway", "trunk", "primary", "secondary")) 
brd_mjr
```

*Plot for road network*
```r
ggplot() +
  geom_sf(data = brn_sf) +
  geom_sf(data = brd_mjr, aes(col = highway), size = 0.5) +
  #scale_colour_viridis_d(option = "turbo")
  ggsci::scale_colour_npg()
```

*Plotting*
```r
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
```

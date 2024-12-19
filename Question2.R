library(tidyverse)
library(bruneimap)
library(ggrepel)
library(kernlab)
library(osrm)
library(osmdata)
library(readxl)
library(patchwork)
glimpse(bn_census2021)

#joining bn_census2021 with kpg_sf for data
bn_pop_sf <- 
  left_join(
    kpg_sf, 
    bn_census2021, 
    by = join_by(id, kampong, mukim, district)
  )

bn_popdist <- bn_pop_sf %>%
  select(population, district)

#Checking data
view(bn_popdist)
view(bn_pop_sf)


#Plotting
plotpop <- bn_pop_sf |>
  # filter(population > 50) |>
  ggplot() +
  geom_sf(aes(fill = population), col = NA, alpha = 0.8) +
  geom_sf(data = kpg_sf, fill = NA, col = "black") + 
  labs(
    title = "Population in Brunei",
    x = "Longitude",
    y = "Latitdue"
  )+
  scale_fill_viridis_b(
    name = "Population",
    na.value = NA,
    labels = scales::comma,
    breaks = c(0, 100, 1000, 10000, 20000)
    # limits = c(0, 12000)
  ) +
  theme_bw()
plotpop
plotpop + plotm + plot_layout(ncol = 2)
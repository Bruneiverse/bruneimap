A scuffed explaination cause to be honest, I don't know what's going on half of the time
Loading packages
```r
library(tidyverse)
library(bruneimap)
library(ggrepel)
library(kernlab)
library(osrm)
library(osmdata)
library(readxl)
library(patchwork)
glimpse(bn_census2021)
```
Saving the population file and joining it with teh kpg_sf 
```r
bn_pop_sf <- 
  left_join(
    kpg_sf, 
    bn_census2021, 
    by = join_by(id, kampong, mukim, district)
  )

bn_popdist <- bn_pop_sf %>%
  select(population, district)
```

Checking the data
```r
view(bn_popdist)
view(bn_pop_sf)
```


Plotting
```r
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
```

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

The labeling for pointing towards the top 3 kampongs in terms of population
```r
mkm_labels_sf <-
  bn_pop_sf |>
  arrange(desc(population)) |>
  slice_head(n = 3)
# ^ points towards the districts with the highest population; arranges the population in descending order 
# and basically finds the top 3 highest population and finds out which districts its from, eg; top 2 are from Brunei Muara
# btw the reason its theres 2 from Brunei Muara is cause they count the populations by each kampong,
# so theres 2 kampongs with high population from Brunei Muara
view(mkm_labels_sf) 
```

Plotting
```r
plotpop <- bn_pop_sf |>
  # filter(population > 50) |>
  ggplot() +
  geom_sf(aes(fill = population), col = NA, alpha = 0.8) +
  geom_sf(data = kpg_sf, fill = NA, col = "black") +
  ggrepel::geom_label_repel(
    data = mkm_labels_sf,
    aes(label = district, geometry = geometry),
    stat = "sf_coordinates",
    inherit.aes = FALSE,
    box.padding = 1,
    size = 2,
    max.overlaps = Inf
  ) + 
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

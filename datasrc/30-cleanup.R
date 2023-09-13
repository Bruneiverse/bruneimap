library(tidyverse)
library(bruneimap)  # or load all

tmp <-
  kpg_sf %>%
  group_by(mukim) %>%
  summarize(geometry = st_union(geometry), .groups = "drop")


tmp2 <- st_simplify(tmp, preserveTopology = TRUE, dTolerance = 1)

tmp3 <- st_difference(tmp, st_union(st_boundary(tmp)))


ggplot(tmp3) +
  geom_sf()

ggplot(mkm_sf) +
  geom_sf(fill = "red3")


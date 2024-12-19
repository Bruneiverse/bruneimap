# Load libraries
library(tidyverse)
library(bruneimap)
theme_set(theme_bw())
library(sf)

# Create map from shapefile
fr_sf <- st_read("~/Documents/github_local/forestreserve_bn/data/fr_map.shp")

# Check fr_sf
p1 <- ggplot(dis_sf) +
  geom_sf() +
  geom_sf(data = fr_sf, fill = "green4") +
  labs(title = 'Gazetted Forest Reserves of Brunei Darussalam')+
  theme(plot.title = element_text(size = 20))

ggsave("forestreserve_bn.png", plot = p1)

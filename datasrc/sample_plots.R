library(tidyverse)
library(bruneimap)
library(sf)

my_df <- readxl::read_excel("datasrc/NEW_bnkpg_df.xlsx")

kpg_sf
left_join(kpg_sf, my_df, by = "id") %>%
  filter(district.x == "Belait") %>%
  ggplot(aes(fill = mosque)) +
  geom_sf() +
  scale_fill_viridis_c()

left_join(kpg_sf, my_df, by = "id") %>%
  filter(district.x == "Belait") %>%
  group_by(mukim.x) %>%
  summarise(mosque = sum(mosque, na.rm = TRUE)) %>%
  mutate(mosque = factor(mosque)) %>%
  ggplot(aes(fill = mosque)) +
  geom_sf() +
  geom_sf_label(aes(label = mukim.x)) +
  scale_fill_viridis_d()

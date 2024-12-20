library(bruneimap)
library(tidyverse)
library(hexSticker)

p <-
  ggplot(kpg_sf) +
  geom_sf(aes(fill = mukim), col = "gray50", alpha = 0.8) +
  geom_sf(data = mkm_sf, col = "black", lwd = 0.5, fill = NA) +
  geom_sf(data = filter(kpg_sf, is.na(mukim)), fill = "gray70", col = "gray70") +
  scale_fill_viridis_d(option = "mako") +
  theme_void() +
  theme_transparent() +
  theme(legend.position = "none")

s <-
  sticker(
    p, package = "bruneimap",
    s_x = 1, s_y = 0.91, s_width = 1.7, s_height = 1.7,
    p_x = 0.75, p_y = 1.48, p_size = 25, angle = 30,
    # p_family = "Roboto",
    p_fontface = "plain", p_color = "#357BA2FF",
    h_size = 3, h_fill = "white", h_color = "black",
    # spotlight = TRUE,
    filename = "inst/hexSticker.png", dpi = 300
  ); print(s)
ggsave("inst/hexSticker.png", dpi = 200, width = 4, height = 4)

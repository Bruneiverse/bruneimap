library(tidyverse)
masjid <- read_csv(here::here("data-raw/masjid/masjid.csv"))
usethis::use_data(masjid, overwrite = TRUE, compress = "xz")

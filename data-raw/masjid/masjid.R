library(tidyverse)

# Notes:
# - masjid_old.csv from Algebros (least amount of information)
# - masjid.csv from The R Ones
# - bruneimosque.csv from HWR (most complete)
#
# Edit bruneimosque.csv for future updates!

masjid <-
  read_csv(here::here("data-raw/masjid/bruneimosque.csv")) |>
  mutate(
    district = gsub("Brunei Muara", "Brunei-Muara", district)
  )
usethis::use_data(masjid, overwrite = TRUE, compress = "xz")

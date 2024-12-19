library(tidyverse)
theme_set(theme_bw())

census2021 <- readxl::read_excel(here::here("data-raw/census/census_2021.xlsx"))
# usethis::use_data(bn_census2021, overwrite = TRUE, compress = "xz")
usethis::use_data(census2021, overwrite = TRUE, compress = "xz")

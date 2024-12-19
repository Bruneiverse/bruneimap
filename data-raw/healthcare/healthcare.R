library(tidyverse)

healthcare <- read_csv(here::here("data-raw/healthcare/healthcare.csv"))
usethis::use_data(healthcare, overwrite = TRUE, compress = "xz")

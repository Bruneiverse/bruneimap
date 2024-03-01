library(tidyverse)
theme_set(theme_bw())

bn_census2021 <- readxl::read_excel("datasrc/census_2021.xlsx")
usethis::use_data(bn_census2021, overwrite = TRUE, compress = "xz")

# FIXME: In the future, want to add metadata to the data set.

library(tidyverse)
source(here::here("data-raw/schools/school_spatial_analysis.R"))

usethis::use_data(sch_sf, overwrite = TRUE, compress = "xz")
usethis::use_data(tchr, overwrite = TRUE, compress = "xz")
usethis::use_data(enrolment, overwrite = TRUE, compress = "xz")
usethis::use_data(enrolment_MOE, overwrite = TRUE, compress = "xz")

#' Brunei census data 2021
#'
#' Brunei census data 2021
#'
#' A tibble containing the population, split into male/female or
#'   Bruneian/PR/foreigners for each kampong. Also contains number of households
#'   and occupied living quarters.
#'
#' @references DEPS. “The Population and Housing Census Report (BPP) 2021:
#'   Demographic, Household and Housing Characteristics.” Department of Economic
#'   Planning and Statistics, Ministry of Finance and Economy, Brunei
#'   Darussalam, October 2022. URL:
#'   \url{https://deps.mofe.gov.bn/SitePages/Population.aspx}
#'
#' @examples
#' library(tidyverse)
#' census2021 |>
#'   summarise(
#'     population = sum(population),
#'     .by = district
#'   )
"census2021"



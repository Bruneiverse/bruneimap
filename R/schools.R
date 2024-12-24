#' Brunei schools datasets 2018
#'
#' Brunei schools datasets 2018
#'
#' There are four datasets related to schools in Brunei:
#' - `sch_sf`: A spatial object of schools in Brunei
#' - `tchr`: A tibble of the number of teachers in schools in Brunei
#' - `enrolment`: A tibble of the number of students in schools in Brunei
#' - `enrolment_moe`: A tibble of the number of students in MOE schools (primary to sixth form) in Brunei
#'
#' @name schools
#' @references
#' - [Brunei Darussalam Education Statistics 2018](https://www.moe.gov.bn/DocumentDownloads/Education%20Statistics%20and%20Indicators%20Handbook/Brunei%20Darussalam%20Education%20Statistics%202018.pdf).
#'   Ministry of Education, Brunei.
#'   
#' @examples
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = kpg_sf) +
#'   geom_sf(data = sch_sf)
NULL

#' @rdname schools
"sch_sf"

#' @rdname schools
"tchr"

#' @rdname schools
"enrolment"

#' @rdname schools
"enrolment_moe"

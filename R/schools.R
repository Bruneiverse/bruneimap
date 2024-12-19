#' Data sets relating to schools in Brunei
#'
#' Data sets relating to schools in Brunei
#'
#' There are four datasets related to schools in Brunei:
#' - `sch_sf`: A spatial object of schools in Brunei
#' - `tchr`: A tibble of the number of teachers in schools in Brunei
#' - `enrolment`: A tibble of the number of students in schools in Brunei
#' - `enrolment_MOE`: A tibble of the number of students in schools in Brunei
#'
#' @name schools
#' @references
#' - [Education Statistics and Indicators
#'   Handbook](https://www.moe.gov.bn/DocumentDownloads/Forms/AllItems.aspx?RootFolder=%2fDocumentDownloads%2fEducation%20Statistics%20and%20Indicators%20Handbook&FolderCTID=0x012000686799B237388E4B93D164D060CACEC6).
#'   Ministry of Education, Brunei.
#' - [Summarised version, Y2023](https://deps.mofe.gov.bn/SitePages/Statistical%20Publications.aspx)
#' @examples
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = brn_sf) +
#'   geom_sf(data = sch_sf)
NULL

#' @rdname schools
"sch_sf"

#' @rdname schools
"tchr"

#' @rdname schools
"enrolment"

#' @rdname schools
"enrolment_MOE"

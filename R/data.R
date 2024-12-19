#' Simple feature objects to plot Brunei maps
#'
#' Simple feature objects to plot Brunei maps
#'
#' There are three simple feature objects in this package that can be used to
#' plot Brunei maps:
#' - `dis_sf`: District-level boundaries
#' - `mkm_sf`: Mukim-level boundaries
#' - `kpg_sf`: Kampong-level boundaries
#' - `brn_sf`: A sillhouette of Brunei
#'
#' @name bruneimap
#' @rdname bruneimap
#' @format An [`sf`] object.
#'
#' @examples
#' library(ggplot2)
#'
#' # District-level boundaries
#' ggplot(dis_sf) +
#'   geom_sf()
#'
#' # Mukim-level boundaries
#' ggplot(mkm_sf) +
#'   geom_sf()
#'
#' # Kampong-level boundaries
#' ggplot(kpg_sf) +
#'   geom_sf()
"dis_sf"

#' @rdname bruneimap
#' @format NULL
"mkm_sf"

#' @rdname bruneimap
#' @format NULL
"kpg_sf"

#' @rdname bruneimap
#' @format NULL
"brn_sf"

#' Brunei kampong data
#'
#' Brunei kampong data
#'
#' Brunei kampong data
#'
#' @format A tibble containing kampong, mukim, district information. All rows
#'   correspond to an area on the map. Useful to combine with other unit-level
#'   information for plotting.
"bnkpg_df"

#' Brunei census data 2021
#'
#' Brunei census data 2021
#'
#' Brunei census data 2021
#'
#' @format A tibble containing the population, split into male/female or
#'   Bruneian/PR/foreigners for each kampong. Also contains number of households
#'   and occupied living quarters.
#'
#' @references DEPS. “The Population and Housing Census Report (BPP) 2021:
#'   Demographic, Household and Housing Characteristics.” Department of Economic
#'   Planning and Statistics, Ministry of Finance and Economy, Brunei
#'   Darussalam, October 2022. URL:
#'   \url{https://deps.mofe.gov.bn/SitePages/Population.aspx}
#'
"bn_census2021"



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
"mkm_sf"

#' @rdname bruneimap
"kpg_sf"

#' @rdname bruneimap
"brn_sf"

#' Brunei kampong data
#'
#' Brunei kampong data
#'
#' A tibble containing kampong, mukim, district information. All rows
#'   correspond to an area on the map. Useful to combine with other unit-level
#'   information for plotting.
"bnkpg_df"

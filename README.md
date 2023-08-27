
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bruneimap

<!-- badges: start -->

[![R-CMD-check](https://github.com/propertypricebn/bruneimap/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/propertypricebn/bruneimap/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Provides Brunei GIS data in the form of a `sf` (simple features) object
ready for plotting and analysis in R. The smallest (areal) unit of
analysis is a “kampong”, which in turn is contained within mukims and
districts.

## Installation

You can install the development version of bruneimap from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pkg_install("propertypricebn/bruneimap")
```

## Features

There are three `sf` files contained in the package:

1.  `dis_sf` (District level boundaries)
2.  `mkm_sf` (Mukim level boundaries)
3.  `kpg_sf` (Kampong level boundaries)

Most likely you will want to use the kampong level data. Of course, the
`kpg_sf` can be summarised in terms of mukims and districts, but for
convenience the other two data sets are provided as well.

### Data

``` r
# Load libraries
library(tidyverse)
theme_set(theme_bw())
library(bruneimap)
library(sf)

# What's in our data set?
glimpse(kpg_sf)
#> Rows: 451
#> Columns: 8
#> $ id       <dbl> 28, 29, 30, 31, 32, 25, 26, 27, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10…
#> $ kampong  <chr> "Perumahan Negara Rimba Kawasan 5", "Perumahan Negara Mentiri…
#> $ mukim    <chr> "Mukim Gadong A", "Mukim Mentiri", "Mukim Mentiri", "Mukim Be…
#> $ area     <dbl> 1697312.38, 2850799.25, 926182.25, 4115267.50, 902883.44, 137…
#> $ X        <dbl> 114.9061, 115.0368, 115.0269, 114.9399, 114.9472, 114.9197, 1…
#> $ Y        <dbl> 4.968952, 4.970981, 4.970273, 4.924705, 4.936163, 4.963643, 4…
#> $ district <chr> "Brunei Muara", "Brunei Muara", "Brunei Muara", "Brunei Muara…
#> $ geometry <MULTIPOLYGON [°]> MULTIPOLYGON (((114.9104 4...., MULTIPOLYGON (((…
```

### Plots

``` r
ggplot(kpg_sf) +
  geom_sf(aes(fill = mukim)) +
  geom_sf(data = mkm_sf, col = "black", lwd = 0.5, fill = NA) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "turbo")
```

<img src="man/figures/README-brunei_map_plot-1.png" width="100%" />

## Acknowledgements

- The Brunei map was generated from publically available [Survey
  Department](https://geoportal.survey.gov.bn/start-gp) data. The
  GeoJSON files were sourced from
  [thewheat/brunei_map](https://github.com/thewheat/brunei_map) GitHub
  repo. Comes with a neat [web
  app](http://thewheat.github.io/brunei_map/). Many thanks to these
  guys!

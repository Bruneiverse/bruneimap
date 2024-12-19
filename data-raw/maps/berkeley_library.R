library(sf)           # Objects and functions for geospatial data
library(rgdal)        # Functions for spatial data input/output
library(ggplot2)      # Graphing functions
library(dplyr)        # Functions for processing tabular data
library(tidyr)        # Functions for processing tabular data
library(scales)       # Additional graphics functions
library(RColorBrewer) # Color ramps for graphs and maps
library(gridExtra)    # Functions for arranging multiple plots on a page
library(readr)        # Functions for reading data

# The following shape file was obtained from the Berkeley Library. However it
# only contains Mukim level boundaries.
#
# https://geodata.lib.berkeley.edu/catalog/stanford-rb162qt8897

bnmap <- st_read(dsn = "src/map_data/shapefile/BRN_adm2.shp")

ggplot(bnmap) +
  geom_sf()



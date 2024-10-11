library(tidyverse)
library(bruneimap)
library(ggrepel)
library(kernlab)
library(osrm)
library(osmdata)


# Data Collection ---------------------------------------------------------
dis_sf
# Brunei districts geometries.
mkm_sf 
# Brunei mukim geometries.
kpg_sf
# Brunei kampong geometries.
brn_sf
# Brunei outline geometries.
bn_census2021
# Brunei 2021 census data.

<<<<<<< Updated upstream

# Data - GIS School-----------------------------------------------------
# @ Aniq @ Rayme
# Make sure to clean out schools not in brunei
# Make sure total count of schools match the total count from MOE source
# Format:
# school |  district  | kampung | geometry | school type

# Boundary boxes for each districts ---------------------------------------



=======
# Boundary boxes for each districts ---------------------------------------


>>>>>>> Stashed changes
# Bounding box for Brunei Muara
bm_sf <- filter(kpg_sf, district == "Brunei Muara")
bm_bbox <- st_bbox(bm_sf)

#Tutong
tut_sf <- filter(kpg_sf, district == "Tutong")
tut_bbox <- st_bbox(tut_sf)

#Temburong
tem_sf <- filter(kpg_sf, district == "Temburong")
tem_bbox <- st_bbox(tem_sf)

#Belait
bel_sf <- filter(kpg_sf, district == "Belait")
bel_bbox <- st_bbox(bel_sf)


# Queries assignments -----------------------------------------------------


q <- opq(bm_bbox) |>
  add_osm_feature(
    key = "amenity", 
    value = "school"
  ) |>
  osmdata_sf()
print(q)

r <- opq(tut_bbox) |>
  add_osm_feature(
    key = "amenity", 
    value = "school"
  ) |>
  osmdata_sf()
print(r)

s <- opq(tem_bbox) |>
  add_osm_feature(
    key = "amenity", 
    value = "school"
  ) |>
  osmdata_sf()
print(s)

t <- opq(bel_bbox) |>
  add_osm_feature(
    key = "amenity", 
    value = "school"
  ) |>
  osmdata_sf()
print(t)


# OSM assignments ---------------------------------------------------------



schools_sf_bm <-
  q$osm_polygons  |>
  as_tibble() |>  # these two lines convert to tibble-like object
  st_as_sf() |> 
  select(osm_id, name) |>
  drop_na() |>
  st_centroid()  # obtains X,Y coordinates of centroids

print(schools_sf_bm)

schools_sf_tut <-
  r$osm_polygons  |>
  as_tibble() |>  # these two lines convert to tibble-like object
  st_as_sf() |> 
  select(osm_id, name) |>
  drop_na()

print(schools_sf_tut)

schools_sf_tem <-
  s$osm_polygons  |>
  as_tibble() |>  # these two lines convert to tibble-like object
  st_as_sf() |> 
  select(osm_id, name) |>
  drop_na()

print(schools_sf_tem)

schools_sf_bel <-
  t$osm_polygons  |>
  as_tibble() |>  # these two lines convert to tibble-like object
  st_as_sf() |> 
  select(osm_id, name) |>
  drop_na()

print(schools_sf_bel)

<<<<<<< Updated upstream
bind_rows(schools_sf_bel, schools_sf_tut, schools_sf_tem, schools_sf_bm)



# Data - study variable - MOE Data ---------------------------------------------------
# trimmed pdf using pdftools: pdf_subset("moe2.pdf", pages = c(127:145), output = "moe2018.pdf")
=======
schools_sf_all <- bind_rows(schools_sf_bel, schools_sf_tut, schools_sf_tem, schools_sf_bm)

#   B. Study variable - MOE 2018 ---------------------------------------------------
# Trimmed pdf using pdftools: pdf_subset("moe2018.pdf", pages = c(127:145), output = "moe2018_extracted.pdf")
>>>>>>> Stashed changes
# Split landscape (2 pages per page) to portrait using https://deftpdf.com/split-pdf-down-the-middle
# Converted to excel using online ilovepdf.com

read_csv("data/Count of Student by District and School Type.csv")
# count of students

# EDA ---------------------------------------------------------------------




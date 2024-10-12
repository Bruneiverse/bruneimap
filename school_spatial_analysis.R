library(tidyverse)
# remotes::install_github("propertypricebn/bruneimap")
library(bruneimap)
library(ggrepel)
library(kernlab)
library(osrm)
library(osmdata)
library(readxl)


# 1. DATA COLLECTION/CLEANING
#   A. GIS - School-----------------------------------------------------

# Boundary boxes for each districts ---------------------------------------


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

schools_sf_all <- bind_rows(schools_sf_bel, schools_sf_tut, schools_sf_tem, schools_sf_bm)

#   B. Study variable - MOE 2018 ---------------------------------------------------
# Trimmed pdf using pdftools: pdf_subset("moe2018.pdf", pages = c(127:145), output = "moe2018_extracted.pdf")
# Split landscape (2 pages per page) to portrait using https://deftpdf.com/split-pdf-down-the-middle
# Converted to excel using online ilovepdf.com
# Clean and tidy on MS Excel


# 2. DATA CLEANING --------------------------------------------------------
#   A. GIS - School ---------------------------------------------------------------------
# Filter out schools not in Brunei
# Match to MOE school listing


#   B. Study variable - MOE 2018 ---------------------------------------------------
#     tchr ----------------------------------------------------------------------
# Create a list of sheet numbers and corresponding sector names
sheets <- list(
  list(sheet = 2, sector = "MOE"),
  list(sheet = 7, sector = "MORA"),
  list(sheet = 9, sector = "private")
)

# Initialize an empty list to store data frames
tchr <- list()

# Loop through each sheet
for (i in sheets) {
  df <- read_excel("MOE2018/moe2018_final.xlsx", i$sheet)
  colnames(df) <- df[2, ]
  df <- df %>% 
    slice(3:nrow(df)) %>% 
    mutate(Sector = i$sector)
  
  # Store the processed data frame in the list
  tchr[[i$sector]] <- df
}

# Combine
tchr <- bind_rows(tchr)

#     enrolment (district, sector) ---------------------------------------------------------
# Create a list of sheet numbers and corresponding sector names
sheets <- list(
  list(sheet = 3, sector = "MOE"),
  list(sheet = 8, sector = "MORA"),
  list(sheet = 9, sector = "private")
)

# Initialize an empty list to store data frames
enrolment <- list()

# Loop through each sheet
for (i in sheets) {
  df <- read_excel("MOE2018/moe2018_final.xlsx", i$sheet)
  colnames(df) <- df[2, ]
  df <- df %>% 
    slice(3:nrow(df)) %>% 
    mutate(Sector = i$sector)
  
  # Store the processed data frame in the list
  enrolment[[i$sector]] <- df
}

# Combine all data frames into one
enrolment <- bind_rows(enrolment)
view(enrolment)

#     enrolment_cluster (MOE cluster) -------------------------------------------
# Initialize an empty list to store data frames
enrolment_MOE <- list()

for (i in 4:6) {
  df <- read_excel("MOE2018/moe2018_final.xlsx", i)
  colnames(df) <- df[2, ]
  df <- df %>% 
    slice(3:nrow(df))
  
  # Store the processed data frame in the list
  enrolment_MOE[[i]] <- df
}

# Combine all data frames into one
enrolment_MOE <- bind_rows(enrolment_MOE)
view(enrolment_MOE)




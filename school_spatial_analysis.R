# o. Libraries ---------------------------------------------------------------
library(tidyverse)
# remotes::install_github("propertypricebn/bruneimap")
library(bruneimap)
library(ggrepel)
library(kernlab)
library(osrm)
library(osmdata)
library(readxl)

# 1. GIS - School---------------------------------------------------------------
#   A. Data Cleaning - 175 entries--------------------------------------
bb <- getbb('brunei darussalam', format_out = 'polygon', featuretype = "country")
q <-
  opq(bb[2]) |>
  add_osm_feature(
    key = "amenity", 
    value = "school"
  ) |>
  osmdata_sf() |>
  trim_osmdata(bb[2]) # must be in data.frame format (section a1 for more details)

schools_sf <-
  q$osm_polygons  |>
  as_tibble() |>  
  st_as_sf() |> 
  select(osm_id, name) |>
  drop_na() |>
  st_centroid()

#     A1: Fix issue on getbb ----------------------------------------------------------------------
# there are multiple places called "brunei"
str(bb)

# we need to compare bb[[1]] vs bb[[2]...to determine which is the correct boundary we want
bb1 <- bb[[1]]
# Convert the matrix to a dataframe
bb1_df <- data.frame(lng = bb1[, 1], lat = bb1[, 2])
# Convert the dataframe to an sf object
bb1_sf <- st_as_sf(bb1_df, coords = c("lng", "lat"), crs = 4326)
# Create a polygon from the sf points
bb1_polygon <- st_cast(st_combine(bb1_sf), "POLYGON")
# Plot boundary
ggplot(data = bb1_polygon) +
  geom_sf() 

bb2 <- bb[[2]]
# Convert the matrix to a dataframe
bb2_df <- data.frame(lng = bb2[, 1], lat = bb2[, 2])
# Convert the dataframe to an sf object
bb2_sf <- st_as_sf(bb2_df, coords = c("lng", "lat"), crs = 4326)
# Create a polygon from the sf points
bb2_polygon <- st_cast(st_combine(bb2_sf), "POLYGON")
# Plot boundary
ggplot(data = bb2_polygon) +
  geom_sf() 

# Comparing the plots, bb[[2]] is the boundary we are looking for
# alternatively, we can use brn_sf but it has to be converted to matrix format 


#   B. Data Processing - left join geometry (address) ----------------------------------------------------------------------
# we want to compare OSM query to MOE school listing
df <- read_csv("data/School listing.csv")
# only 71 entries from OSM query match MOE listing of 251 entries
schools_sf_temp <- schools_sf[schools_sf$name %in% df$School,]

# Issue: school names slight variation/abbreviated
# Solution: left join the matching entries, and fill up the remaining geometry manually
df <- 
  left_join(df, schools_sf_temp, by = c("School" = "name")) %>% 
  select(School, Sector, Cluster, `Education Level`, geometry)

# Convert from list to sf object
df_sf <- st_as_sf(df)

# Extract coordinates
df_sf$longitude <- st_coordinates(df_sf)[,1]
df_sf$latitude <- st_coordinates(df_sf)[,2]

# Convert to data frame from sf
df_sf <- st_drop_geometry(df_sf) %>% data.frame()

# Extract (DO NOT RUN AGAIN)
# write.csv(df_sf, "school listing v2.csv", row.names = FALSE)

# Manually add in remaining address using MS Excel
# New version saved as "school listing.xlsx"

#   C. Data Processing - join kpg_sf --------------------------------
schools <- read_excel("data/school listing.xlsx", 1)
schools_sf <- (st_as_sf(schools, coords = c("longitude", "latitude"), crs = 4326))
schools_sf <- st_join(schools_sf, kpg_sf, join = st_within)
view(schools_sf)

# 2. Study variable - MOE2018 --------------------------------------------------
#   A. Data Cleaning ---------------------------------------------------
# Trimmed pdf using pdftools: pdf_subset("moe2018.pdf", pages = c(127:145), output = "moe2018_extracted.pdf")
# Split landscape (2 pages per page) to portrait using https://deftpdf.com/split-pdf-down-the-middle
# Converted to excel using online ilovepdf.com
# Clean and tidy on MS Excel

#   B. Data Processing - Convert to tidy data---------------------------------------------------
#     i. tchr ----------------------------------------------------------------------
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
  df <- read_excel("data/moe2018.xlsx", i$sheet)
  colnames(df) <- df[2, ]
  df <- df %>% 
    slice(3:nrow(df)) %>% 
    mutate(Sector = i$sector)
  
  # Store the processed data frame in the list
  tchr[[i$sector]] <- df
}

# Combine
tchr <- bind_rows(tchr)
view(tchr)

#     ii. enrolment (by district, sector) ---------------------------------------------------------
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
  df <- read_excel("data/moe2018.xlsx", i$sheet)
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

#     iii. enrolment_cluster (MOE cluster) -------------------------------------------
# Initialize an empty list to store data frames
enrolment_MOE <- list()

for (i in 4:6) {
  df <- read_excel("data/moe2018.xlsx", i)
  colnames(df) <- df[2, ]
  df <- df %>% 
    slice(3:nrow(df))
  
  # Store the processed data frame in the list
  enrolment_MOE[[i]] <- df
}

# Combine all data frames into one
enrolment_MOE <- bind_rows(enrolment_MOE)
view(enrolment_MOE)





# TOPIC QUESTION ----------------------------------------------------------
# 1. School Distribution ------------------------------------------------------



# 2. Proximity to School from Home -------------------------------------------
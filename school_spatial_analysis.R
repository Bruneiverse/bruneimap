# 0. Libraries ---------------------------------------------------------------
library(tidyverse)
# remotes::install_github("propertypricebn/bruneimap")
library(bruneimap)
library(ggrepel)
library(kernlab)
library(osrm)
library(osmdata)
library(readxl)
library(sfhotspot)
library(sfdep)

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
sch <- read_excel("data/school listing.xlsx", 1)
sch_sf <- (st_as_sf(sch, coords = c("longitude", "latitude"), crs = 4326))
sch_sf <- st_join(sch_sf, kpg_sf, join = st_within)
view(sch_sf)

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
# a. points of all schools ------------------------------------------------
ggplot() +
  geom_sf(data = kpg_sf) +
  geom_sf(data = sch_sf)

# b. schools by kampong ---------------------------------------------------
sch_kpg <-
  sch_sf %>% 
  tibble() %>% 
  group_by(kampong) %>% 
  summarise("count_of_schools" = n())

kpg_sch <-
  kpg_sf %>% 
  left_join(sch_kpg) %>% 
  select(kampong, count_of_schools)

ggplot(kpg_sch, aes(count_of_schools)) +
  geom_histogram()

ggplot() +
  geom_sf(data = kpg_sf) +
  geom_sf(data = kpg_sch, 
          aes(fill = count_of_schools),
          alpha = 0.8,
          colour = NA) +
  scale_fill_viridis_b(direction = 1)

# c. schools by mukim -----------------------------------------------------
sch_mkm <-
  sch_sf %>% 
  tibble() %>% 
  group_by(mukim) %>% 
  summarise("count_of_schools" = n())

mkm_sch <-
  mkm_sf %>% 
  left_join(sch_mkm) %>% 
  select(mukim, count_of_schools)

ggplot(mkm_sch, aes(count_of_schools)) +
  geom_histogram()

ggplot() +
  geom_sf(data = mkm_sf) +
  geom_sf(data = mkm_sch, 
          aes(fill = count_of_schools),
          alpha = 0.8,
          colour = NA) +
  scale_fill_viridis_b(direction = 1)

# d. kde ------------------------------------------------------------------
sch_kde <- 
  sch_sf %>% 
  st_transform("EPSG:27700") %>% 
  hotspot_kde(cell_size = 3000, bandwidth = 50000, grid_type = "hex") %>% 
  st_intersection(st_transform(brn_sf, "EPSG:27700"))

ggplot() +
  geom_sf(data = kpg_sf) +
  geom_sf(data = sch_kde,
          aes(fill = kde),
          alpha = 0.8,
          colour = NA) +
  scale_fill_viridis_c(direction = -1) 

# maybe can consider changing the colout of low
# since overlap by darker colour

# e. kde by district ------------------------------------------------------
district <- c("Brunei Muara", "Belait", "Tutong", "Temburong")

for (i in district) {
  sch_kde <- 
    sch_sf %>% 
    filter(district == i) %>% 
    st_transform("EPSG:27700") %>% 
    hotspot_kde() %>% 
    st_intersection(st_transform(brn_sf, "EPSG:27700"))
  
  # consider filling up the remaining regions?
  print(
    ggplot(sch_kde, aes(x = kde)) +
      geom_histogram()
  )
  
  print(
    ggplot() +
      geom_sf(data = filter(mkm_sf, district == i)) +
      geom_sf(aes(fill = kde),
              data = sch_kde,
              alpha = 0.85,
              colour = NA) +
      scale_fill_viridis_c(direction = -1)
  )
}

# f. global moran ---------------------------------------------------------
# 2 types of neighbour: st_contiguity require polygon ;  st_knn require point
# there are different types of weights?

# Duplicate: for reference
# kpg_sch <-
#   kpg_sf %>% 
#   left_join(sch_kpg) %>% 
#   select(kampong, count_of_schools)

kpg_sch$count_of_schools[is.na(kpg_sch$count_of_schools)] <- 0

nb <- st_knn(st_centroid(kpg_sch), k = 5) # multiple islands in brunei so cannot use st_contiguity
wt <- st_weights(nb, style = "W")
global_moran_test(kpg_sch$count_of_schools, nb, wt)

# Reject H0, there is statistically significant clustering of schools

## ----- Moran's test ----------------------------------------------------------

# We can use the Moran's test to determine if there is spatial autocorrelation
# in the distribution of number of schools. Probably best to do this by Mukim.

# Checking that the kampong order is the same in both data sets
all(kpg_sch_df$kampong == kpg_sf$kampong)

kpg_sch_df <- st_set_geometry(kpg_sch, NULL)
kpg_sch_df$mukim <- kpg_sf$mukim
kpg_sch_df$district <- kpg_sf$district
mkm_sch_df <- 
  kpg_sch_df |>
  summarise(
    count = sum(count_of_schools),
    .by = mukim
  )

# Create the sf object, with count as a feature
mkm_sch_sf <-
  mkm_sf |>
  left_join(mkm_sch_df) |>
  mutate(
    count = case_when(
      count == 0 ~ NA_real_,
      TRUE ~ count
    )
  ) 

# Let's plot it first
ggplot(mkm_sch_sf) +
  geom_sf(aes(fill = count)) +
  # geom_sf_text(aes(label = count)) +
  scale_fill_viridis_c() +
  theme_bw()

# Now we can do the Moran's test
library(spdep)
mor_sf <- drop_na(mkm_sch_sf, count)  
nb <- poly2nb(mor_sf, row.names = mor_sf$mukim) 

# OPTIONAL: Connect Mukim Kota Batu to Mukim Labu (because of the bridge)
idx_kotabatu <- which(mor_sf$mukim == "Mukim Kota Batu")
idx_labu <- which(mor_sf$mukim == "Mukim Labu")
nb[[idx_kotabatu]] <- c(nb[[idx_kotabatu]], idx_labu)
nb[[idx_labu]] <- c(nb[[idx_labu]], idx_kotabatu)

lw <- nb2listw(nb)   
mt <- moran.test(mor_sf$count, lw)

# The moran's test is effectively conducting this hypothesis test:
#
# H0: There is no spatial correlation 
# H1: There is spatial correlation
#
# Results are I = 0.495, which is quite high, indicating positive spatial
# correlation. The p-value is < 0.001 so we reject the null hypothesis.

# Curious, plot the neighbours
library(sp)
mor_sp <- as(mor_sf, "Spatial")
nb_sf <- as(nb2lines(nb, coords = coordinates(mor_sp)), "sf")
nb_sf <- st_set_crs(nb_sf, st_crs(mor_sp)) 
ggplot() +
  geom_sf(data = mkm_sf) +
  geom_sf(data = nb_sf, col = "red3")

# 2. Proximity to School from Home -------------------------------------------
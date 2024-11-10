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
library(ggspatial)
library(sf) # some older version RStudio requires this
library(prettymapr) # some older version RStudio requires this

# 1. GIS - School---------------------------------------------------------------
#   A. Data Cleaning - 175 entries--------------------------------------
bb <- getbb("brunei darussalam", format_out = "polygon", featuretype = "country")
q <-
  opq(bb[2]) %>% 
  add_osm_feature(key = "amenity", value = "school") %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb[2]) # must be in data.frame format (section a1 for more details)

sch_sf <-
  q$osm_polygons %>% 
  as_tibble() %>% 
  st_as_sf() %>%  
  select(osm_id, name) %>% 
  drop_na() %>% 
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
sch_df <- read_csv("data/School listing.csv")
# only 71 entries from OSM query match MOE listing of 251 entries
sch_sf_match <- sch_sf[sch_sf$name %in% sch_df$School,]
sch_sf_match

# Issue: school names slight variation/abbreviated
# Solution: left join the matching entries, and fill up the remaining geometry manually
sch_df <- 
  left_join(sch_df, sch_sf_match, by = c("School" = "name")) %>% 
  select(School, Sector, Cluster, `Education Level`, geometry)

# Convert from list to sf object
sch_sf <- st_as_sf(sch_df)

# Extract coordinates
sch_sf$longitude <- st_coordinates(sch_sf)[,1]
sch_sf$latitude <- st_coordinates(sch_sf)[,2]

# Convert to data frame from sf
sch_sf <- st_drop_geometry(sch_sf) %>% data.frame()

# Extract (DO NOT RUN AGAIN)
# write.csv(sch_sf, "school listing v2.csv", row.names = FALSE)

# Manually add in remaining address using MS Excel
# New version saved as "school listing.xlsx"

#   C. Data Processing - join kpg_sf --------------------------------
sch_df <- read_excel("data/school listing.xlsx", 1)
sch_sf <- st_as_sf(sch_df, coords = c("longitude", "latitude"), crs = 4326)
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
  list(sheet = 10, sector = "private")
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

# EDA ---------------------------------------------------------------------- --------------------------- 
  # a. count of school by sector, cluster -------------------------------------------------------
  sch_df %>% 
    group_by(Sector) %>% 
    summarise(count = n())
  
  sch_df %>% 
    group_by(Cluster) %>% 
    summarise(count = n())
  
  ggplot() +
    annotation_map_tile(type = "osm", zoomin =  0, alpha = 0.8) +
    geom_sf(data = kpg_sf, fill = NA, col = "grey", lwd = 0.3) +
    geom_sf(data = mkm_sf, fill = NA, col = "black", lwd = 0.5) +
    geom_sf(data = dis_sf, fill = NA, col = "black", lwd = 1) +
    geom_sf(data = sch_sf %>% filter(Cluster != "NA"), 
            aes(col = Cluster, ),
            size = 2) +
    scale_color_brewer(palette = "Set2") +
    labs(x = NULL, 
         y = NULL, 
         col = NULL,
         caption = "© OpenStreetMap") +
    theme_bw()
  
  enrolment_MOE$M <- as.integer(enrolment_MOE$M)
  enrolment_MOE$F <- as.integer(enrolment_MOE$F)
  enrolment_MOE$Cls <- as.integer(enrolment_MOE$Cls)
  enrolment_MOE %>%
    mutate(student = enrolment_MOE$M + enrolment_MOE$F) %>% 
    group_by(Cluster) %>% 
    summarise(class = sum(Cls, na.rm = TRUE), student = sum(student))
  # class omited for Y12, 13
  
  # b. student teacher ratio ------------------------------------------------
  tchr$M <- as.integer(tchr$M)
  tchr$F <- as.integer(tchr$F)
  enrolment$M <- as.integer(enrolment$M)
  enrolment$F <- as.integer(enrolment$F)
  
  enrolment_v1 <-
    enrolment %>% 
    mutate(student = enrolment$M + enrolment$F) %>% 
    group_by(Sector, District) %>% 
    summarise(student = sum(student))
  
  tchr_v1 <-
    tchr %>% 
    mutate(teacher = tchr$M + tchr$F) %>% 
    group_by(Sector, District) %>% 
    summarise(teacher = sum(teacher))
  
  cbind(enrolment_v1, teacher = tchr_v1$teacher) %>% 
    mutate(stud_tchr_ratio = student/teacher) %>% 
    ggplot(aes(x = Sector, 
               y = stud_tchr_ratio, 
               fill = District)) +
    geom_col(position = "dodge") +
    labs(x = "sector",
         y = "student teacher ratio",
         fill = NULL) +
    scale_fill_viridis_d() +
    theme_bw() +
    theme(legend.position = "bottom") 
  
  cbind(enrolment_v1, teacher = tchr_v1$teacher) %>% 
    group_by(Sector) %>% 
    summarise(student = sum(student), teacher = sum(teacher)) %>% 
    mutate(stud_tchr_ratio = student/teacher)
  
  cbind(enrolment_v1, teacher = tchr_v1$teacher) %>% 
    group_by(District) %>% 
    summarise(student = sum(student), teacher = sum(teacher)) %>% 
    mutate(stud_tchr_ratio = student/teacher)
  
  
  # ggplot this with facets?
  
# TOPIC QUESTION: DISTRIBUTION --------------------------------------------- ----------------------------------------------------------------------------------------------------------
  # a. points of all schools ------------------------------------------------
  ggplot() +
    annotation_map_tile(type = "osm", zoomin =  0, alpha = 0.8) +
    geom_sf(data = kpg_sf, fill = NA, col = "grey", lwd = 0.3) +
    geom_sf(data = mkm_sf, fill = NA, col = "black", lwd = 0.5) +
    geom_sf(data = dis_sf, fill = NA, col = "black", lwd = 1) +
    geom_sf(data = sch_sf, size = 0.7) + 
    geom_label_repel(data = dis_sf,
                     aes(label = name, geometry = geometry),
                     stat = "sf_coordinates",
                     inherit.aes = FALSE,
                     box.padding = 6,
                     nudge_x = 15,
                     nudge_y = 20,
                     # hjust = 0.1,
                     # vjust = 0,
                     size = 5,
                     segment.colour = "#1f77b4",
                     segment.size = 0.8,
                     max.overlaps = Inf) +
    labs(x = NULL, y = NULL, caption = "© OpenStreetMap") +
    theme_bw() 

  # b. schools by kampong ---------------------------------------------------
  sch_kpg <-
    sch_sf %>% 
    tibble() %>% 
    group_by(kampong) %>% 
    summarise("count_of_schools" = n())
  
  kpg_sch <-
    kpg_sf %>% 
    left_join(sch_kpg) %>% 
    select(kampong, mukim, count_of_schools)
  
  kpg_sch_labels <-
    kpg_sch %>% 
    arrange(desc(count_of_schools)) %>% 
    slice_head(n = 10)
  
  ggplot(kpg_sch, aes(count_of_schools)) +
    geom_histogram()
  
  ggplot() +
    annotation_map_tile(type = "osm", zoomin =  0, alpha = 0.6) +
    #geom_sf(data = dis_sf, fill = NA, lwd = 1) +
    geom_sf(data = kpg_sch, aes(fill = count_of_schools), alpha = 0.8) +
    geom_sf(data = kpg_sf, fill = NA, col = "black") +
    geom_label_repel(data = kpg_sch_labels,
                     aes(label = kampong, geometry = geometry),
                     stat = "sf_coordinates",
                     inherit.aes = FALSE,
                     box.padding = 1.5,
                     size = 2,
                     segment.colour = "#1f77b4",
                    # segment.size = 0.8,
                     max.overlaps = Inf) +
    scale_fill_viridis_b(name = "Count of Schools", na.value = NA) +
    labs(x = NULL, y = NULL, caption = "© OpenStreetMap") +
    theme_bw()
  
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
  
  mkm_sch_labels <-
    mkm_sch %>% 
    arrange(desc(count_of_schools)) %>% 
    slice_head(n = 10)
  
  ggplot(mkm_sch, aes(count_of_schools)) +
    geom_histogram()
  
  ggplot() +
    annotation_map_tile(type = "osm", zoomin =  0, alpha = 0.6) +
    geom_sf(data = mkm_sch, aes(fill = count_of_schools), col = "grey", lwd = 0.6) +
    # geom_sf(data = dis_sf, fill = NA, lwd = 1, col = "black") +
    geom_label_repel(data = mkm_sch_labels,
                     aes(label = mukim, geometry = geometry),
                     stat = "sf_coordinates",
                     inherit.aes = FALSE,
                     box.padding = 1,
                     size = 3,
                     segment.colour = "#1f77b4",
                     segment.size = 0.8,
                     max.overlaps = Inf) +
    scale_fill_viridis_b(name = "Count of Schools", na.value = NA) +
    labs(x = NULL, y = NULL, caption = "© OpenStreetMap") +
    theme_bw()
  
  # d. global moran by mukim ------------------------------------------------
  mkm_sch$count_of_schools[is.na(mkm_sch$count_of_schools)] <- 0
  
  nb <- st_contiguity(mkm_sch)
  wt <- st_weights(nb)
  global_moran_test(mkm_sch$count_of_schools, nb, wt)
  
  # e. lisa - getis ord -----------------------------------------------------
  sch_gi <- 
    sch_sf %>% 
    st_transform("EPSG:27700") %>%  # sfhotspot rejects 4326 format
    hotspot_gistar() %>%  
    filter(gistar > 0, pvalue < 0.05) %>% 
    st_intersection(
      st_union(dis_sf) %>% 
      st_transform("EPSG:27700")
    )

  sch_gi_labels <-
    mkm_sf %>% 
      filter(mukim %in% c("Mukim Kuala Belait",
                          "Mukim Seria",
                          "Mukim Telisai",
                          "Mukim Pekan Tutong",
                          "Mukim Bangar"))

  ggplot(sch_gi) +
    geom_histogram(aes(kde))
  
  ggplot() +
    annotation_map_tile(type = "osm", zoomin =  0, alpha = 0.6) +
    geom_sf(data = kpg_sf, fill = NA, col = "grey", lwd = 0.1) +
    geom_sf(data = mkm_sf, fill = NA, col = "black", lwd = 0.3) +
    geom_sf(data = dis_sf, fill = NA, col = "black", lwd = 0.5) +
    geom_sf(data = sch_gi, aes(fill = kde), alpha = 0.8, col = NA) +
    geom_label_repel(data = sch_gi_labels,
                     aes(label = mukim, geometry = geometry),
                     stat = "sf_coordinates",
                     inherit.aes = FALSE,
                     box.padding = 1,
                     direction = "y",
                     size = 2,
                     segment.size = 0.8,
                     max.overlaps = Inf) +
    scale_fill_gradient(name = "Hotspot intensity",
                        low = "orange",
                        high = "darkred",
                        breaks = range(pull(sch_gi, kde)),
                        labels = c("Hot", "Very Hot")) +
    labs(x = NULL, 
         y = NULL,
         #title = "School Hotspots",
         #subtitle = "density of clustered schools (more than expected by chance)",
         caption = "© OpenStreetMap") +
    theme_bw()
  
  # f. brunei population by kampong---------------------------------------------------------------
  bn_pop_sf <- left_join(kpg_sf, bn_census2021, by = join_by(id, kampong, mukim, district))
  
  bn_pop_labels <-
    bn_pop_sf %>% 
    arrange(desc(population)) %>% 
    slice_head(n = 10)
  
  ggplot() +
    annotation_map_tile(type = "osm", zoomin =  0, , alpha = 0.6) +
    #geom_sf(data = dis_sf, fill = NA, lwd = 1) +
    geom_sf(data = bn_pop_sf, aes(fill = population), col = NA, alpha = 0.8) +
    geom_sf(data = kpg_sf, fill = NA, col = "black") +
    geom_label_repel(
      data = bn_pop_labels,
      aes(label = kampong, geometry = geometry),
      stat = "sf_coordinates",
      inherit.aes = FALSE,
      box.padding = 1,
      size = 2,
      #segment.colour = "#1f77b4",
      #segment.size = 0.6,
      max.overlaps = Inf
    ) +
    scale_fill_viridis_b(
      name = "Population",
      na.value = NA,
      labels = scales::comma,
      breaks = c(0, 100, 1000, 10000, 20000)
    ) +
    labs(x = NULL, y = NULL, caption = "© OpenStreetMap") +
    theme_bw()

  # g. brunei population by mukim -----------------------------------------------------------
  bn_pop_mkm_sf <-
    bn_pop_sf %>%
    group_by(mukim) %>% 
    summarise(population = sum(population, na.rm = TRUE))
  
  bn_pop_mkm_labels <-
    bn_pop_mkm_sf %>% 
    arrange(desc(population)) %>% 
    slice_head(n = 10)
  
  view(bn_pop_mkm_sf %>% 
         arrange(desc(population)))
    
  ggplot() +
    annotation_map_tile(type = "osm", zoomin =  0, , alpha = 0.6) +
    geom_sf(data = dis_sf, fill = NA, lwd = 1) +
    geom_sf(data = bn_pop_mkm_sf, aes(fill = population), col = NA, alpha = 0.8) +
    # geom_sf(data = kpg_sf, fill = NA, col = "black") +
    geom_label_repel(
      data = bn_pop_mkm_labels,
      aes(label = mukim, geometry = geometry),
      stat = "sf_coordinates",
      inherit.aes = FALSE,
      box.padding = 1,
      size = 5,
      segment.size = 0.8,
      max.overlaps = Inf
    ) +
    scale_fill_viridis_b(
      name = "Population",
      na.value = NA,
      labels = scales::comma,
      breaks = c(0, 100, 1000, 10000, 35000)
    ) +
    labs(x = NULL, y = NULL, caption = "© OpenStreetMap") +
    theme_bw()

# h. top 10 mkm, kpg with highest pop and sch --------------------------------
  bn_pop_labels %>% 
    select(kampong, population)
  
  bn_pop_sf %>% 
    group_by(mukim) %>% 
    summarise(pop = sum(population, na.rm = TRUE)) %>% 
    arrange(desc(pop)) %>% 
    slice_head(n = 10)
  
  kpg_sch_labels
  mkm_sch_labels
  
## Backup ------------------------------------------------------------------ -------------------------------------------------------------------------------------------
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
  # i. kpg - st_knn (5 nearest neighbour) -----------------------------------
  # Duplicate 1b : for reference
  # kpg_sch <-
  #   kpg_sf %>% 
  #   left_join(sch_kpg) %>% 
  #   select(kampong, count_of_schools)
  
  # replace NA with 0
  kpg_sch$count_of_schools[is.na(kpg_sch$count_of_schools)] <- 0
  
  nb <- st_knn(st_centroid(kpg_sch), k = 5)
  wt <- st_weights(nb, style = "W")
  global_moran_test(kpg_sch$count_of_schools, nb, wt)
  
  # Reject H0, there is statistically significant (minor) clustering of schools
  # moran value >0 clustered; <0 dispersed
  # z,p value for hyp test
  
  # ii. kpg - st_contiguity (literal neighbour) -----------------------------
  nb <- st_contiguity(st_geometry(kpg_sch)) 
  wt <- st_weights(nb, style = "W")
  global_moran_test(kpg_schools$count_of_schools, nb, wt)
  
  # error: empty neighbour
  # Is the issue: multiple islands in brunei so cannot use st_contiguity?
  # fix: add one kota batu point to (polygon) of mukim labu?
  mkm_sf %>% 
    filter(mukim == "Mukim Kota Batu" | mukim == "Mukim Labu") %>% 
    st_geometry()
  
  # *** BUT st_contiguity works for mkm (despite having 2 sub-graphs?)

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

# The above plot seems to be spatially dependent. If schools are not spatially
# dependent, it would look something like this:
mkm_sch_sf |>
  mutate(rand_count = sample(count)) |>
  ggplot() +
  geom_sf(aes(fill = rand_count)) +
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

# 0. Libraries ---------------------------------------------------------------
library(tidyverse)
library(bruneimap)
library(readxl)
library(sfhotspot)
library(sf) # some older version RStudio requires this
library(sfdep)
library(sp)
library(spdep)
library(ggspatial)
library(prettymapr) # some older version RStudio requires this
library(mapview)
library(leaflet)
library(leaflet.extras2)
# Reorder OpenStreetMap as the first one 
all_basemaps <- c("OpenStreetMap", 
                  "CartoDB.Positron",  
                  "CartoDB.DarkMatter", 
                  "Esri.WorldImagery", 
                  "Esri.WorldStreetMap") 
# Set default basemap to OSM
mapviewOptions(basemaps = all_basemaps)

# 1. Load Data ------------------------------------------------------------
# schools data were previously obtained using combination of osm query and manual data entry
sch_df <- read_excel("data/schools_2018.xlsx", 1)
sch_sf <- st_as_sf(sch_df, coords = c("longitude", "latitude"), crs = 4326)
sch_sf <- st_join(sch_sf, kpg_sf, join = st_within)

tchr <- read_excel("data/schools_2018.xlsx", 2)
enrolment <- read_excel("data/schools_2018.xlsx", 3)
enrolment_moe <- read_excel("data/schools_2018.xlsx", 4)

# EDA ---------------------------------------------------------------------- --------------------------- 
  # a. points of all school (by sector & cluster)-------------------------------------------------------
  sch_df %>% 
    group_by(sector) %>% 
    summarise(count = n())
  
  # plot
  mapview(sch_sf, zcol = "sector", layer.name = "Sector") + 
    mapview(sch_sf, zcol = "cluster", layer.name = "MOE Cluster")
  
  # school by cluster
  enrolment_moe %>%
    mutate(
      class = as.integer(class), # Convert class to integer
      student = male + female    # Calculate total students
    ) %>% 
    group_by(cluster) %>% 
    summarise(
      class = sum(class, na.rm = TRUE), # Summarize class (handling NA values)
      student = sum(student)           # Summarize total students
    ) %>% 
    left_join(
      sch_df %>% 
        group_by(cluster) %>% 
        summarise(school = n()), # Count schools per cluster
      by = "cluster"             # Specify join column
    )
  
  # b. student teacher ratio ------------------------------------------------
  enrolment_temp <-
    enrolment %>% 
    mutate(student = enrolment$male + enrolment$female) %>% 
    group_by(sector, district) %>% 
    summarise(student = sum(student))
  
  tchr_temp <-
    tchr %>% 
    mutate(teacher = tchr$male + tchr$female) %>% 
    group_by(sector, district) %>% 
    summarise(teacher = sum(teacher))
  
  # by District
  cbind(enrolment_temp, teacher = tchr_temp$teacher) %>% 
    group_by(district) %>% 
    summarise(student = sum(student), teacher = sum(teacher)) %>% 
    mutate(stud_tchr_ratio = student/teacher)
  
  # by Sector
  cbind(enrolment_temp, teacher = tchr_temp$teacher) %>% 
    group_by(sector) %>% 
    summarise(student = sum(student), teacher = sum(teacher)) %>% 
    mutate(stud_tchr_ratio = student/teacher)
  
  # plot
  cbind(enrolment_temp, teacher = tchr_temp$teacher) %>% 
    mutate(stud_tchr_ratio = student/teacher) %>% 
    ggplot(aes(x = sector, 
               y = stud_tchr_ratio, 
               fill = district)) +
    geom_col(position = "dodge") +
    labs(x = "sector",
         y = "student teacher ratio",
         fill = NULL) +
    scale_fill_viridis_d() +
    theme_bw() +
    theme(legend.position = "bottom") 
  
  # c. schools by kampong (village) ---------------------------------------------------
  sch_kpg <-
    sch_sf %>% 
    tibble() %>% 
    group_by(kampong) %>% 
    summarise("count_of_schools" = n())

  kpg_sch <-
    kpg_sf %>% 
    left_join(sch_kpg) %>% 
    select(kampong, mukim, count_of_schools) 
  
  mapview(kpg_sch, 
          zcol = "count_of_schools", 
          layer.name = "School Count",
          label = "kampong")
  
  # d. schools by mukim (county) -----------------------------------------------------
  sch_mkm <-
    sch_sf %>% 
    tibble() %>% 
    group_by(mukim) %>% 
    summarise(count_of_schools = n())

  mkm_sch <-
    mkm_sf %>% 
    left_join(sch_mkm) %>% 
    select(mukim, count_of_schools)
  
  # plot
  mapview(mkm_sch, 
          zcol = "count_of_schools", 
          layer.name = "School Count",
          label = "mukim")
  
  # Hypothetical map if not clustered (random shuffle)
  mkm_sch %>% 
    mutate(rand_count = sample(count_of_schools)) %>% 
    mapview(zcol = "rand_count", layer.name = "School count")
  
  # e. neighbour ------------------------------------------------------------
  mkm_sch$count_of_schools[is.na(mkm_sch$count_of_schools)] <- 0
  
  nb <- st_contiguity(mkm_sch)
  mkm_sp <- as(mkm_sf, "Spatial")
  nb_sf <- as(nb2lines(nb, coords = coordinates(mkm_sp)), "sf")
  nb_sf <- st_set_crs(nb_sf, st_crs(mkm_sp)) 
  
  mapview(dis_sf, 
          alpha.region = 0, 
          layer.name = "Districts", 
          color = "black") + 
    mapview(mkm_sf, 
            alpha.region = 0, 
            color = "black", 
            layer.name = "Mukims") +
    mapview(kpg_sf, 
            alpha.region = 0, 
            color = "grey", 
            layer.name = "Kampongs") +
    mapview(nb_sf, 
            color = "red3", 
            layer.name = "Neighbors") 
  
# RESEARCH QUESTION: DISTRIBUTION ------------------------------------------ --------------------------------------------- 
  # a. global moran by mukim ------------------------------------------------
  mkm_sch$count_of_schools[is.na(mkm_sch$count_of_schools)] <- 0
  nb <- st_contiguity(mkm_sch)
  wt <- st_weights(nb)
  global_moran_test(mkm_sch$count_of_schools, nb, wt)
  
  # b. lisa - getis ord -----------------------------------------------------
  sch_gi <- 
    sch_sf %>% 
    st_transform("EPSG:27700") %>%  # sfhotspot rejects 4326 format
    hotspot_gistar() %>%  
    filter(gistar > 0, pvalue < 0.05) %>% 
    st_intersection(
      st_union(dis_sf) %>% 
      st_transform("EPSG:27700")
    )
  
  color_palette <-
    colorNumeric(
      palette = c("orange", "red", "darkred"),
      domain = range(sch_gi$gistar)
    )
  
  sch_gi %>% 
    st_transform("EPSG:4326") %>% 
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>% 
      addPolygons(
        fillColor = ~color_palette(gistar),
        fillOpacity = 0.7,
        weight = 0,
        label = ~paste("Gistar:", gistar)
      ) %>%
      addPolygons(
        data = mkm_sf,
        fillOpacity = 0, # to enable display of mukim when hover
        color = "black",
        weight = 1,
        label = ~mkm_sf$mukim
      ) %>%
      addLegend(
        pal = color_palette,
        values = ~gistar,
        title = "Hotspot Intensity",
        labFormat = function(type, cuts, p) {
          labels = c("Hot", "Very Hot")
          labels },
        opacity = 1
      )  

  # c.1 brunei population by mukim -----------------------------------------------------------
  bn_pop_kpg_sf <- left_join(kpg_sf, 
                         bn_census2021, 
                         by = join_by(id, kampong, mukim, district))

  bn_pop_mkm_sf <-
    bn_pop_kpg_sf %>%
    #drop_na() %>% 
    group_by(mukim) %>% 
    summarise(population = sum(population, na.rm = TRUE))
  
  mapview(
    bn_pop_mkm_sf,
    zcol = "population",
    label = "mukim",
    layer.name = "population"
  )

  # c.2 linear regression on pop and sch count (by mkm)----------------------------------
  pop_sch_mod_mkm <- 
    mkm_sch %>%
    tibble() %>%
    select(mukim, count_of_schools) %>% 
    left_join(bn_pop_mkm_sf) %>% 
    select(mukim, count_of_schools, population)

  fit <- lm(count_of_schools ~ population, data = pop_sch_mod_mkm)
  summary(fit)
  
  # plot
  ggplot(pop_sch_mod_mkm, aes(x = population, y = count_of_schools)) +
    geom_point() +  # Plot the data points
    geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add the regression line
    labs(x = "Population",
         y = "Count of Schools") +
    theme_bw()
  
  # c.3 brunei population by kampong---------------------------------------------------------------
  mapview(bn_pop_kpg_sf,
          zcol = "population",
          label = "kampong",
          layer.name = "population") |
        mapview(kpg_sch, 
          zcol = "count_of_schools", 
          label = "kampong", 
          layer.name = "schools")
  
  # c.4 top 10 highest population & schools--------------------------------
  # by kampong
  bn_pop_kpg_sf %>% 
    arrange(desc(population)) %>% 
    slice_head(n = 10) %>% 
    select(kampong, population)
  
  kpg_sch %>% 
    arrange(desc(count_of_schools)) %>% 
    slice_head(n = 10) %>% 
    select(kampong, count_of_schools)
  
  # by mukim
  bn_pop_mkm_sf %>% 
    arrange(desc(population)) %>% 
    slice_head(n = 10) %>% 
    select(mukim, population)
  
  mkm_sch %>% 
    arrange(desc(count_of_schools)) %>% 
    slice_head(n = 10) %>% 
    select(mukim, count_of_schools)
  
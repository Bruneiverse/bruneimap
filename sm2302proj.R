library(tidyverse)
library(bruneimap)
library(ggrepel)
library(kernlab)
library(osrm)
library(osmdata)

# Spatial data analysis of Brunei data

## Mosques coverage in Brunei Darussalam

### Brunei map with district outline

ggplot(dis_sf) +
  geom_sf()

### Bounding box for Brunei district

dis_bbox <- st_bbox(dis_sf)

z <-
  opq(dis_bbox) |>
  add_osm_feature(
    key = c("amenity", "religion") ,
    value = c("place_of_worship" , "muslim")
  ) |>
  osmdata_sf()

mosques_sf1 <-
  z$osm_polygons |>
  as_tibble() |>
  st_as_sf() |> 
  select(osm_id, name) |>
  drop_na() |>
  st_centroid()
print(mosques_sf1)

### slice unwanted rows 

mosques_sf2 <- slice(.data = mosques_sf1 , -c(18,31,32,34,35,58,83,84,91,99,100))
print(mosques_sf2)

### There are some mosques that are not in osm and has to be done manually

mosques <- data.frame(
  name = c("Masjid Mohammad Jamalul Alam", 
           "Balai Ibadat Kampong Melilas" , 
           "Masjid STKRJ Lorong 3" , 
           "Masjid Pekan Seria" , 
           "Balai Ibadat Kampong Merangking" , 
           "Balai Ibadat Kampong Sukang" , 
           "Masjid Al-Mashor" , 
           "Masjid Kampong Bukit Sawat" , 
           "Masjid Kampong Labi") , 
  longitude = c(114.1904996, 
                114.66090496335256, 
                114.32838209241287, 
                114.33427073237323, 
                114.57863288262956, 
                114.62519519612009, 
                114.49019888448353, 
                114.55027519241261, 
                114.46509197150523) ,     
  latitude = c(4.5852151 , 
               4.257895216459671, 
               4.6007070520941555, 
               4.615941446349221, 
               4.5225697452027065, 
               4.31158865906157, 
               4.555390188254131, 
               4.523612935865245, 
               4.418724155874466)      
)
print(mosques)

### Convert df to sf object

mosques_sf3 <- st_as_sf(
  mosques, 
  coords = c("longitude", "latitude"),
  crs = 4326                           
)

### Add a radius column to the data frame

mosques_sf2$radius <- c(
  3000, 3000, 4000, 2000, 5000, 3000, 2000, 2000, 2000, 4000, 
  2000, 3000, 2000, 2000, 3000, 2000, 2000, 2000, 2000, 1000,
  3000, 2000, 2000, 3000, 2000, 2000, 1000, 2000, 2000, 2000,
  1000, 1000, 3000, 2000, 2000, 2000, 2000, 3000, 2000, 2000,
  2000, 2000, 2000, 1000, 2000, 2000, 3000, 3000, 3000, 2000,
  2000, 3000, 2000, 2000, 2000, 1000, 2000, 2000, 2000, 2000,
  3000, 1000, 2000, 2000, 2000, 1000, 1000, 3000, 2000, 2000,
  1000, 1000, 2000, 3000, 3000, 2000, 1000, 2000, 1000, 2000,
  2000, 2000, 2000, 1000, 3000, 2000, 2000, 2000, 1000
)

mosques_sf3$radius <- c(
  2000, 1000, 2000, 2000, 1000, 1000, 2000, 2000, 2000)  # Radii in meters

### Create buffers using the radius for each location

buffer_sf1 <- st_buffer(mosques_sf2, dist = mosques_sf2$radius)
buffer_sf2 <- st_buffer(mosques_sf3, dist = mosques_sf3$radius)

### Plotting mosques on Brunei map

ggplot() +
  geom_sf(data = dis_sf, aes(fill = name), alpha = 0.3) +
  geom_sf(data = mosques_sf2, color = "black", size = 1.5) + 
  geom_sf(data = buffer_sf1, fill = "blue", alpha = 0.3, color = "darkblue", 
          linetype = "dashed") + 
  geom_sf(data = mosques_sf3, color = "black", size = 1.5) +
  geom_sf(data = buffer_sf2, fill = "blue", alpha = 0.3, color = "darkblue", 
          linetype = "dashed") +
  labs(title = "Mosques of Brunei" , subtitle = "with radius covering land") +
  theme_minimal()
<<<<<<< HEAD

=======
>>>>>>> 4eaa8c0a7fcff7a412c4e34cc66ba2105abf033f

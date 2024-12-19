
# Finding which Hospitals are the nearest to corresponding Schools --------


# turn datas into sf
sch_sff <- st_as_sf(schools_sf,
                    coords = c("longitude", "latitude"),
                    crs = 4326)

hospitals_sf_all <- st_as_sf(rh,
                             coords = c("Longitude", "Latitude"),
                             crs = 4326)

#set distance from each schools to each hospitals
dist_matrix <- st_distance(sch_sff, hospitals_sf_all)

#set the closest 
closest_hospital <- apply(dist_matrix, 1, function(x) hospitals_sf_all$Hospitals [which.min (x)])
sch_sff$closest_hospital <- closest_hospital


sch_hos <- data.frame(School = sch_sff$name,
                      Closest_Hospital = closest_hospital)

#change back to df
sch_hos_min_dis_df <- data.frame(
  School = sch_sff$name,
  Closest_Hospital = closest_hospital,
  stringsAsFactors = FALSE 
)

sch_hos_min_dis<- sch_hos_min_dis_df %>%
  arrange(School) %>%
  mutate(School = as.character(School),
         Closest_Hospital = as.character(Closest_Hospital))

view(sch_hos_min_dis)




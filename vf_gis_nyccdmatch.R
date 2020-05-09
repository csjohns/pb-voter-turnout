### geocoding to assign unassigned voters to NYCCD
library(sf)

# extract voters w/no cd
nocds <- voterfile %>% filter(is.na(CityCouncilName)) %>% select(`Voter File VANID`, Latitude, Longitude) %>% filter(!is.na(Latitude))
nocds <- st_as_sf(nocds, coords = c("Longitude", "Latitude"))

# load nyc council districts
cdsf <-  st_read("shapefiles/nycc_18c/nycc.shp", stringsAsFactors = FALSE)

# align crs
st_crs(nocds) <- 4326
nocds <- st_transform(nocds, st_crs(cdsf))

# points in polygon join
nyccdassign <- st_join(nocds, cdsf, join = st_intersects)

nyccdassign[, c("Shape_Leng", "Shape_Area", "geometry")] <- NULL
nyccdassign <- nyccdassign %>% 
  st_drop_geometry() %>% 
  select(-starts_with("Shape_")) %>% 
  mutate(CounDist = as.integer(CounDist))
voterfile <- voterfile %>% left_join(nyccdassign)
voterfile <- voterfile %>% 
  mutate(CityCouncilName = coalesce(CityCouncilName, CounDist)) %>% 
  select(-CounDist)

rm(nyccdassign, nocds, cdsf)

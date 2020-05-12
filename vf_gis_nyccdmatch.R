sample1_without_na <- function(x){
  x <- na.omit(x)
  if (length(x) > 1) {
    sample(as.vector(x), 1) }
  else if (length(x) == 1) {
    x
  } else {NA}
}

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

## fill in the districts still missing based on ED crosswalk
nyccds <- read.csv("ed-nyccd-map.csv", as.is = TRUE)
nyccds <- nyccds %>% 
  mutate(ED = paste0("Ad ", str_sub(ElectDist, 1, 2), " - Ed ", str_sub(ElectDist, 3,5)))

voterfile_missing <- voterfile %>% 
  filter(is.na(CityCouncilName)) %>% 
  select(`Voter File VANID`, PrecinctName, CityCouncilName) %>% 
  mutate(NYCCD = ifelse(CityCouncilName == "", NA, CityCouncilName),
         ED = ifelse(PrecinctName == "", NA, PrecinctName))

nyccds <- full_join(nyccds, data.frame(ED = as.vector(na.omit(unique(voterfile_missing$ED))), stringsAsFactors = FALSE))

for (i in 1:nrow(voterfile_missing)){
  if(is.na(voterfile_missing$NYCCD[i])){
    voterfile_missing$NYCCD[i] <- sample1_without_na(nyccds$CounDist[nyccds$ED == voterfile_missing$ED[i]])
  }
}

voterfile <- voterfile_missing %>% 
  select(`Voter File VANID`, NYCCD) %>% 
  left_join(voterfile, ., by = "Voter File VANID") %>% 
  mutate(CityCouncilName = coalesce(CityCouncilName, NYCCD)) %>% 
  select(-NYCCD)

rm(nyccds)

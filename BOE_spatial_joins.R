##############################################################################################################################
###
### NYC PB Voters Project 
### Geocoding voters to their earlier political districts (2000 census era) for matching to competetiveness data
### Carolina Johnson
### 02/02/2020
###
### Code to attache earlier political districts to full 2018 voterfile.  Voters without a NYC lat long at the time 
### of the voterfile pull have NYC location imputed from the election district where they report returns (using 2018 ED files)
###
##############################################################################################################################

# Load libraries
library(dplyr)
library(tidyr)
library(data.table)
library(sf)
library(tmap)

# tmap_mode("view") # toggle from default "plot" to "view" for interactive maps

### First: load boundary shapefiles ----
# plotting code for review commented out

edshape2018 <- read_sf("shapefiles/nyed_18b/nyed.shp")
# ed has validity issues
edshape2018 <- edshape2018 %>% 
  lwgeom::st_make_valid() %>% 
  sf::st_collection_extract() 
# qtm(edshape2018)
cdshape <- read_sf("shapefiles/nycg_09a_av/nycg.shp")
# qtm(cdshape)
adshape <- read_sf("shapefiles/nyad_08aav/nyad.shp")
# qtm(adshape)
sdshape <- read_sf("shapefiles/nyss_09a_av/nyss.shp")
# qtm(sdshape)
ccshape <- read_sf("shapefiles/nycc_09a_av/nycc.shp")
# qtm(ccshape)

# city border
boroughs <- read_sf("shapefiles/borough.geojson")
city <- st_union(boroughs)

## have verified all political borders are same CRS, convert city to match
pfs <- st_crs(ccshape)
city <- st_transform(city, pfs)


### Second: read in voterfile - limit to relevant columns ----
# read in only sample for testing:
# top1000 <-  fread("PersonFile20180426-11056504994/PersonFile20180426-11056504994.txt", nrows = 1000)
# voterfile <- top1000

voterfile <- fread("PersonFile20180426-11056504994/PersonFile20180426-11056504994.txt")
voterfile <- voterfile[,c("Voter File VANID", "Latitude", "Longitude", "PrecinctName")]
voterfile <- as.data.frame(voterfile)
voterfile_sf <- select(voterfile, VANID=`Voter File VANID`, Latitude, Longitude)

### convert voterfile to sf  ---
voterfile_sf <- voterfile_sf %>% 
  drop_na() %>% 
  st_as_sf(coords = c("Longitude", "Latitude"))

# set geographic CRS
st_crs(voterfile_sf) <- 4326

# tranform to NYC crs
voterfile_sf <- st_transform(voterfile_sf, pfs)

### Third: Prep for spatial join district files to voterfile ----
## 1. split VF to NYC/not NYC points ----
nyc <- st_intersection(voterfile_sf, city)
# tm_shape(city) + tm_borders() +
  # tm_shape(nyc) + tm_dots()

not_nyc <- voterfile_sf %>% filter(!VANID %in% nyc$VANID)
# tm_shape(city) + tm_borders() +
  # tm_shape(not_nyc, is.master = TRUE) + tm_dots(col = "red") +
  # tm_shape(nyc) + tm_dots(col = "blue")

# remove voterfile_sf to save space
rm(voterfile_sf)

## 2. remove geometry for not NYC----
# remove geometry, add ED to nyc
ed_clean <- function(x) {
  stringr::str_extract_all(x, "[0-9]") %>% sapply(paste0,collapse = "")
}

not_nyc <- voterfile %>% 
  select(VANID = `Voter File VANID`, ED = PrecinctName) %>% 
  semi_join(not_nyc) %>% 
  mutate(ED_clean = as.numeric(ed_clean(ED))) %>% 
  select(-ED) %>% 
  as_tibble()

# remove voterfile to save space
rm(voterfile)
## 3. create ED sfdf with centroids----
ed_center <- st_centroid(edshape2018)

## 4. join to notNYC voterfile based on ED----
not_nyc <- right_join(ed_center, not_nyc, by = c("ElectDist" = "ED_clean"))

#check that now in nyc
tm_shape(city) + tm_borders() +
  tm_shape(not_nyc, is.master = TRUE) + tm_dots(col = "red") 

## 5. rbind vf back together----
voterfile_sf <- not_nyc %>% 
  select(VANID) %>% 
  rbind(nyc) 

#quick check plot
# st_geometry(voterfile_sf) %>% plot()

### Fourth: Join vf to all boundary geoms ------- )
# council district
voterfile_sf <- ccshape %>% 
  select(CounDist) %>% 
  st_join(voterfile_sf, .) 
# head(voterfile_sf)
# 
# ## it works!
# tm_shape(ccshape) + tm_polygons("CounDist", alpha = .2) + tm_text("CounDist", col = "blue", alpha = .2)  +
#   tm_shape(voterfile_sf)  + tm_text("CounDist")

# ad district
voterfile_sf <- adshape %>% 
  select(AssemDist) %>% 
  st_join(voterfile_sf, .)
# head(voterfile_sf)
# tm_shape(adshape) + tm_polygons("AssemDist", alpha = .2) + tm_text("AssemDist", col = "blue", alpha = 1)  +
#   tm_shape(voterfile_sf)  + tm_text("AssemDist", alpha = .5)

# cd district
voterfile_sf <- cdshape %>% 
  select(CongDist) %>% 
  st_join(voterfile_sf, .)
# head(voterfile_sf)
# tm_shape(cdshape) + tm_polygons("CongDist", alpha = .2) + tm_text("CongDist", col = "blue", alpha = 1)  +
#   tm_shape(voterfile_sf)  + tm_text("CongDist", alpha = .5) ## whoa, gerrymandering is gross!!!

# sd district
voterfile_sf <- sdshape %>% 
  select(StSenDist) %>% 
  st_join(voterfile_sf, .)
# head(voterfile_sf)
# tm_shape(sdshape) + tm_polygons("StSenDist", alpha = .2) + tm_text("StSenDist", col = "blue", alpha = 1)  +
#   tm_shape(voterfile_sf)  + tm_text("StSenDist", alpha = .5)

### Finally, save to disk ----
st_drop_geometry(voterfile_sf) %>% 
  saveRDS( "voterfile_oldgeo.RDS")


  
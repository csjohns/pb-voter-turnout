

### Geocoding to add census tracts for voterfile records missing it, based on lat long
## File gets saved for reload - eventually should get combined with the NYCCD attachments that happen earlier
## End of script does some comparison to the tracts as exist inthe original file. Confirm that known tracts are 
##  basically consistent with my new geocoded tracts; my geocoding just gets more, apparently? (maybe have a cleaner shapefile now)


library(dplyr)
library(tidyr)
library(sf)
library(data.table)
library(tigris)
library(mapview)

voterfile_sf <- fread("PersonFile20180426-11056504994/PersonFile20180426-11056504994.txt")
voterfile_sf <- voterfile_sf[,c("Voter File VANID", "Latitude", "Longitude", "CensusTractName")]
voterfile_sf <- as.data.frame(voterfile_sf)
# voterfile_sf <- select(voterfile, VANID=`Voter File VANID`, Latitude, Longitude, CensusTractName)


voterfile_sf <- voterfile_sf %>% 
  filter(!is.na(Longitude)) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"))

# set geographic CRS
st_crs(voterfile_sf) <- 4326

#B03002 - race
#B19013 - medhhinc
#B15003 - educ

tracts <-tracts(state = "NY", county = c("BRONX", "KINGS", "NEW YORK", "QUEENS", "RICHMOND"), year = 2010, class = "sf")
# tracts <- st_as_sf(tracts)
voterfile_sf <- st_transform(voterfile_sf, crs = st_crs(tracts))
voterfile_sf <- st_join(voterfile_sf, tracts)

### exploration of the voterfile data suggests that there are a bunch of census tracts for latlongs
### that exist in nyc but don't appear to be valid tract IDS.  I'm probably going to use the new tract ids I've created

### also now testing counties
counties <- counties(state = "NY", year = 2010, class = "sf")
counties %>% filter(toupper(NAME10) %in% c("BRONX", "KINGS", "NEW YORK", "QUEENS", "RICHMOND"))

voterfile_sf <- counties %>% select(countyname = NAME10) %>% st_join(voterfile_sf, .)

voterfile_sf %>% 
  select(VANID = `Voter File VANID`, CensusTract = TRACTCE10, countycode = COUNTYFP10) %>% 
  st_drop_geometry() %>% 
  saveRDS("data/cleaned_R_results/voters_census.rds")


#### comparing to voterfile from rr_vf_processing.R
voterfile_sf <- readRDS("data/cleaned_R_results/voters_census")
voterfile_sf <- rename(voterfile_sf, 
                       "CensusTractUpdate" = "CensusTract",
                       countycodeupdate = "countycode")

voterfile <- voterfile %>% filter(County %in% c("BRONX", "KINGS", "NEW YORK", "QUEENS", "RICHMOND")) %>% 
  mutate(countycode = recode(County, BRONX = "005", KINGS = "047", `NEW YORK` = "061", QUEENS = "081", RICHMOND = "085"),
         tract = paste0(countycode, str_pad(CensusTract, 6, "left", "0")))

voterfile <- left_join(voterfile, voterfile_sf, by = "VANID")

table(voterfile$countycode == voterfile$countycodeupdate, useNA = 'ifany') %>% prop.table()
table(str_pad(voterfile$CensusTract, 6, "left", "0") == voterfile$CensusTractUpdate, useNA = 'ifany') %>% prop.table()
table(str_pad(voterfile$CensusTract, 6, "left", "0") == voterfile$CensusTractUpdate, 
        voterfile$countycode == voterfile$countycodeupdate, useNA = 'ifany' ) 

table(is.na(voterfile$CensusTract), is.na(voterfile$CensusTractUpdate), voterfile$pb, useNA = 'ifany')




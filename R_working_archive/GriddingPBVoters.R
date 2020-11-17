library(dplyr)
library(rgdal)    # for readOGR and others
library(sp)       # for spatial objects
library(tigris)   # for getting state shape files
library(raster)


rasterOptions(overwrite = TRUE)

# load potential projection systems

#A function for getting the shape file for tracts sound area, and calcualting the extent


Zonal_Trans<- function (input, gridsize = 1312/2, point_var, agg_stat = "sum", 
                        lon = "lon", lat = "lat", download_tiger = TRUE, tigerfile = NULL, 
                        onlyNYC = FALSE, convert_to_dataframe = FALSE) {
  
  ##Rasterize ORCA transaction data##
  ##20160803 by Yiqin Alicia Shen##
  ##Overlay raster with census tract, Compute the summed number of taps within each tract##
  ##Currently only works in King County, Pierce County and Snohomish County##
  
  ##Args:
  # Input: A data frame including variables with lon lat info and one column to allocate to grid
  # Raster size: In feet, default 1312 feet (length of two city blocks)
  # point_var: name of variable containing points to be allocated to grid, as string
  # agg_stat: function requested to aggregate, as string
  # lon: name of variable containing longitude information (default is "lon")
  # lat: name of variable containing latitude information (default is "lat")
  
  #Returns:
  # Data frame; could be merged with other datasets through GEOID
  
  require(rgdal)    # for readOGR and others
  require(sp)       # for spatial objects
  require(dplyr)    # for working with data frames
  require(tigris)   # for getting state shape files
  require(acs)      # for getting ACS data
  require(raster)   # for constructing empty rasters
  require(spatialEco) #for zonal.stats
  
  #Set projection system
  # for the proj4 strings see www.spatialreference.org
  ny.2263.proj <- CRS("+proj=lcc +lat_1=41.03333333333333 +lat_2=40.66666666666666 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000.0000000001 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs")
  #srorg.110.proj <- CRS("+proj=lcc +lat_1=47.5 +lat_2=49.73333333333333 +lat_0=47 +lon_0=-120.8333333333333 +x_0=500000.0000000001 +y_0=0 +ellps=GRS80 +to_meter=0.3048006096012192 +no_defs")
  wgs84.proj<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  #Load in shape file from tiger line
  #optional, user can omit if already downloaded:
  if (download_tiger){
    # If user chooses to return only three counties
    if (onlyNYC) tracts <- tracts(state = 'NY', county = c("Kings", "Richmond", "Queens", "New York", "Bronx")) #Load in shape file from tiger line
    else tracts <- tracts(state = 'NY') #Load in shape file from tiger line
    
    tracts<- spTransform(tracts,ny.2263.proj) #The tiger line is in WGS84, transform everything into WA 2926 for easy comparison
  }
  #else tracts <- tigerfile ## this needs to be read in already transformed into 2926
  
  #tracts<- spTransform(tracts,wa.2926.proj) #The tiger line is in WGS84, transform everything into WA 2926 for easy comparison
  
  #if ##check if spatial points df
  #coordinates(input) <- c("lon", "lat") #transforms into SpatialPointsDataFrame
  #proj4string(input) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") #give it a current projection system
  input <- spTransform(input, ny.2263.proj) #again transform it into wa 2926 system so that the units are in feet
  
  #option to filter out lon lat out of bounds
  #input <- filter(input, lat<max_lat) %>% filter(lat>min_lat) %>% filter(lon>min_lon) %>% filter(lon<max_lon)
  
  # set up an 'empty' grid, here via an extent object derived from your data
  
  e <- extent(input) #Where you want drawing to stop
  g <- raster(e) #can also specify the number of rows and columns you want, otherwise square
  
  res(g) <- gridsize #Set resolution of grid
  
  re <- exract(r, y, fun = 'count')
  return(re)
  
  ## LOOP OVER ZONAL STAT CALCULATION FOR EACH VARIABLE
  
  # zonalvars <- list()
  # 
  # for (v in point_var){
  #   # provide a function 'fun' for when there are multiple points per cell, here use sum
  #   r_input <- rasterize(input, g, input[[v]], sum, na.rm=TRUE)
    
    # crop wa_tracts to match the extent of r_input for zonal.stats
  #  tractscrop <- crop(tracts, r_input)
    
    #Use zonal.stats to aggregate raster within each tract
  #  zonalvars[[v]] <- output <- zonal.stats(x = tractscrop, y = r_input, stat = match.fun(agg_stat), trace=TRUE, plot=FALSE)  
  #  removeTmpFiles(h=0)
#  }
  
# #  zonalvars <- as.data.frame(zonalvars)
#   
#   
#   
#   # convert tractscrop back to WGS84 for pretty plotting
#   tractscrop <- spTransform(tractscrop, wgs84.proj)
#   
#   if(convert_to_dataframe){
#     #Merge that back onto the zonal data (census tract)
#     tractscrop_df<-as.data.frame(tractscrop@data) #transform the cropped data file into dataframe
#     tractscrop_df<-cbind(tractscrop_df,zonalvars)
#     
#     #Return output as dataframe
#     return(tractscrop_df)
#   }
#   
#   else {
#     zonalvars$GEOID <- tractscrop@data$GEOID
#     merged <- geo_join(tractscrop, na.omit(zonalvars), "GEOID", "GEOID", how = "inner")
#     # drop tracts with no land
#     merged <- merged[merged$ALAND > 0, ]
#     merged
#   }
#   
#   
}


#### Actually trying this out ####


source("credentials.R")
source("dbDownload.R")

data <- dbDownload(table = "pb", username = username, password = password, dbname = db.name, host = hostname, port = port)

geodata <- data %>% filter(!is.na(Long) & !is.na(Lat)) %>% filter(County %in% toupper(c("kings", "richmond", "queens", "new york", "bronx")))

### making spatial points dataframe ###
xy <- geodata %>% dplyr::select(Long, Lat) %>% as.matrix()
proj_string <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
geodata <- geodata %>% dplyr::select(-Long, -Lat) 
geodata <- SpatialPointsDataFrame(xy, geodata)

#gridout <- Zonal_Trans(geodata, download_tiger = FALSE)

ny.2263.proj <- CRS("+proj=lcc +lat_1=41.03333333333333 +lat_2=40.66666666666666 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000.0000000001 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs")
#srorg.110.proj <- CRS("+proj=lcc +lat_1=47.5 +lat_2=49.73333333333333 +lat_0=47 +lon_0=-120.8333333333333 +x_0=500000.0000000001 +y_0=0 +ellps=GRS80 +to_meter=0.3048006096012192 +no_defs")
wgs84.proj<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

proj4string(geodata) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") #give it a current projection system
geonyc <- spTransform(geodata, ny.2263.proj) #again transform it into wa 2926 system so that the units are in feet

# set up an 'empty' grid, here via an extent object derived from your data
e <- extent(geonyc) 
g <- raster(e)
crs(g) <- ny.2263.proj
res(g) <- 750 #Set resolution of grid (in feet - ny.2263 is a project with units in feet))
values(g) <- 1:ncell(g) # arbitrary value necessary for extract() to work 

# extract cell identifier
re <- extract(g, geonyc, cellnumbers = TRUE, df = TRUE)
re_count <- extract(g, geonyc, fun = 'count')
head(re)
nrow(re) == nrow(geonyc) 
max(table(re$cells)) #max number of voters per cell

#folding count per cell into 
head(values(g))
head(re)
votercounts <- re %>% group_by(cells) %>% dplyr::count()#dplyr::summarize(nvoters = n())
summary(votercounts)
re_counts <- data.frame(cells = values(g)) %>% 
  left_join(votercounts) %>% 
  arrange(cells) %>% 
  mutate(n = ifelse(is.na(n), 0, n))
values(g) <- re_counts$n
plot(g)
# attach cell index to geonyc data
geonyc@data$cells <- re$cells

## plot 
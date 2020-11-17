library(RColorBrewer)
library(dplyr)
library(sp)
library(ggmap)
library(leaflet)
library(htmlwidgets)

source("pb_querydownload.R")

## Mapping ##
geodata <- data %>% filter(!is.na(Long) & !is.na(Lat))

### ggpmap ###
nymap <- get_map(location = "Middletown, NY", zoom = 7)
nymapped <- ggmap(nymap)
nymapped + geom_point(data = geodata, aes(x=Long, y=Lat))  
  
### making spatial points dataframe ###
xy <- geodata %>% select(Long, Lat) %>% as.matrix()
proj_string <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
geodata <- geodata %>% select(-Long, -Lat)
geodata <- SpatialPointsDataFrame(xy, geodata)

### Leaflet ###
bpal <- brewer.pal(length(unique(geodata$Race)), "Set1")
pal <- colorFactor(bpal, domain = unique(geodata$Race))

m <- leaflet(geodata) %>% 
  addTiles() %>% 
  addCircles(#clusterOptions = markerClusterOptions(), 
                   color = ~pal(Race)
                   ) %>%
  addLegend("bottomright", pal = pal, values = ~Race,
            title = "Race")
  
m


m <- leaflet(geodata) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircleMarkers(clusterOptions = markerClusterOptions(), 
    color = ~pal(Race)
  ) 

m


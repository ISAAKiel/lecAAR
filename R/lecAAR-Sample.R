library(mapview)
 library(readr)
 library(sf)
library(magrittr)
library(automap)
library(raster)
 locations <- read_delim("./data/raw_data/BronzeAgeFortifications.csv", delim=";") %>% 
   st_as_sf(coords = c("xUTM", "yUTM"),crs = 32634)
 
 applylarge <- largest_empty_circle(locations)
 SLDF<-rasterToContour(applylarge,nlevels=10)
 SLDF<-c(SLDF,extent(SLDF)) ##needs refinement
 ps <- SpatialPolygons(
   lapply(1:length(SLDF), 
          function(i) Polygons(lapply(sp::coordinates(SLDF)[[i]], function(y) Polygon(y)), as.character(i))))
 
##' This function calculates the interpolated largest empty circle raster for point distributions
##' @title Calculate the largest empty circle raster
##' @param points sf feature of the locations of interest
##' @param raster raster or SpatialGridDataFrame indicating the interpolation environment
##' @return a list containing the LEC_raster and the Nodes including the distance to the closest location
##' @import dplyr sf deldir gstat
##' @importFrom raster raster
##' @importFrom sp CRS
##' @importFrom automap autoKrige
##' @importFrom magrittr %>% %<>%
##' @export largest_empty_circle
##' @author Wolfgang Hamer
##' @author Daniel Knitter
##' @examples
##' library(mapview)
##' library(readr)
##' library(sf)
##' locations <- read_delim("./data/raw_data/BronzeAgeFortifications.csv", delim=";") %>% 
##'   st_as_sf(coords = c("xUTM", "yUTM"),crs = 32634)
##' 
##' applylarge <- largest_empty_circle(locations)
##' 
##' mapview(applylarge$LEC_raster)+mapview(applylarge$Nodes,zcol="mindist")+mapview(locations)
largest_empty_circle <- function(points,raster="default"){
  
  xdat <- sf::st_coordinates(points)[,1]
  ydat <- sf::st_coordinates(points)[,2]
  
  deld <- deldir::deldir(x=xdat,y=ydat)

  dd <- dplyr::tibble(x = c(deld$dirsgs$x1,deld$dirsgs$x2),
                      y = c(deld$dirsgs$y1,deld$dirsgs$y2)) %>% 
    dplyr::distinct() %>% 
    st_as_sf(coords = c("x", "y"),crs = sf::st_crs(points))
  
  dd %<>% dplyr::mutate(mindist = unlist(Map(function(x){min(sf::st_distance(dd[x,],points))},x=1:nrow(dd)))) %>% 
    dplyr::select(mindist)
    
  if(class(raster)=="character"){
    raster <-raster::raster(nrows=200, ncols=200, 
                        xmn=st_bbox(points)$xmin, xmx=st_bbox(points)$xmax, 
                        ymn=st_bbox(points)$ymin, ymx=st_bbox(points)$ymax,
                        crs = sp::CRS((sf::st_crs(points))$proj4string))
    raster[] <- rep(1,length(raster[]))
  }
  
  if(class(raster)=="RasterLayer"){
    raster <- as(raster,"SpatialGridDataFrame")
  }
  
  autom <- automap::autoKrige(mindist ~ 1,as(dd,"Spatial"),raster)
  ret <- raster::raster(autom$krige_output)
  
  return(list(LEC_raster = ret,
              Nodes = dd))
}



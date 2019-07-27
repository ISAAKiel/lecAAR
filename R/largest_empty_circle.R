##' This function calculates the interpolated largest empty circle raster for point distributions
##' @title Calculate the largest empty circle raster
##' @param sites_sf sf feature of the locations of interest
##' @param raster raster or SpatialGridDataFrame indicating the interpolation environment
##' @return a list containing the LEC_raster and the Nodes including the distance to the closest location
##' @export
##' @author Wolfgang Hamer
##' @author Daniel Knitter
##' @examples
##' library(mapview)
##' library(sf)
##' 
##' locations <- bronze_age_fortifications %>% 
##'   st_as_sf(coords = c("xUTM", "yUTM"), crs = 32634)
##' 
##' applylarge <- largest_empty_circle(locations)
##' sp::plot(applylarge)
##' 
largest_empty_circle <- function(sites_sf,raster="default"){
  
  xdat <- sf::st_coordinates(sites_sf)[,1]
  ydat <- sf::st_coordinates(sites_sf)[,2]
  
  deld <- deldir::deldir(x=xdat,y=ydat)

  dd <- dplyr::tibble(x = c(deld$dirsgs$x1,deld$dirsgs$x2),
                      y = c(deld$dirsgs$y1,deld$dirsgs$y2)) %>% 
    dplyr::distinct() %>% 
    sf::st_as_sf(coords = c("x", "y"),crs = sf::st_crs(sites_sf))
  
  dd %<>% dplyr::mutate(mindist = unlist(Map(function(x){min(sf::st_distance(dd[x,],sites_sf))},x=1:nrow(dd)))) %>% 
    dplyr::select(.data$mindist)
    
  if(class(raster)=="character"){
    # todo: rasterzellengroesse selectierbar
    raster <-raster::raster(nrows=200, ncols=200, 
                        xmn=sf::st_bbox(sites_sf)$xmin, xmx=sf::st_bbox(sites_sf)$xmax, 
                        ymn=sf::st_bbox(sites_sf)$ymin, ymx=sf::st_bbox(sites_sf)$ymax,
                        crs = sp::CRS((sf::st_crs(sites_sf))$proj4string))
    raster[] <- rep(1,length(raster[]))
  }
  
  if(class(raster)=="RasterLayer"){
    raster <- methods::as(raster,"SpatialGridDataFrame")
  }

  
  autom <- automap::autoKrige(mindist ~ 1, methods::as(dd,"Spatial"),raster)
  ret <- raster::raster(autom$krige_output)
  
  return(ret)
}



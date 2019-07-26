##' This function helps finding the point of maximum area increase 
##' @title Determine the point of maximum area increase 
##' @param project_CRS Coordinate Reference System of Project. Unit has to be metres.
##' @param sites_table Dataframe or tibble containing individual sites in each row with two separate columns containing the coordinates
##' @param xy Dataframe or tibble containing the coordinates in two separate columns
##' @param x_axis_steps Steps on x-axis in metres. Default is 10 
##' @export
##' @author David N. Matzig
##' @examples
##' 
##' # provide CRS and data
##' 
##' project_CRS <- "+init=epsg:32634"
##' sites_table <- read_delim("./data/raw_data/BronzeAgeFortifications.csv", delim=";")
##' xy <- sites[,c("xUTM","yUTM")]
##' 
##' # Plot ratio of area increase 
##'
##' max_area_increase(project_CRS, sites_table, xy)
##'
##'
##' # set value for level of maximum area increase
##' 
##' max_increase <- 10


max_area_increase <- function(project_CRS, sites_table, xy, x_axis_steps){
  # load data in the same way as in the other function
  sites <- sp::SpatialPointsDataFrame(xy, sites_table, proj4string = sp::CRS(project_CRS))
  # sites <- sp::spTransform(sites, sp::CRS(projektion)) # ggf. möchte der Nutzer noch seine Projektion verändern?
  sites <- sp::remove.duplicates(sites)
  
  # convert to sf for kriging-function only
  sf::st_as_sf(sites) -> sites_sf
  
  # kriging
  applylarge <- largest_empty_circle(sites_sf) # W. Hamer's and D. Knitter's function!
  
  # isolines 
  SLDF <- raster::rasterToContour(applylarge,  # create isolines
                                  nlevels = 50)
  
  ps <- sp::SpatialPolygons(lapply(1:length(SLDF), # prepare isolines
                               function(i) Polygons(lapply(sp::coordinates(SLDF)[[i]], 
                                                           function(y) Polygon(y)), as.character(i)))) 
  # there might be a problem if the circles are outside the plot? see:
  # plot(ps)
  
  raster::crs(ps) <- project_CRS
  
  ## level = ID of isoline
  ## area = area within isoline
  level <- list()
  area <- list()
  for (p3 in 1:length(ps@polygons)){
    area[[p3]] <- ps@polygons[[p3]]@area
    level[[p3]] <- p3
  }
  area <- unlist(area)
  level <- unlist(level)
  
  flaeche_levels <- data.frame(level, area)    
  
  # determining the isoline where the area increase has its maximum through ratio
  ratio <- list() 
  for (p0 in 1:(nrow(flaeche_levels)-1)){ # -1 because last value cannot be divided by something
    ratio[[p0]] <- area[p0] /  area[p0+1]
  }
  ratio <- unlist(ratio)
  level <- head(level, -1)  # -1 because both vectors have to have the same length
  area_increase <- data.frame(level, ratio) 
  
  
  # manuelle/visuelle Ermittlung des "geeigneten Punkts" 
  
  if (!exists("x_axis_steps")) {
    x-axis_steps = 10
  }
  
  area_increase_plot <- ggplot2::ggplot(data = area_increase, 
                                        ggplot2::aes(x = level, 
                                                     y = ratio)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(name = "Level",
                                breaks = seq(from = 0, 
                                             to = nrow(area_increase), 
                                             by = x_axis_steps)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(size = 12, 
                                              angle = 90))
  
  return(area_increase_plot)
}







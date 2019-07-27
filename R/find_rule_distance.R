##' This function helps finding the rule-distance. It produces a SpatialPointsDataframe, calculates the nearest neighbour distance and plots a density curve for the to determine the rule-distance.
##' @title Determine the rule-distance
##' @param project_CRS Coordinate Reference System of Project. Unit has to be metres.
##' @param sites_table Dataframe or tibble containing individual sites in each row with two separate columns containing the coordinates
##' @param xy Dataframe or tibble containing the coordinates in two separate columns
##' @param threshold Threshold for nearest neighbour in metres. Default is 10000
##' @param bin_width Bin width for density plot. Default is 250
##' @param x_axis_steps Steps on x-axis in metres. Default is 500 
##' @export 
##' @author David N. Matzig
##' @examples
##' 
##' # provide CRS and data
##' 
##' project_CRS <- "+init=epsg:32634"
##' sites_table <- bronze_age_fortifications
##' xy <- sites_table[,c("xUTM","yUTM")]
##' 
##' 
##' # create density plot
##' 
##' density_plot_nn(project_CRS, sites, xy, threshold, bin_width, x_axis_steps)
##' 
##' 
##' # save rule-distance in variable 'rule_distance_value_m'
##' 
##' rule_distance_value_m <- 2500 



density_plot_nn <- function(project_CRS, sites_table, xy, threshold = 10000, bin_width = 250, x_axis_steps = 1000){
  
  sites <- sp::SpatialPointsDataFrame(xy, sites_table, proj4string = sp::CRS(project_CRS))
  # sites <- sp::spTransform(sites, sp::CRS(projektion)) # ggf. möchte der Nutzer noch seine Projektion verändern?
  sites <- sp::remove.duplicates(sites)
  coordinates <- as.data.frame(sites@coords)
  
  # Nearest Neighbour
  sites_nn <- RANN::nn2(coordinates)
  nn <- sites_nn$nn.dists[, 2] # distance to 1st nearest neighbour
  nn <- sort(nn, decreasing = F)
  
  nn[nn >= threshold] <- NA # distances over "threshold" (in m) get removed
  
  regelabstand_plot <-
    ggplot2::ggplot(data = as.data.frame(nn), ggplot2::aes(x = nn)) +
    ggplot2::geom_density(bw = bin_width) +
    ggplot2::xlim(0, threshold) +
    ggplot2::scale_x_continuous(name = "Distance to Nearest Neighbour",
                                breaks = seq(from = 0,
                                             to = threshold,
                                             by = x_axis_steps)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12,
                                              angle = 90))
  
  return(regelabstand_plot)
  
}


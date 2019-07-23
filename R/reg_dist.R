#' Regular Distances
#'
#' @param points 
#'
#' @return
#' @export
#'
#' @examples
#' sites <- readr::read_delim("./data/raw_data/BronzeAgeFortifications.csv", delim=";")
#' xy <- sites[,c("xUTM","yUTM")]
#' project_CRS <- "+init=epsg:32634"
#' max_nn_dist <-  10000
#' density_bw <- 500
#' 
reg_dist <- function(sites, project_CRS, max_nn_dist, density_bw){
  
  sites <- sp::SpatialPointsDataFrame(xy, sites, proj4string = sp::CRS(project_CRS))
  sites <- sp::remove.duplicates(sites)
  coords <- as.data.frame(sites@coords)
  
  # Nearest Neighbour 
  sites_nn <- RANN::nn2(coords) 
  nn <- sites_nn$nn.dists[,2] # die länge von jedem punkt zu seinem 1. nächsten nachbarn
  nn <- sort(nn, decreasing = F)
  nn[nn>max_nn_dist] <- NA
  
  #ggplot2::ggplot(data = as.data.frame(nn), ggplot2::aes(x = nn)) +
    #ggplot2::geom_density(bw = density_bw) 
}
## + ggplot2::scale_x_continuous(breaks=2000)max_nn_dist/10


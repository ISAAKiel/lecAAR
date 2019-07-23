##' This function produces the final results
##' @title Create results
##' @param area Vector of area within each isoline
##' @param max_increase Level where the area increase has its (local) maximum. Can either be set manually or automatically by using ?TODO?
##' @param M2 Rule-distance in metres
##' @param sites SpatialPointsDataFrame of known sites
##' @return A dataframe containing the results, such as "Total area in square kilometres", "Expected rule-distance in  kilometers", "Hexagon area in square kilometres", "Number of expected sites", and "Percentage of known sites"
##' @export create_results 
##' @author David N. Matzig
##' @examples
##' 
##' create_results(area, max_increase, rule_distance_value_m, sites)


create_results <- function(area, max_increase, rule_distance_value_m, sites){
  
  # total area
  area_value <- area[max_increase] # in square metres
  area_value_km2 <- round(area_value * 1e-6, digits = 2) # in square kilometres
  
  # rule-distance
  rule_distance_value_m <- M2 #  in metres
  rule_distance_value_km <- round(rule_distance_value_m/1000, digits = 2) # in kilometres
  
  # area within one hexagon calculated with rule-distance in metres 
  area_hex_m2 <- ((M2/2)^2) * 2 * (3^0.5) # in square metres
  area_hex_km2 <- round(area_hex_m2 * 1e-6, digits = 2) # in square kilometres
  
  # number of sites to be expected
  expected_settlements <- area[max_increase]/area_hex
  
  # percentage of known sites
  perc_found_sites <- nrow(sites)/expected_settlements*100
  
  # create dataframe
  result_table <- data.frame(area_value_km2, rule_distance_value_km, area_hex_km2, round(expected_settlements, digits = 1), round(perc_found_sites, digits = 2))
  names(result_table) <- c("Total area in square kilometres", "Expected rule-distance in  kilometers", "Hexagon area in square kilometres", "Number of sites to be expected", "Percentage of known sites")
  
  return(t(result_table))
  
}




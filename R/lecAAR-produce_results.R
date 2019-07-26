##' This function produces the final results
##' @title Create results
##' @param max_increase Level where the area increase has its (local) maximum. Can either be set manually or automatically by using ?TODO?
##' @param rule_distance_value_m Rule-distance in metres
##' @return A dataframe containing the results, such as "Total area in square kilometres", "Expected rule-distance in  kilometers", "Hexagon area in square kilometres", "Number of expected sites", and "Percentage of known sites"
##' @export 
##' @author David N. Matzig
##' @examples
##' 
##' 
##' # provide CRS and data
##' 
##' project_CRS <- "+init=epsg:32634"
##' sites_table <- bronze_age_fortifications
##' xy <- sites_table[,c("xUTM","yUTM")]
##' 
##' 
##' # 1. Create density plot
##' 
##' density_plot_nn(project_CRS, sites_table, xy)
##' 
##' 
##' # 2. look up the rule-distance from the plot and save it in the variable 'rule_distance_value_m'
##' 
##' rule_distance_value_m <- 2500 
##' 
##' 
##' # 3. Plot ratio of area increase 
##'
##' max_area_increase(project_CRS, sites_table, xy)
##'
##'
##' # 4. Set the value for level of maximum area increase
##' 
##' max_increase <- 10
##' 
##' 
##' # Create table containing the results, such as "Total area in square kilometres", "Expected rule-distance in  kilometers", "Hexagon area in square kilometres", "Number of expected sites", and "Percentage of known sites"
##' 
##' create_results(max_increase, rule_distance_value_m)


create_results <- function(max_increase, rule_distance_value_m){
  
  # total area
  area_value <- area[max_increase] # in square metres
  area_value_km2 <- round(area_value * 1e-6, digits = 2) # in square kilometres
  
  # rule-distance
  rule_distance_value_m <- rule_distance_value_m #  in metres
  rule_distance_value_km <- round(rule_distance_value_m/1000, digits = 2) # in kilometres
  
  # area within one hexagon calculated with rule-distance in metres 
  area_hex_m2 <- ((rule_distance_value_m/2)^2) * 2 * (3^0.5) # in square metres
  area_hex_km2 <- round(area_hex_m2 * 1e-6, digits = 2) # in square kilometres
  
  # number of sites to be expected
  expected_settlements <- area[max_increase]/area_hex_m2
  
  # percentage of known sites
  perc_found_sites <- nrow(sites_table)/expected_settlements*100
  
  # create dataframe
  result_table <- data.frame(area_value_km2, rule_distance_value_km, area_hex_km2, round(expected_settlements, digits = 1), round(perc_found_sites, digits = 2))
  names(result_table) <- c("Total area in square kilometres", "Expected rule-distance in kilometers", "Hexagon area in square kilometres", "Number of sites to be expected", "Percentage of known sites")
  
  return(t(result_table))
  
}




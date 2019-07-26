# Largest Empty Circles (LEC) nach Zimmermann/Wendt (2003).

Code für die Berechnung der größten leeren Kreise nach Zimmermann/Wendt (2003).  
Das Endprodukt ist ein Schätzungswert der zu erwartenden Siedlungsanzahl. 

## Installation
```
if(!require('devtools')) install.packages('devtools')
devtools::install_github("ISAAKiel/lecAAR")
```

## How to use

```
### Provide CRS and data 
project_CRS <- "+init=epsg:32634"
sites_table <- bronze_age_fortifications
xy <- sites_table[,c("xUTM","yUTM")]


### Create density plot
density_plot_nn(project_CRS, sites_table, xy)


### Look up the rule-distance from the plot and save it in the variable 'rule_distance_value_m'
rule_distance_value_m <- 2500 


### Plot ratio of area increase 
max_area_increase(project_CRS, sites_table, xy)

### Set the value for level of maximum area increase
max_increase <- 10


### Create table containing the results
### such as "Total area in square kilometres", "Expected rule-distance in  kilometers", "Hexagon area in square kilometres", "Number of expected sites", and "Percentage of known sites"
create_results(max_increase, rule_distance_value_m)
```

# References

__Zimmermann/Wendt 2003__: Andreas Zimmermann, Karl Peter Wendt, Wie viele Bandkeramiker lebten 5.060 v. Chr.? Techniken Geographischer Informationssysteme zum Schätzen von Bevölkerungsdichten, Archäologische Informationen 26/2, 2003, 491-497.

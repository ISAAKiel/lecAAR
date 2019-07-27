# Largest Empty Circles (LEC) after Zimmermann and Wendt.

Tool for calculating the largest empty circles and estimation of archaeological sites theoretically to be expected in region of interest.  
This package is an implementation of the method published by Andreas Zimmermann and Karl Peter Wendt (Zimmermann/Wendt 2003).

## Method

* to do


## Installation
```
if(!require('devtools')) install.packages('devtools')
devtools::install_github("ISAAKiel/lecAAR")
```

## How to use

```
# Provide CRS and data 
project_CRS <- "+init=epsg:32634"
sites_table <- bronze_age_fortifications
xy <- sites_table[,c("xUTM","yUTM")]


# Create density plot
density_plot_nn(project_CRS, sites_table, xy)


## Look up the rule-distance from the plot and save it in the variable 'rule_distance_value_m'
rule_distance_value_m <- 2500 


# Plot ratio of area increase 
max_area_increase(project_CRS, sites_table, xy)

## Set the value for level of maximum area increase
max_increase <- 10


# Create table containing the results
## such as "Total area in square kilometres", "Expected rule-distance in  kilometers", 
## "Hexagon area in square kilometres", "Number of expected sites", 
## and "Percentage of known sites"
create_results(max_increase, rule_distance_value_m)
```

# References

__Zimmermann/Wendt 2003__: Andreas Zimmermann, Karl Peter Wendt, Wie viele Bandkeramiker lebten 5.060 v. Chr.? Techniken Geographischer Informationssysteme zum Schätzen von Bevölkerungsdichten, Archäologische Informationen 26/2, 2003, 491-497.


# Licence

`lecAAR` is released under the [GNU General Public Licence, version 3](http://www.r-project.org/Licenses/GPL-3). Comments and feedback are welcome, as are code contributions.

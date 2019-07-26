library(mapview)
library(readr)
library(sf)
library(magrittr)
library(automap)
library(raster)

# festlegen, welches CRS:
project_CRS <- "+init=epsg:32634"

# kriging und Isolinien erstellen
 locations <- read_delim("./data/raw_data/BronzeAgeFortifications.csv", delim=";") %>% 
   st_as_sf(coords = c("xUTM", "yUTM"),crs = 32634)
 
 applylarge <- largest_empty_circle(locations)
 SLDF <- raster::rasterToContour(applylarge,    # isolines
                                 nlevels = 50)

 
# Punkte neu laden, weil ich mit sp arbeite und ich den schon geschriebenen code nicht in sf umwandeln möchte --> TODO 
 sites <- read_delim("./data/raw_data/BronzeAgeFortifications.csv", delim=";")
 xy <- sites[,c("xUTM","yUTM")]
 sites <- sp::SpatialPointsDataFrame(xy, sites, proj4string = sp::CRS(project_CRS))
 # sites <- sp::spTransform(sites, sp::CRS(projektion)) # ggf. möchte der Nutzer noch seine Projektion verändern?
 sites <- sp::remove.duplicates(sites)
 coordinates <- as.data.frame(sites@coords)

 
 
 
# Nearest Neighbour 
 sites_nn <- RANN::nn2(coordinates) 
 nn <- sites_nn$nn.dists[,2] # die länge von jedem punkt zu seinem 1. nächsten nachbarn
 nn <- sort(nn, decreasing = F)
 nn[nn >= 10000] <- NA
 
 h = hist(nn, breaks=100)
 M2 = h$mids[which.max(h$counts)] # hier wird das __absolute__ Maximum gesucht -> das sollte man dem Nutzer überlassen. Evtl. entscheidet der sich für ein lokales Maximum!
 # M2 ist somit der "Regelabstand"!
 
 # manuell
 regelabstand_plot <- ggplot2::ggplot(data = as.data.frame(nn), ggplot2::aes(x = nn)) +
   ggplot2::geom_density(bw = 250) +
   ggplot2::xlim(0, 10000) +
   ggplot2::scale_x_continuous(name = "Distance to Nearest Neighbour",
                               breaks = seq(from = 0, 
                                            to = 10000, 
                                            by = 1000)) +
   ggplot2::theme_bw() +
   ggplot2::theme(axis.text.x = element_text(size = 12, 
                                             angle = 90))
   
 
 # "automatisiert"
 regelabstand_plot <- ggplot2::ggplot(data = as.data.frame(nn), ggplot2::aes(x = nn)) +
   ggplot2::geom_density(bw = 5) +     # ob die vline den peak trifft, hängt hier davon ab, wie große die BW ist!!!! <901 = komplett daneben.
   ggplot2::geom_vline(ggplot2::aes(xintercept = M2), col = "red") +
   ggplot2::geom_label(ggplot2::aes(x = M2, y = 0, label = paste("nn =", round(M2,0))))
 # die häufigste Distanz zum nächsten Nachbar wird als Regelabstand gewählt.
 # die vline entspricht nicht dem Maximum in GGPLOT! weil vline aus hist-Funktion abgeleitet wurde?
 # TODO!
 # bei manueller/visueller Festlegung von M2 muss der Wert noch angegeben werden, als "M2 <- ..."
 
 
 

# 2. wo ist die flächenzunahme = 0 ? (Null oder maximal? -> nochmal in die Literatur schauen!)
 ps <- SpatialPolygons(lapply(1:length(SLDF), 
                              function(i) Polygons(lapply(sp::coordinates(SLDF)[[i]], 
                                                          function(y) Polygon(y)), as.character(i)))) ## isolinien vorbereiten
 
 raster::crs(ps) <- project_CRS
 
 ## level = bezeichnung der isolinien
 ## area = fläche innerhalb der jeweiligen isolinie
 level <- list()
 area <- list()
 for (p3 in 1:length(ps@polygons)){
   area[[p3]] <- ps@polygons[[p3]]@area
   level[[p3]] <- p3
 }
 
 area <- unlist(area)
 level <- unlist(level)
 
 flaeche_levels <- data.frame(level, area)    
 
 quotient <- list() # um die maximale flächenZUNAHME zu ermitteln, muss der Quotient gebildet werden!
 for (p0 in 1:(nrow(flaeche_levels)-1)){ # -1 weil der letzte wert durch nichts geteilt werden kann
   quotient[[p0]] <- area[p0] /  area[p0+1]
 }
 quotient <- unlist(quotient)
 level <- head(level, -1)  # -1 weil beide vectoren die selbe länge haben müssen
 flaechenzunahme <- data.frame(level, quotient) 
 
# ENTWEDER: automatische Ermittlung des "geeigneten Punkts" 
 area_zunahme_null <- which(splus2R::peaks(flaechenzunahme$quotient))[1] # der erste peak wird gewählt
 
 flaechenzunahme_plot <- ggplot2::ggplot(data = flaechenzunahme, 
                                         ggplot2::aes(
                                           x = level, 
                                           y = quotient)) +
   ggplot2::geom_line() +
   ggplot2::geom_vline(ggplot2::aes(
     xintercept = which(splus2R::peaks(flaechenzunahme$quotient))[1]), 
     col = "red") +
   ggplot2::geom_label(ggplot2::aes(
     x = area_zunahme_null, 
     y = 0, 
     label = paste("level =", area_zunahme_null)))
 
 
# ODER: manuelle/visuelle Ermittlung des "geeigneten Punkts" 
 flaechenzunahme_plot <- ggplot2::ggplot(data = flaechenzunahme, ggplot2::aes(x = level, y = quotient)) +
   ggplot2::geom_line()
 
 # area_zunahme_null muss dann vom Nutzer festgelegt werden!
 area_zunahme_null <- 10
 
 

 
 
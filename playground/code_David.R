# Functions
# Carson's Voronoi polygons function
voronoipolygons <- function(x) {
  require(deldir)
  require(sp)
  if (.hasSlot(x, 'coords')) {
    crds <- x@coords  
  } else crds <- x
  z <- deldir(crds[,1], crds[,2], sort = F)
  w <- tile.list(z)
  polys <- vector(mode='list', length=length(w))
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP <- SpatialPolygons(polys)
  voronoi <- SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1],
                                                          y=crds[,2], row.names=sapply(slot(SP, 'polygons'), 
                                                                                       function(x) slot(x, 'ID'))))
}

# Settings

projektion <- "+init=epsg:32636"
urspr_crs <- "+init=epsg:4326"

sites_corrected <- readr::read_delim("2data/sites_corrected.csv", 
                                     "\t", escape_double = FALSE, trim_ws = TRUE)


###############################
sites_by_stage <- list()

stages <- c("A", "AI", "AII", "AIII")
for (p1 in stages){
  current_stage <- list()
  for (p00 in 1:nrow(sites_corrected)){
    current_site <- sites_corrected[p00,]
    if (current_site[,"Stage"] == p1){
      current_stage[[p00]] <- current_site
    }
  }
  sites_by_stage[[p1]] <- do.call(rbind, current_stage)
}
# mehrphasige siedlungen müssen in diskrete phasen eingeteilt werden (b1-2 in b1 und b2).
#####
stage_b1 <- list()
for (p00 in 1:nrow(sites_corrected)){
  current_site <- sites_corrected[p00,]
  if (current_site[,"Stage"] == "BI" || current_site[,"Stage"] == "BI-BII"){
    stage_b1[[p00]] <- current_site
  }
}
stage_b1 <- do.call(rbind, stage_b1)
#####
stage_b2 <- list()
for (p00 in 1:nrow(sites_corrected)){
  current_site <- sites_corrected[p00,]
  if (current_site[,"Stage"] == "BII" || current_site[,"Stage"] == "BI-BII" || current_site[,"Stage"] == "BII-CI"){
    stage_b2[[p00]] <- current_site
  }
}
stage_b2 <- do.call(rbind, stage_b2)
#####
stage_c1 <- list()
for (p00 in 1:nrow(sites_corrected)){
  current_site <- sites_corrected[p00,]
  if (current_site[,"Stage"] == "CI" || current_site[,"Stage"] == "BII-CI" || current_site[,"Stage"] == "CI-CII"){
    stage_c1[[p00]] <- current_site
  }
}
stage_c1 <- do.call(rbind, stage_c1)
#####
stage_c2 <- list()
for (p00 in 1:nrow(sites_corrected)){
  
  current_site <- sites_corrected[p00,]
  
  if (current_site[,"Stage"] == "CII" || current_site[,"Stage"] == "CI-CII"){
    stage_c2[[p00]] <- current_site
  }
}
stage_c2 <- do.call(rbind, stage_c2)
#####
sites_by_stage[["BI"]] <- stage_b1
sites_by_stage[["BII"]] <- stage_b2
sites_by_stage[["CI"]] <- stage_c1
sites_by_stage[["CII"]] <- stage_c2
#######################

sites_grobe_zeitstufen <- list()

sites_grobe_zeitstufen[["A"]] <- dplyr::bind_rows(sites_by_stage$A, sites_by_stage$AI, sites_by_stage$AII, sites_by_stage$AIII)

sites_grobe_zeitstufen[["B"]] <- dplyr::bind_rows(sites_by_stage$BI, sites_by_stage$BII)

sites_grobe_zeitstufen[["C"]] <- dplyr::bind_rows(sites_by_stage$CI, sites_by_stage$CII)



Shape_3 <- list()
for (s in names(sites_grobe_zeitstufen)){
  loop_by_stage_2 <- sites_grobe_zeitstufen[[s]]
  
  xy <- loop_by_stage_2[,c("Long E. deg","Lat N. deg")]
  loop_by_stage_2 <- sp::SpatialPointsDataFrame(xy, loop_by_stage_2, proj4string = sp::CRS(urspr_crs))
  loop_by_stage_2 <- sp::spTransform(loop_by_stage_2, sp::CRS(projektion))
  loop_by_stage_2<- sp::remove.duplicates(loop_by_stage_2)
  
  koordinaten <- as.data.frame(loop_by_stage_2@coords)
  names(koordinaten) <- c("x", "y")
  loop_by_stage_2$x <- koordinaten$x
  loop_by_stage_2$y <- koordinaten$y
  
  Shape_3[[s]] <- loop_by_stage_2
}
############################

Layer = "Testdatensatz"


regelabstand_plot <- list()
flaechenzunahme_plot <- list()
area_value <- list()
regelabstand_value <- list()
area_hex_value <- list()
expected_settlements_value <- list()
perc_found_sites <- list()

for (p in names(Shape_3)){
  
  current_stage <- Shape_3[[p]]
  Shape_2 <- current_stage
  
  
  # Carsten Schmid      
  # Thiessen polygon tessellation
  VoronoiPolygons <- voronoipolygons(Shape_2)
  VoronoiPolygonNodes <- ggplot2::fortify(VoronoiPolygons) 
  
  df.sp<-VoronoiPolygonNodes
  sp::coordinates(df.sp)<-~long+lat
  sp::proj4string(df.sp) <- sp::CRS(projektion)
  
  VoronoiPolygonNodes.sp <- sp::remove.duplicates(df.sp)
  
  VoronoiPolygonNodes.sp.df <- as.data.frame(VoronoiPolygonNodes.sp)
  VoronoiPolygonNodes.sp.df$NearestNodeDistance <- -9999  # wieso??
  
  #Calculate distance between voronoi nodes and arch. sites 
  glDistance <- rgeos::gDistance(VoronoiPolygonNodes.sp, Shape_2, byid = TRUE) # will be 0, all inside
  
  ##Create GRID
  ### Set cellsize -> nur worauf einstellen??
  xmin <- min(VoronoiPolygonNodes$long) - 30000
  xmax <- max(VoronoiPolygonNodes$long) + 30000
  ymin <- min(VoronoiPolygonNodes$lat) - 30000
  ymax <- max(VoronoiPolygonNodes$lat) + 30000
  
  # Create grid (location where an interpolation shouild be calculated for)
  ### was soll die Gridgröße sein?   
  grd <- expand.grid(x=seq(from=xmin, to=xmax, by= 5000), y=seq(from=ymin, to=ymax, by= 5000))
  
  plot(VoronoiPolygons, lty ="dashed")
  points(VoronoiPolygonNodes.sp, pch=16, col="red")
  points(Shape_2, pch = 16, col="darkblue")
  
  for (pts in 1:length(VoronoiPolygonNodes.sp.df[,1])) {
    points(Shape_2[which.min(glDistance[,pts]),], pch=16, cex=2, col="darkblue")
    points(VoronoiPolygonNodes.sp[pts,],pch=16, cex=2, col="red")
    
    VoronoiPolygonNodes.sp.df[pts,length(VoronoiPolygonNodes.sp.df[1,])] <- min(glDistance[,pts])/1000 # /1000 für 1 km
  }
  
  
  ###### Ordinary Kriging using geoR-Package
  m.coords <- as.matrix(cbind(VoronoiPolygonNodes.sp.df[1],VoronoiPolygonNodes.sp.df[2]))
  v.values <- as.vector(VoronoiPolygonNodes.sp.df$NearestNodeDistance)
  bin1 <- geoR::variog(coords=m.coords, data=v.values)
  
  # Show Variogram 
  ols <- geoR::variofit(bin1, ini=grd, fix.nug=F, wei="npairs")
  km <- geoR::krige.conv(coords=m.coords, data=v.values, loc=grd, krige = geoR::krige.control(obj.model=ols)) # Fehler: kann Vektor der Größe 8.8 GB nicht allozieren -> wenn cellsize = 500
  
  # Export Grid and Contour Lines (ESRI-Format)
  ExportGrid <- expand.grid(x=seq(from=xmin, to=xmax, by= 5000), y=seq(from=ymin, to=ymax, by= 5000))
  sp::coordinates(ExportGrid) <- ~x+y
  sp::gridded(ExportGrid) <- TRUE
  ExportGrid$predict <- km$predict
  
  ExportGrid$predict <- as.numeric(ExportGrid$predict)
  im <- sp::as.image.SpatialGridDataFrame(ExportGrid)
  cl <- grDevices::contourLines(x = im$x, y = im$y, z = im$z, levels = pretty(range(im$z, na.rm = TRUE), 20))
  SLDF <- maptools::ContourLines2SLDF(cl)
  
  
  #######################
  
  # David Matzig      
  # warum nicht nearest neighbour?
  Shape_2_coords <- as.data.frame(Shape_2@coords)
  Shape_2_nn <- RANN::nn2(Shape_2_coords) 
  nn <- Shape_2_nn$nn.dists[,2] # die länge von jedem punkt zu seinem 1. nächsten nachbarn
  nn <- sort(nn, decreasing = F)
  
  h = hist(nn, breaks=100)
  M2 = h$mids[which.max(h$counts)]
  
  regelabstand_plot[[p]] <- ggplot2::ggplot(data = as.data.frame(nn), ggplot2::aes(x = nn)) +
    ggplot2::geom_density(bw = 1) +     # ob die vline den peak trifft, hängt hier davon ab, wie große die BW ist!!!! <901 = komplett daneben.
    ggplot2::geom_vline(ggplot2::aes(xintercept = M2), col = "red") +
    ggplot2::geom_label(ggplot2::aes(x = M2, y = 0, label = paste("nn =", round(M2,0))))
  # die häufigste Distanz zum nächsten Nachbar wird als Regelabstand gewählt.
  # die vline entspricht nicht dem Maximum in GGPLOT! weil vline aus hist-Funktion abgeleitet wurde?
  
  
  #ggplot2::geom_vline(ggplot2::aes(xintercept=nn[which.max(splus2R::peaks(nn))]))
  
  # 2. wo ist das flächenzunahme = 0 ?
  ## isolinien vorbereiten
  ps <- SpatialPolygons(
    lapply(1:length(SLDF), 
           function(i) Polygons(lapply(sp::coordinates(SLDF)[[i]], function(y) Polygon(y)), as.character(i))))
  
  #raster::crs(ps) <- urspr_crs
  #ps <- sp::spTransform(ps, sp::CRS(projektion))
  raster::crs(ps) <- projektion
  
  
  ###
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
  
  flaechenzunahme <- data.frame(level, area)    
  
  area_zunahme_null <- which(splus2R::peaks(flaechenzunahme$area))[1] # der erste peak wird gewählt
  
  flaechenzunahme_plot[[p]] <- ggplot2::ggplot(data = flaechenzunahme, ggplot2::aes(x = level, y = area)) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(ggplot2::aes(xintercept = which(splus2R::peaks(flaechenzunahme$area))[1]), col = "red") +
    ggplot2::geom_label(ggplot2::aes(x = area_zunahme_null, y = 0, label = paste("level =", area_zunahme_null)))
  
  
  ## sich einen peak aussuchen und an der stelle die fläche aus area[] ablesen.
  ## bzw, den ersten peak
  ## which(splus2R::peaks(flaechenzunahme$area))[1]
  
  area_value[[p]] <- area[area_zunahme_null] # in m² (nehme ich an) ???
  regelabstand_value[[p]] <- M2 # Regelabstand
  
  # fläche eines hexagons mit dem regelabstand M2
  
  area_hex <- (M2/2)^2 * 2 * 3^0.5
  area_hex_value[[p]] <- area_hex
  
  expected_settlements <- area[area_zunahme_null]/area_hex
  expected_settlements_value[[p]] <- expected_settlements
  
  perc_found_sites[[p]] <- nrow(Shape_2)/expected_settlements # prozent der gefundenen siedlungen
  
  
}




plot(ExportGrid)
lines(SLDF, add = T)
points(Shape_2, add =T, pch = 3, col = "white")


#  und 2. nach Region/Provinz

#      
#    knitr::kable(Shape_2)







#####################################################################################
# Title:   Utilities to interact with Google Distance Matrix, HERE, and OSRM APIs
# Date:    November 2016
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

# Load common libraries
library(data.table)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(httr)
library(listviewer) # pretty print complex lists
library(rhandsontable) # pretty tables


setwd("~/Projects/hc-shiny/traveltime")
load("./tmp/traveltime.RData")

# Keys
# AIzaSyDFB0iBHCv7L3apVbHfXOJb5fwRJecSkck
api_key_goog <- "AIzaSyDtQ4aW92HbdUOwDfgtKUrEngIfgoJSThA"
api_key_here <- c(app_id="JyAEQWONPJRdajOHClCc", app_code="eCV-jVYOlYybP1cALBLe-g")

apiList <- c(
  `Do not compare`="NONE",
  `Google Distance Matrix`="GOOG",
  `HERE Routing Matrix`="HERE",
  `Open Source Routing Machine`="OSRM"
)

# Get HarvestChoice travel time rasters and create tiles
tt <- hcapi3::hcapi(c("tt10_20k", "tt10_50k", "tt10_100k", "tt10_250k", "tt10_500k"))
tt <- SpatialPixelsDataFrame(tt[, .(X, Y)], data.frame(tt),
  proj4string=CRS("+init=epsg:4326"))
tt <- raster::brick(tt[, c("tt10_20k", "tt10_50k", "tt10_100k", "tt10_250k", "tt10_500k")])
raster::writeRaster(tt, "./tt.tif")
# Save into 8-bit GeoTIFF
r <- raster::stretch(tt, './tt.tif', datatype='INT1U')
raster::writeRaster(r, "./tt10.tif", bylayer=T, datatype="INT1U")


# Load some hhld locations from the TZA LSMS-ISA (or from AR)
rm(list=ls())
load("./tmp/traveltime.RData")
load("../ar/data/ARPoints.RData")

initGPS <- data.table(ar@data)
initGPS <- initGPS[country=="United Republic of Tanzania"]
initGPS <- initGPS[, .SD, .SDcols=c("lon", "lat", "id")]
setnames(initGPS, c("X", "Y", "ID"))
initGPS[, ID := paste0("Loc ", formatC(1:nrow(initGPS), flag="0", width=2))]


# Validate Google API
initResults <- google_api(initGPS[1:2], initGPS[3:4], api_key_goog)
jsonedit(tmp$res)
View(tmp$data)

# Save this workspace for the TravelTime visualization
rm(list=ls()[!ls() %in% c("initGPS", "initResults", "api_key_goog", "api_key_here",
  "apiList", "je_simple_style")])

save.image("./tmp/traveltime.RData")


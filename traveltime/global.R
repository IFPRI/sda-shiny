#####################################################################################
# Title:   Utilities to interact with Google Distance Matrix, HERE, and OSRM APIs
# Date:    November 2016
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

# Load common libraries
library(shiny)
library(shinyBS)
library(data.table)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(httr)
library(listviewer) # pretty print complex lists
library(rhandsontable) # pretty tables

options(shiny.trace=TRUE)

api_key <- "AIzaSyDFB0iBHCv7L3apVbHfXOJb5fwRJecSkck"

apiList <- c(
  `Do not compare`="NONE",
  `Google Distance Matrix`="GOOG",
  `HERE Routing`="HERE",
  `OpenStreetMap Routing`="OSRM")

initGPS <- data.table(
  ID=c("01", "02", "03"),
  X=c(35.85439, 39.25198, 36.72286),
  Y=c(-5.085751, -6.860888, -6.456619))

je_simple_style <- function(je) {
  # for now manually add elementId
  elid <- sprintf(
    "jsonedit-%s",
    htmlwidgets:::createWidgetId()
  )
  # add the elementId to our jsonedit
  #  this will be much easier in future
  je$elementId <- elid

  # use prependContent for custom styling
  htmlwidgets::prependContent(
    je,
    htmltools::tags$style(
      sprintf(
        "
#%s .jsoneditor-menu {
  display: none;
}
#%s .jsoneditor {
  border: none;
}
",
elid,
elid
      )
    )
  )
}







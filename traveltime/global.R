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
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(httr)
library(listviewer) # pretty print complex lists
library(rhandsontable) # pretty tables

options(shiny.trace=TRUE)

load("./tmp/traveltime.RData")

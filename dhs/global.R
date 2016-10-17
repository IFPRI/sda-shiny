#####################################################################################
# Title: Visualize DHS Regional Estimates
# Date: Januray 2015
# Project: HarvestChoice for A4NH
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

library(shiny)
library(shinyBS)
library(data.table)
library(leaflet)
library(RColorBrewer)
library(rCharts)

load("./data/dhsMap.2014.10.16.RData")



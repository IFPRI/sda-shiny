#####################################################################################
# Title:   Utilities to interact with Google Distance Matrix, HERE, and OSRM APIs
# Date:    November 2016
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################


#####################################################################################
# Helper - Archive spatial formats for download
#####################################################################################
writeRasterZip <- function(x, file, filename) {
  require(raster)

  # Convert to spatial
  x <- rbind(
    x[, .(ID=from, X=X_from, Y=Y_from)],
    x[, .(ID=to, X=X_to, Y=Y_to)])
  setkey(x, ID)
  x <- unique(x)
  x <- SpatialPointsDataFrame(x[, .(X,Y)], data.frame(x),
    proj4string=CRS("+init=epsg:4326"))

  # Save
  shapefile(x, filename, overwrite=T)
  f <- list.files(pattern=paste0(strsplit(filename, ".", fixed=T)[[1]][1], ".*"))
  zip(paste0(filename, ".zip"), f, flags="-9Xjm", zip="zip")
  file.copy(paste0(filename, ".zip"), file)
  file.remove(paste0(filename, ".zip"))
}



#####################################################################################
# Helper - Google Distance Matrix
#####################################################################################
google_api <- function(f, t, key) {

  if (ncol(f)==1) {
    fromstr <- paste(f$ID, collapse="|")
  } else {
    fromstr <- paste(f$Y, f$X, sep=",", collapse="|")
  }

  if (ncol(t)==1) {
    tostr <- paste(t$ID, collapse="|")
  } else {
    tostr <- paste(t$Y, t$X, sep=",", collapse="|")
  }

  # Make empty table structure
  dt <- data.table(
    from=character(),
    to=character(),
    status=character(),
    dist_str=character(),
    dist_m=numeric(),
    time_str=character(),
    time_hrs=numeric(),
    X_from=numeric(),
    Y_from=numeric(),
    X_to=numeric(),
    Y_to=numeric()
    )

  nm <- names(dt)[c(2,10,11,3:7)]
  url <- "https://maps.googleapis.com/maps/api/distancematrix/json"
  out <- GET(url, query=list(origins=fromstr, destinations=tostr, mode="driving", key=key))
  out <- fromJSON(content(out, as="text"))

  if(is.null(out$rows$elements)) return(list(response=out, data=dt))

  # Convert JSON response to data.table and clean up
  dt <- lapply(out$rows$elements, function(x) cbind(
    t$ID, t$X, t$Y, x$status, x$distance, x$duration))

  dt <- lapply(dt, data.table)
  dt <- lapply(dt, function(x) setnames(x, nm[1:ncol(x)]))
  dt <- rbindlist(dt, fill=T)
  setnames(dt, nm[1:ncol(dt)])

  dt[, from := rep(f$ID, each=nrow(t))]
  dt[, X_from := rep(f$X, each=nrow(t))]
  dt[, Y_from := rep(f$Y, each=nrow(t))]
  dt[, X_to := as.numeric(X_to)]
  dt[, Y_to := as.numeric(Y_to)]

  # Append empty columns if needed
  if (ncol(dt) < 8) dt[, `:=`(
    dist_str=as.character(NA),
    dist_m=as.nmeric(NA),
    time_str=as.character(NA),
    time_hrs=as.numeric(NA))]

  dt[, time_hrs := time_hrs/(60*60)]
  setcolorder(dt, c("from", "to", "status",
    "dist_str", "time_str", "dist_m", "time_hrs",
    "X_from", "Y_from", "X_to", "Y_to"))

  return(list(response=out, data=dt))
}

#####################################################################################
# Helper - HERE Routing API
#####################################################################################
here_api <- function(from, to, key) {
  url <- "https://maps.googleapis.com/maps/api/distancematrix/json"
  out <- GET(url, query=list(origins=from, destinations=to, mode="driving", key=key))
  out <- fromJSON(content(out, as="text"))
  # Keep `time` only (in seconds)
  return(out$rows$elements[[1]]$duration$value)
}

#####################################################################################
# Helper - Make Polylines
#####################################################################################
toPolyLines <- function(x, coords) {
  l <- x[, .SD, .SDcols=coords]
  l <- lapply(1:nrow(l), function(i) list(L=Line(matrix(unlist(l[i,]), ncol=2, byrow=T)), i=i))
  l <- SpatialLines(lapply(l, function(E) Lines(list(E$L),as.character(E$i))),
    proj4string=CRS("+init=epsg:4326"))
  l <- SpatialLinesDataFrame(l, data.frame(x))
  return(l)
}


#####################################################################################
# Helper - Validate points from CSV strings
#####################################################################################

mapPoints <- function(x, group, session=NULL) {

  # Empty table
  dt.na <- data.table(ID=as.character(), X=numeric(), Y=numeric())

  closeAlert(session, "alert1")
  closeAlert(session, "alert2")
  dt <- try(fread(x))

  if (class(dt)[1] != "data.table") {
    # Raise alert
    createAlert(session, alertId="alert1",
      anchorId=switch(group, Origins="alertFrom", Destinations="alertTo"),
      content="Not a valid CSV input, please correct.",
      style="danger", append=F)
    dt <- dt.na

  } else if (length(setdiff(names(dt), c("ID", "X", "Y"))) > 0) {
    # Raise alert
    createAlert(session, alertId="alert1",
      anchorId=switch(group, Origins="alertFrom", Destinations="alertTo"),
      content=paste0("This seems valid CSV but I read columns <code>",
        paste(names(dt), collapse="</code>, <code>"),
        "</code> instead of the expected <code>ID</code>, <code>X</code>, and
          <code>Y</code>. Please correct."),
      style="danger", append=F)
    dt <- dt.na
  }

  return(dt)
}





#####################################################################################
# Main
#####################################################################################

shinyServer(function(input, output, session) {

  # Init reactive values
  values <- reactiveValues(
    dtFrom = initGPS[1:2],
    dtTo = initGPS[3:4],
    api1 = "GOOG",
    api2 = "NONE",
    api_key_goog = api_key_goog,
    api_key_here = api_key_here,
    mapTitle = "Google Distance Matrix",
    res = initResults
  )

  # Init map
  output$map <- renderLeaflet(
    leaflet() %>%
      # Init view
      setView(mean(initGPS$X), mean(initGPS$Y), 6) %>%
      addTiles(
        #urlTemplate="http://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png",
        urlTemplate="http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution="OSM | HarvestChoice") %>%

      # Add HarvestChoice market access layers
      addTiles(
        options=tileOptions(opacity=.5),
        urlTemplate="http://tile.harvestchoice.org/tt10_20k/tt10_20k/{z}/{x}/{y}.png",
        group="Travel Times 20k") %>%
      hideGroup("Travel Times 20k") %>%

      addTiles(
        options=tileOptions(opacity=.5),
        urlTemplate="http://tile.harvestchoice.org/tt10_50k/tt10_50k/{z}/{x}/{y}.png",
        group="Travel Times 50k") %>%
      hideGroup("Travel Times 50k") %>%

      addTiles(
        options=tileOptions(opacity=.5),
        urlTemplate="http://tile.harvestchoice.org/tt10_100k/tt10_100k/{z}/{x}/{y}.png",
        group="Travel Times 100k") %>%
      hideGroup("Travel Times 100k") %>%

      # # Add draw toolbar
      # addDrawToolbar(layerId="draw", group="Drawn",
      #   editOptions=editToolbarOptions(),
      #   polylineOptions=F, polygonOptions=F, rectangleOptions=F, circleOptions=F,
      #   markerOptions=drawMarkerOptions(repeatMode=T)) %>%

      # Add legends
      addLegend(
        title="Travel Times", layerId="lgdTT",
        colors=rev(RColorBrewer::brewer.pal(8, "Spectral")),
        labels=c("0 hr", "2 hrs", "4 hrs", "6 hrs", "8 hrs", "10 hrs", "12 hrs", "20 hrs"),
        position="bottomright", className="info legend small") %>%

      addLegend(
        title="Locations", layerId="lgdPts",
        colors=c("blue", "red"),
        labels=c("origin", "destination"),
        position="bottomright", className="info legend small") %>%

      # Add layer controls
      addLayersControl(
        overlayGroups=c(
          "Origins", "Destinations",
          "Travel Times 20k", "Travel Times 50k", "Travel Times 100k"),
        position="bottomleft",
        options=layersControlOptions(collapsed=F))


  )

  # Add dynamic point layers (react on values$dtFrom)
  observeEvent(values$dtFrom,
    leafletProxy("map", session) %>%
      clearGroup("Origins") %>%
      setView(mean(values$dtFrom$X), mean(values$dtFrom$Y), 6) %>%
      addCircleMarkers(lng=~X, lat=~Y, layerId=~ID, data=values$dtFrom,
        color="blue", weight=2, radius=8, fillOpacity=.4,
        popup=~paste0(
          "<label>Location</label><br/>", ID,
          "<br/><label>Longitude</label><br/>", X,
          "<br/><label>Latitude</label><br/>", Y),
        group="Origins")
  )

  # Add dynamic point layers (react on values$dtTo)
  observeEvent(values$dtTo,
    leafletProxy("map", session) %>%
      clearGroup("Destinations") %>%
      setView(mean(values$dtTo$X), mean(values$dtTo$Y), 6) %>%
      addCircleMarkers(lng=~X, lat=~Y, layerId=~ID, data=values$dtTo,
        color="red", weight=2, radius=8, fillOpacity=.4,
        popup=~paste0(
          "<label>Location</label><br/>", ID,
          "<br/><label>Longitude</label><br/>", X,
          "<br/><label>Latitude</label><br/>", Y),
        group="Destinations")
  )

  # Map title
  observeEvent(input$selectAPI1, values$api1 <- input$selectAPI1)
  output$mapTitle <- renderText(names(apiList)[apiList==values$api1])

  # Results
  output$tbResults <- renderRHandsontable(rhandsontable(
    values$res$data[, .SD, .SDcols=-c(6,7)],
    colHeaders=c("From", "To", "Status", "Distance", "Time",
      "X-From", "Y-From", "X-To", "Y-To"),
    readOnly=T, width="100%", stretchH="all"))

  # JSON responses
  output$jsResults <- renderJsonedit(jsonedit(values$res$response) %>%
      je_simple_style())


  # Primary observer (react to main button)
  observeEvent(input$btnMain, {
    leafletProxy("map") %>% showGroup("Origins") %>% clearGroup("Distances")
    validtoAPI()
  })

  # Map observers (redraw the map only if valid CSV input)
  observeEvent(input$btnFrom, {
    leafletProxy("map") %>% showGroup("Origins") %>% clearGroup("Distances")
    values$dtFrom <- mapPoints(input$txtFrom, "Origins", session)
  })

  observeEvent(input$btnTo, {
    leafletProxy("map") %>% showGroup("Origins") %>% clearGroup("Distances")
    values$dtTo <- mapPoints(input$txtTo, "Destinations", session)
  })

  # Refresh API keys
  observeEvent(input$btnKeyGOOG, values$api_key_goog <- input$txtKeyGOOG)
  observeEvent(input$btnKeyHERE, values$api_key_here <- input$txtKeyHERE)

  # show distance labels on click
  observeEvent(input$map_marker_click, {
    evt <- input$map_marker_click
    if(is.null(evt)) return()

    # Undo on subsequent mouse clicks
    if (evt$id=="selected") {
      leafletProxy("map") %>% showGroup("Origins") %>% clearGroup("Distances")
      return()
    }

    # Show times
    dt <- values$res$data[from==evt$id]
    if(nrow(dt)==0) return()

    leafletProxy("map") %>%
      hideGroup("Origins") %>%
      clearGroup("Distances") %>%
      addCircleMarkers(evt$lng, evt$lat, color="yellow", radius=8, fillOpacity=.8,
        layerId="selected", group="Distances") %>%
      addLabelOnlyMarkers(data=dt,
        lng=~X_to, lat=~Y_to,
        label=~gsub("NA", "--", paste(dist_str, time_str, sep=" | ")),
        labelOptions=labelOptions(noHide=T, direction="top"),
        group="Distances")

  })


  # Hide details on random map click
  observeEvent(input$map_click, {
    evt <- input$map_marker_click
    #if(!is.null(evt)) return()
    leafletProxy("map") %>% showGroup("Origins") %>% clearGroup("Distances")
  })


  # Main validation
  validtoAPI <- function() {

    # Validate CSV input
    from <- mapPoints(input$txtFrom, "Origins", session)
    to <- mapPoints(input$txtTo, "Destinations", session)

    values$dtFrom <- from
    values$dtTo <- to

    # Do not send more than 200 requests
    apilimit <- ifelse(values$api_key_goog==api_key_goog, 200, 1000)

    if (nrow(from) > 0 & nrow(to)>0 & nrow(from)*nrow(to) <= apilimit) {
      # Hit API
      values$res <- google_api(from, to, values$api_key_goog)
      #values$res <- here_api(from, to, values$api_key_here)

    } else {
      # Raise alert
      createAlert(session, alertId="alert1", anchorId="alertFrom",
        content="Limit your request to 1,000 pairs of points if using your own API keys,
        or to 200 pairs if using the default keys.",
        style="danger", append=F)
    }
  }



  # Download handler
  output$btnSave <- downloadHandler(function() {
    t <- ifelse(input$fileType=="shp", "zip", input$fileType)
    paste0("pointsDistanceMatrix_", Sys.Date(), ".", t)

  }, function(file) {
    switch(input$fileType,
      csv = write.csv(values$res$data, file, row.names=F, na=""),
      shp = writeRasterZip(values$res$data, file, "pointsDistanceMatrix.shp"),
      pdf = printMap(values$res$data, file)
    )
  })


  # Capture marker drag events
  observeEvent(input$map_draw_edited_features, {
    evt <- input$map_draw_edited_features
    # evt <- lapply(evt, function(x) data.table(X=x$lng, Y=x$lat))
    # evt <- rbindlist(evt)
    # evt[, ID := paste("Drawn", formatC(1:nrow(evt), width=2, flag=0))]
    #updateTextAreaInput(session, "txtFrom", value=write.csv(evt, row.names=F))
    createAlert(session, anchorId="alertFrom", content="Locations have been updated.")
    leafletProxy("map") %>% clearGroup("Drawn")
    output$jsResults <- renderJsonedit(jsonedit(evt) %>% je_simple_style())
    #values$dtFrom <- evt
  })



})


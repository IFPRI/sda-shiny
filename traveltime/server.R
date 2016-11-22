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

  nm <- names(dt)
  url <- "https://maps.googleapis.com/maps/api/distancematrix/json"
  out <- GET(url, query=list(origins=fromstr, destinations=tostr, mode="driving", key=key))
  out <- fromJSON(content(out, as="text"), flatten=T)

  if(is.null(out$rows$elements)) return(list(response=out, data=dt))

  # Convert JSON response to data.table and clean up
  dt <- rbindlist(out$rows$elements, fill=T)
  setnames(dt, c("status", "dist_str", "dist_m", "time_str", "time_hrs"))
  dt[, from := rep(f$ID, each=nrow(t))]
  dt[, X_from := rep(f$X, each=nrow(t))]
  dt[, Y_from := rep(f$Y, each=nrow(t))]
  dt[, to := rep(t$ID, nrow(f))]
  dt[, X_to := rep(t$X, nrow(f))]
  dt[, Y_to := rep(t$Y, nrow(f))]
  dt[, time_hrs := time_hrs/(60*60)]
  setcolorder(dt, nm)

  return(list(response=out, data=dt))
}

#####################################################################################
# Helper - HERE Routing API
#####################################################################################
here_api <- function(f, t, key) {

  # Make empty table structure
  dt <- data.table(
    from=character(),
    to=character(),
    cost=numeric(),
    X_from=numeric(),
    Y_from=numeric(),
    X_to=numeric(),
    Y_to=numeric()
  )

  from <- paste(f$Y, f$X, sep=",")
  to <- paste(t$Y, t$X, sep=",")
  names(from) <- paste0("start", 1:length(from)-1)
  names(to) <- paste0("destination", 1:length(to)-1)
  l <- c(from, to, mode="fastest;car;traffic:disabled", key[1], key[2])

  url <- "https://matrix.route.cit.api.here.com/routing/7.2/calculatematrix.json"
  out <- GET(url, query=as.list(l))
  out <- fromJSON(content(out, as="text"), flatten=T)

  if(is.null(out$response$matrixEntry)) return(list(response=out, data=dt))
  dt <- data.table(out$response$matrixEntry)

  # Combine with from/to coords and loc names
  f[, startIndex := 1:nrow(f)-1]
  t[, destinationIndex := 1:nrow(t)-1]
  setkey(f, startIndex)
  setkey(dt, startIndex)
  dt[f, from := ID]
  dt[f, X_from := X]
  dt[f, Y_from := Y]
  setkey(t, destinationIndex)
  setkey(dt, destinationIndex)
  dt[t, to := ID]
  dt[t, X_to := X]
  dt[t, Y_to := Y]
  dt[, `:=`(startIndex=NULL, destinationIndex=NULL)]
  setnames(dt, 1, "cost")
  setcolorder(dt, c("from", "to", "cost", "X_from", "Y_from", "X_to", "Y_to"))

  return(list(response=out, data=dt))
}


#####################################################################################
# Helper - OSRM Routing API (demo server)
#####################################################################################
osrm_api <- function(f, t, key=NULL) {

  # Make empty table structure
  dt <- data.table(
    from=character(),
    to=character(),
    time_str=character(0),
    time_hrs=numeric(),
    X_from=numeric(),
    Y_from=numeric(),
    X_to=numeric(),
    Y_to=numeric()
  )

  url <- "https://router.project-osrm.org/table/v1/driving/"
  url <- paste0(url,
    paste(f$X, f$Y, sep=",", collapse=";"), ";", paste(t$X, t$Y, sep=",", collapse=";"))
  out <- GET(url,
    query=list(
      sources=paste(1:nrow(f)-1, collapse=";"),
      destinations=paste(nrow(f):(nrow(f)+nrow(t)-1), collapse=";")))
  out <- fromJSON(content(out, as="text"), flatten=T)

  if(is.null(out$durations)) return(list(response=out, data=dt))
  dt <- as.data.table(out$durations)
  dt[, from := f$ID]
  setnames(dt, c(t$ID, "from"))
  dt <- melt(dt, id.vars="from", variable.name="to", value.name="time_hrs")
  dt[, time_str := sprintf("%d hrs %d min", time_hrs%/%(60*60), time_hrs%%(60*60)%/%60)]
  dt[, time_hrs := time_hrs/(60*60)]

  # Combine with from/to coords and loc names
  setkey(f, ID)
  setkey(dt, from)
  dt[f, X_from := X]
  dt[f, Y_from := Y]
  setkey(t, ID)
  setkey(dt, to)
  dt[t, X_to := X]
  dt[t, Y_to := Y]
  setcolorder(dt, c("from", "to", "time_str", "time_hrs", "X_from", "Y_from", "X_to", "Y_to"))

  return(list(response=out, data=dt))
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
      # addDrawToolbar(group="Origins",
      #   editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions()),
      #   polylineOptions=F, polygonOptions=F, rectangleOptions=F, circleOptions=F,
      #   markerOptions=F) %>%

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
  output$mapTitle <- renderText(names(apiList)[apiList==values$api1])

  # Results
  output$tbResults <- renderRHandsontable(switch(values$api1,
    GOOG = rhandsontable(values$res$data[, .SD, .SDcols=-c(5,7)],
      colHeaders=c("From", "To", "Status", "Distance", "Time",
        "Lng (From)", "Lat (From)", "Lng (To)", "Lat (To)"),
      height=min(40+nrow(values$res$data)*22, 487),
      readOnly=T, width="100%", stretchH="all"),
    HERE = rhandsontable(values$res$data,
      colHeaders=c("From", "To", "Cost Factor",
        "Lng (From)", "Lat (From)", "Lng (To)", "Lat (To)"),
      height=min(40+nrow(values$res$data)*22, 487),
      readOnly=T, width="100%", stretchH="all"),
    OSRM = rhandsontable(values$res$data[, .SD, .SDcols=-c(4)],
      colHeaders=c("From", "To", "Time",
        "Lng (From)", "Lat (From)", "Lng (To)", "Lat (To)"),
      height=min(40+nrow(values$res$data)*22, 487),
      readOnly=T, width="100%", stretchH="all"),
  ))

  # JSON responses
  output$jsResults <- renderJsonedit(jsonedit(values$res$response))


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
  observeEvent(input$btnKeyGOOG,
    if(length(input$btnKeyGOOG)>8) values$api_key_goog <- input$txtKeyGOOG)
  observeEvent(input$btnKeyHERE,
    if(length(input$btnKeyHERE)>8) values$api_key_here <- c(input$txtKeyHEREid, input$txtKeyHEREcode))

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
        label=~switch(values$api1,
          GOOG=gsub("NA", "--", paste(dist_str, time_str, sep=" | ")),
          HERE=gsub("NA", "--", prettyNum(cost, big.mark=",")),
          OSRM=gsub("NA", "--", time_str)),
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

    # Update map
    values$dtFrom <- from
    values$dtTo <- to

    # Do not send more than 200 requests
    apilimit <- ifelse(values$api_key_goog==api_key_goog, 200, 1000)

    if (nrow(from) > 0 & nrow(to)>0 & nrow(from)*nrow(to) <= apilimit) {
      # Hit API
      values$api1 <- input$selectAPI1

      switch(values$api1,
        GOOG = {
          values$res <- google_api(from, to, values$api_key_goog)
          output$txtNoteHERE <- renderText("")
        },
        HERE = {
          values$res <- here_api(from, to, values$api_key_here)
          output$txtNoteHERE <- renderText("HERE Cost Factor is an internal cost
used for calculating the route. This value is based on the objective function of the routing
engine and related to the distance or time, depending on the request settings (such as
pedestrian versus car routes). The value may include certain penalties and represents
the overall quality of the route with respect to the input parameters.")
        },
        OSRM = {
          values$res <- osrm_api(from, to)
          output$txtNoteHERE <- renderText("")
        })

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
    paste0("pointsDistanceMatrix_", values$api1, "_", Sys.Date(), ".", t)

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
    #leafletProxy("map") %>% clearGroup("Drawn")
    output$jsResults <- renderJsonedit(jsonedit(evt) %>% je_simple_style())
    #values$dtFrom <- evt
  })



})


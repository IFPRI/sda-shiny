#####################################################################################
# Title:   Utilities to interact with Google Distance Matrix, HERE, and OSRM APIs
# Date:    November 2016
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################



#####################################################################################
# Helper - Google Distance Matrix
#####################################################################################

google_api <- function(from, to, key) {

  if (ncol(from)==1) {
    from <- paste(from$ID, collapse="|")
  } else {
    from <- paste(from$X, from$Y, sep=",", collapse="|")
  }

  if (ncol(to)==1) {
    to <- paste(to$ID, collapse="|")
  } else {
    to <- paste(to$X, to$Y, sep=",", collapse="|")
  }

  url <- "https://maps.googleapis.com/maps/api/distancematrix/json"
  out <- GET(url, query=list(origins=from, destinations=to, mode="driving", key=key))
  out <- fromJSON(content(out, as="text"))

  # Keep `time` only (in seconds)
  return(list(
    response=out$all_headers[[1]],
    data=out$rows$elements[[1]]))
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

mapPoints <- function(x, group, session) {

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
    dtFrom = initGPS[1],
    dtTo = initGPS[2:3],
    api1 = "GOOG",
    api2 = "NONE",
    mapTitle = "Google Distance Matrix",
    res = google_api(initGPS[1], initGPS[2:3], api_key)
  )

  # Init dynamic map (map reacts on values$dtFrom and values$dtTo)
  output$map <- renderLeaflet(
    leaflet() %>%
      # Init view
      setView(mean(initGPS$X), mean(initGPS$Y), 6) %>%
      addTiles(
        urlTemplate="http://{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution="Mapbox") %>%

      # TODO Add HC market access

      # Add draw toolbar
      addDrawToolbar(layerId="lyrFrom", group="Origins",
        editOptions=editToolbarOptions(),
        polylineOptions=F, polygonOptions=F, rectangleOptions=F, circleOptions=F,
        markerOptions=drawMarkerOptions(repeatMode=T)) %>%

      # Add legend
      addLegend(
        title="Locations",
        colors=c("blue", "red"),
        labels=c("origin", "destination"),
        position="bottomright", layerId="lgd")
  )

  # Add dynamic point layers (react on values$dtFrom)
  observeEvent(values$dtFrom,
    leafletProxy("map", session) %>%
      clearGroup("Origins") %>%
      setView(mean(values$dtFrom$X), mean(values$dtFrom$Y), 6) %>%
      addCircleMarkers(lng=~X, lat=~Y, layerId=~ID, data=values$dtFrom,
        color="blue", weight=2, radius=6, fillOpacity=.4,
        options=markerOptions(draggable=T, title=~ID),
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
        color="red", weight=2, radius=6, fillOpacity=.4,
        popup=~paste0(
          "<label>Location</label><br/>", ID,
          "<br/><label>Longitude</label><br/>", X,
          "<br/><label>Latitude</label><br/>", Y),
        group="Destinations")
  )

  # Map title
  output$mapTitle <- renderText(values$mapTitle)

  # Results
  output$tbResults <- renderRHandsontable(rhandsontable(values$res$data))

  # JSON responses
  output$jsResults <- renderJsonedit(je_simple_style(jsonedit(values$res$response)))


  # Primary observer (react to main button)
  observeEvent(input$btnMain, {
    # Update reactive values
    from <- mapPoints(input$txtFrom, "Origins", session)
    to <- mapPoints(input$txtTo, "Destinations", session)

    if (!class(values$from)[1]=="try-error" & !class(values$to)[1]=="try-error") {
      # Trigger results
      values$res <- google_api(from, to, api_key)
      values$dtFrom <- from
      values$dtTo <- to
    }

  })

  # Map observers (redraw the map only if valid CSV input)
  observeEvent(input$btnFrom, values$dtFrom <- mapPoints(input$txtFrom, "Origins", session))
  observeEvent(input$btnTo, values$dtTo <- mapPoints(input$txtTo, "Destinations", session))





  # Drawing tools for origin points
  observeEvent(input$map_Points_saved, {

    # Clear map
    leafletProxy("map", session) %>% clearGroup("Origins")

    # Capture coordinates
    X <- input$map_Points_created$geometry$coordinates[[1]]
    Y <- input$map_Points_created$geometry$coordinates[[2]]
    dt <- data.table(ID=1:length(X), X=X, Y=Y)
    updateTextAreaInput(session, "txtFrom", value=wite.csv(dt, row.names=F))
    values$dtFrom <- dt

  })

  # Capture marker drag events
  observeEvent(input$map_marker_dragend, {

    evt <- as.data.table(input$map_marker_dragend)
    setnames(evt, c("Y", "X", "ID"))
    dt <- values$dtFrom
    dt <- dt[!evt]
    dt <- rbind(dt, evt, fill=T)
    setkey(dt, ID)
    values$dtFrom <- dt
    updateTextAreaInput(session, "txtFrom", value=write.csv(dt, row.names=F))
    createAlert(session, anchorId="alertFrom", content="Locations have been updated.")
  })

  # # Drawing tools for destination points
  # observeEvent(input$map_lyrTo_created, {
  #   X <- input$map_lyrTo_created$geometry$coordinates[[1]]
  #   Y <- input$map_lyrTo_created$geometry$coordinates[[2]]
  #   dt <- data.table(ID=1:length(X), X=X, Y=Y)
  #   updateTextAreaInput(session, "txtTo", value=toJSON(dt[, .(X, Y)]))
  #   values$dtTo <- dt
  # })





})


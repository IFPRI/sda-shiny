#####################################################################################
# Title:   Utilities to interact with Google Distance Matrix, HERE, and OSRM APIs
# Date:    November 2016
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

shinyUI(fluidPage(
  title="HarvestChoice | Routing and Travel Times",
  theme="../assets/bootstrap.css",
  tags$head(includeScript("../assets/ga.js")),

  # Header
  fluidRow(class="hc",
    column(9,
      h3("Travel Times for sub-Saharan Africa",
        tags$small("Market access and road networks"))),
    column(2, offset=1,
      h5(a(href="http://harvestchoice.org/", title="Home",
        img(src="../assets/global_logo.png", alt="Home"))))
  ),

  # Maps
  fluidRow(style="position:relative;",
    leafletOutput("map", height=380, width="100%"),
    absolutePanel(class="well well-sm", top=5, right=5, width="20%", height="auto",
      tags$label(htmlOutput("mapTitle")),
      p("Click any of the origin locations to view travel times to destinations.
        Click again to hide.", class="small"))
  ),

  # Main
  fluidRow(

    column(4,

      p(br()),
      p("Use this tool to send requests to travel distance and routing APIs provided
        by Google, HERE, and OpenStreetMap."),
      selectInput("selectAPI1", "Select a service to use for this request",
        apiList[-1], selected="GOOG"),
      # selectInput("selectAPI2", "Optionally, compare with this service",
      #   apiList, selected="NONE"),

      hr(),
      p("You will need to provide", strong("your own API keys"), "to send large requests
        to",
        a("Google", href="https://developers.google.com/maps/documentation/distance-matrix/usage-limits"),
        "and",
        a("HERE", href="https://developer.here.com/rest-apis/documentation/routing/topics/quick-start.html"),
        "APIs. Without a key requests are limited to 200 pairs of locations. Please use the
        links here and register if needed. Else requests will be limited to 5 origin
        and destination locations."),

      textInput("txtKeyGOOG", "Your Google API key",
        placeholder="Sign in with Google and enter your API key"),
      actionLink("btnKeyGOOG", "update", icon("refresh")),

      p(br()),

      textInput("txtKeyHERE","Your HERE API key",
        placeholder="Sign in with HERE and enter your API key"),
      actionLink("btnKeyHERE", "update", icon("refresh")),

      # Credits
      hr(),
      includeMarkdown("./www/txtCredits.md")

    ),

    column(8,

      h3("Origins and Destinations"),
      p("Use a CSV notation with at least a",
        code("X"), code("Y"), "and", code("ID"),
        "columns, or a simple list of addresses to geocode."),
      p(br()),

      fluidRow(

        column(6,
          # Origins
          actionLink("btnFrom", "Map origin locations", icon("globe"), style="float:right;"),
          div(class="fix",
            textAreaInput("txtFrom", width="100%", rows=9, resize="vertical",
              label="Origin locations",
              value='
"X","Y","ID"
36.371494,-5.6112156,"Loc 01"
35.89239,-4.2926636,"Loc 02"')
          ),
          bsAlert("alertFrom")
        ),

        column(6,
          # Destinations
          actionLink("btnTo", "Map destination locations", icon("globe"), style="float:right;"),
          div(class="fix",
            textAreaInput("txtTo", width="100%", rows=9, resize="vertical",
              label="Destination locations",
              value='
"X", "Y", "ID"
35.85439, -5.085751, "Location 1"
39.25198, -6.860888, "Location 2"
36.72286, -6.456619, "Location 3"')
          ),
          bsAlert("alertTo")
        )
      ),

      actionButton("btnMain", "Generate Travel Times", icon("exchanges"), class="btn-primary"),

      # Results
      h3("Travel Times"),
      p("Results are shown in the table below. The entire API response in JSON format
        is also available. Use the download options below to save your results.
        You can also use your keyboard Crtl+C/Ctrl+V to copy and paste entries."),

      tabsetPanel(
        tabPanel("Table",
          p(br(), "Driving time for each pair of locations."),
          rHandsontableOutput("tbResults", width="100%")
        ),
        tabPanel("JSON Response",
          p(br(), "Entire JSON response."),
          jsoneditOutput("jsResults", height="280px")
        )
      ),

      p(br()),

      # Export
      div(style="float: left; margin-right: 15px;",
        selectInput("fileType", "Choose Export Format",
          choices=c(
            `ESRI Shapefile`="shp",
            `Comma-separated (CSV)`="csv",
            `PDF Map Document`="pdf"),
          selected="csv")
      ),

      HTML("<label>&nbsp;</label><br />"),
      downloadButton("btnSave", "Save Results", class="btn-info"),

      p(br(clear="left"), "Choose ESRI Shapefile to save the point locations
        shown on the maps, CSV to export a table of travel time statistics for the
        selected pairs of points. Choose PDF to export the map in a printable format.")
    )
  ),

  # Footer
  fluidRow(class="hc-footer",
    column(3,
      p("HarvestChoice generates knowledge products to help guide strategic investments
        to improve the well-being of poor people in sub-Saharan Africa through more
        productive and profitable farming.")
    ),

    column(3,
      p("©IFPRI/HarvestChoice, 2015. Source code on",
        a(href="https://github.com/harvestchoice/hc-shiny/tree/master/traveltime/", "GitHub."),
        "Powered by", a(href="http://shiny.rstudio.com/", "RStudio Shiny."),
        "Code and datasets are licensed under a",
        a(href="http://creativecommons.org/licenses/by-nc-sa/4.0/",
          "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License."))
    ),

    column(2, a(style="color:#3C3A2E;", href="http://ifpri.org/", img(src="../assets/R_ifpri.png"),
      title="International Food Policy Research Institute")),
    column(2, a(style="color:#3C3A2E;", href="http://www.pim.cgiar.org/", img(src="../assets/R_pim.png"),
      title="CGIAR Research Program on Policies Institutions and Markets")),
    column(2, a(style="color:#3C3A2E;", href="http://umn.edu/", img(src="../assets/R_umn.png"),
      title="University of Minnesota"))
  )

)
)

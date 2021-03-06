#####################################################################################
# Title:   CRU and PDSI 1960-2013 Time Series across Districts
# Date:    December 2014
# Project: HarvestChoice/IFPRI
# Authors: Bacou, Melanie <mel@mbacou.com>
#####################################################################################


shinyUI(fluidPage(
  title="HarvestChoice | Monthly Time-Series fo SSA",
  theme="../assets/bootstrap.css",
  tags$head(includeScript("../assets/ga.js")),

  fluidRow(class="hc",
    column(9,
      h3("Long-Term Climate Trends",
        tags$small("Monthly sub-national time-series for sub-Saharan Africa"))),
    column(2, offset=1,
      h5(a(href="http://harvestchoice.org/", title="Home",
        img(src="../assets/global_logo.png", alt="Home"))))
  ),

  fluidRow(style="position: relative;",
    leafletOutput("map", height=380),

    conditionalPanel(condition="input.btn>0",
      absolutePanel(class="panel panel-default",
        bottom=20, right=20, width=220, height="auto",
        div(class="panel-body",
          uiOutput("details", inline=T)
        )
      )
    )
  ),

  fluidRow(
    column(3,
      p(br()),
      selectInput("var", "Choose a Variable", d, selected="pre"),
      selectInput("selectg0", "Choose a Country", names(g2.list), selected="Kenya"),
      actionButton("btn", "Show Series", icon("globe"), class="btn-primary"),
      hr(),
      conditionalPanel(condition="input.btn>0",
        p("The ", strong("long-term mean"), " is over the selected season
          and years only (or over the entire year if no specific season is selected).
          The ", strong("trend component"), " is generated through classical seasonal decomposition
          by moving averages over the entire 1960-2013 period. ", strong("Total precipitation"), " at
          the bottom is over the selected season. ", strong("Temperature"),
          " is near-surface temperature in Celsius."),
        p("You may use your mouse to zoom into
          any specific time period or use the range selectors below the chart.
          Double-click to reset the chart to its full length."),
        hr()
      ),

      includeMarkdown("./www/txtCredits.md")
    ),

    column(7,
      conditionalPanel(condition="input.btn==0",
        includeMarkdown("./www/txtIntro.md")),
      uiOutput("selectedMsg"),
      bsAlert("alertNoData"),
      p(br()),
      dygraphOutput("dygraph", width="100%", height="320px"),
      p(br()),
      dygraphOutput("dygraphAnnual", width="100%", height="320px"),
      #hr(),
      #h3("Top/Bottom Five Districts"),
      #plotOutput("distBarPlot", width="100%", height="320px"),
      p(br())
    ),

    column(2,
      p(br()),
      selectInput("selectg2", "Limit to District",
        choices=c(`Entire Country`="Entire Country", g2.list[["Kenya"]]), selected="Entire Country", selectize=T),
      sliderInput("selectMonth", "Limit to Season (Jan-Dec)", 1, 12, value=c(1,12), step=1),
      sliderInput("rg", "Limit to Date Range", 1960, 2013, value=c(1960, 2013), step=1, sep="", ticks=F),
      hr(),

      selectInput("fileType", "Choose Export Format", choices=c(
        `ESRI Shapefile`="shp", GeoTIFF="tif", netCDF="nc", CSV="csv", STATA="dta", `PDF Document`="pdf"),
        selected="csv"),

      downloadButton("saveData", "Save Layer"),
      p(br(), tags$label("Export formats"), br(), "NetCDF and GeoTIFF produce monthly
        multi-band rasters over the 1960-2013 period. ESRI Shapefile returns district
        means over the entire period. CSV and STATA formats return a table of monthly
        statistics for the selected country or district.")
    )
  ),

  fluidRow(class="hc-footer",
    column(3,
      p("HarvestChoice generates knowledge products to help guide strategic investments
        to improve the well-being of poor people in sub-Saharan Africa through more
        productive and profitable farming.")
      ),

    column(3,
      p("©IFPRI/HarvestChoice, 2015. Source code on",
        a(href="https://github.com/ifpri/sda-shiny/tree/master/rainfall", "GitHub."),
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

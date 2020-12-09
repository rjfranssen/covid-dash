###################################################
# a COVID-19 dash used to practice js and r functions in a shiny app
#################################################
# References
# json apis: https://cran.r-project.org/web/packages/jsonlite/vignettes/json-apis.html
# COVID Data API: https://covidtracking.com/api
# Postman: https://documenter.getpostman.com/view/8854915/SzS8rjHv?version=latest
# US geojson files: https://eric.clst.org/tech/usgeojson/
# Simplyfying json: https://blog.exploratory.io/creating-geojson-out-of-shapefile-in-r-40bc0005857d
# Johns Hopkins data: https://github.com/CSSEGISandData/COVID-19
#################################################

###################################################
# Load libraries
###################################################
library(shiny)
library(shinydashboard)
library(dygraphs)
library(ggplot2)
library(TTR)
library(rvest)
library(xts)
library(rvest)
library(jsonlite)
library(leaflet)
library(lubridate)
library(httr)
library(dplyr)
library(data.table)
library(leaflet)
library(rgdal)
library(geofacet)
library(zoo)
library(plotly)
library(dashboardthemes)
library(RJSONIO)
options(scipen = 999)

###################################################
# Prep COVID-19 Data
###################################################

# Initialize state metrics
state_metrics <- c()

# Load data from COVID Tracker
states_current <- jsonlite::fromJSON('https://covidtracking.com/api/states', flatten=TRUE)
states_daily <- jsonlite::fromJSON('https://covidtracking.com/api/states/daily')
states_info <- jsonlite::fromJSON('https://covidtracking.com/api/states/info')
us_current <- jsonlite::fromJSON('http://covidtracking.com/api/us')
us_daily <- jsonlite::fromJSON('https://covidtracking.com/api/us/daily')

#Fix dates
#str(states_current$date)
states_current$date <- as.character(states_current$date)
states_current$date <- as.Date(as.character(states_current$date),format="%Y%m%d")

#str(states_daily$date)
states_daily$date <- as.character(states_daily$date)
states_daily$date <- as.Date(as.character(states_daily$date),format="%Y%m%d")

# Join states_info for additional data and state names
states_current <- states_current %>% left_join (states_info, by="state")
states_daily <- states_daily %>% left_join (states_info, by="state")


# Get list of metrics for selection
metrics_options <- c('positive', 'negative', 'hospitalizedCurrently', 'hospitalizedCumulative', 'inIcuCurrently', 'inIcuCumulative', 'onVentilatorCurrently', 'onVentilatorCumulative', 'recovered', 'death', 'hospitalized', 'totalTestsViral', 'positiveTestsViral', 'negativeTestsViral', 'positiveIncrease', 'negativeIncrease', 'total', 'totalTestResults', 'deathIncrease', 'hospitalizedIncrease')

# State Names selector
state_names <- unique(states_current$name)

###################################################
# Need to do pre-processing to create static GeoJSON that I'm going to read in directly for the js leaflet map
# The R leaflet map is going to read in the current GeoJSON info dynamically
# If I had RJSONIO, I could make this dynamic for the js map, too
###################################################

# Create spatialpointsdataframe
states_geo <- rgdal::readOGR("gz_2010_us_040_00_20m.json")

# Simplify the geometry information of GeoJSON (reduce file fize!)
#states_geo <- rmapshaper::ms_simplify(states_geo)

# save the rownames
rnames <- rownames(states_geo@data)

# Add COVID data
states_geo@data <- states_geo@data %>% left_join(states_current, by=c("NAME" = "name"))

# Clone for r leaflet map
states_geo_r <- data.table::copy(states_geo)

#######
# The rest of these steps depend on geojsonio and RJSONIO packages, and I ran them once to be able to save my geojson file with the COVID data locally so I could read it in later.
#######

# reapply rownames
rownames(states_geo@data) <- rnames

# Select handful of columns
states_geo@data <- states_geo@data[c(1, 3, 6, 7, 8, 11, 13, 15, 22)]

# Convert to geojson
states_geojson <- geojsonio::geojson_json(states_geo)

# Save static geojson - next I did a manual step and added `var=` to the beginning of that geojson file and changed it to .js so that I could read it in my app as an object
#geojsonio::geojson_write(states_geojson, file = "www/states.geojson")

# Alternatively, if I wanted to format geojson as a message from shiny to js, I can do this (requires RJSONIO package)
states_geojson_message <- RJSONIO::toJSON(states_geo)


###################################################
# Loading Logo
###################################################
# Takes a location 'href', an image location 'src', a loading gif 'loadingsrc'
# height, width and alt text, and produces a loading logo that activates while
# Shiny is busy
loadingLogo <- function(href, src, loadingsrc, height=40, width=40, alt=NULL) {
  tagList(
    tags$head(
      tags$script(
        "setInterval(function(){
                     if ($('html').attr('class')=='shiny-busy') {
                     $('div.busy').show();
                     $('div.notbusy').hide();
                     } else {
                     $('div.busy').hide();
                     $('div.notbusy').show();
           }
         },100)")
    ),
    tags$a(href=href,
           div(class = "busy",  
               img(src=loadingsrc, height=height, width=width, alt=alt)),
           div(class = 'notbusy',
               img(src=src, height=height, width=width, alt=alt))
    )
  )
}

###################################################
# Alternative Logo (dashboardthemes)
###################################################
custom_header <- shinyDashboardLogoDIY(
  boldText=paste(icon('globe'), 'COVID')
  , mainText="| DATA"
  , textSize=16
  , badgeText="DEV"
  , badgeTextColor="white"
  , badgeTextSize=2
  , badgeBackColor="#40E0D0"
  , badgeBorderRadius=3
)


###################################################
# UI Function
###################################################

ui <- dashboardPage(
  
  ###################################################
  # HEADER
  ###################################################
  dashboardHeader(
    title = custom_header,
    dropdownMenuOutput("messagesMenu")
    #dropdownMenuOutput("notificationsMenu")
    #dropdownMenuOutput("tasksMenu")
  ),
  
  ###################################################
  # SIDEBAR
  ###################################################
  dashboardSidebar(
    sidebarMenu(
      
      menuItem(text="Covid Map (js)", # Note menuItem and menuSubItem are interchangeable and can be nested
               tabName = "covid-map-js", 
               #badgeLabel = "sweet", 
               #badgeColor = "purple",
               icon=icon("map")
      ),
      
      menuItem(text="Covid Map (r)", 
               tabName = "covid-map-r", 
               icon=icon("map")
      ),
      
      # This is the geofacet
      menuItem(text="State Trends",
               tabName = "state-trends",
               icon=icon("chart-line")
      )
    )
  ),
  
  ###################################################
  # BODY
  ###################################################
  dashboardBody(
    
    shinyDashboardThemes(
      theme = "poor_mans_flatly"
    ),
    
    tabItems(
      
      ###################################################
      # ui/covid-map-js/start
      ###################################################
      tabItem(tabName="covid-map-js",
              
              tags$h3(textOutput("header_text")),
              
              # Value boxes
              fluidRow(
  
                # Dynamic valueBoxes
                valueBoxOutput("firstBox"),
                
                valueBoxOutput("secondBox"),
                
                valueBoxOutput("thirdBox")
                
              ),
              
              # Covid map js
              fluidRow(
                box(width=12,
                    #title = "COVID-19 Observations", 
                    title = textOutput("map_box_title_js"),
                    status = "success", # primary (blue), success (green), info (blue), warning (orange), danger (red)
                    
                    
                    
                    # load leaflet libraris
                    # leaflet css
                    tags$head(tags$link(rel = "stylesheet", 
                                        type = "text/css", 
                                        href = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.3.1/leaflet.css")),
                    
                    # leaflet js
                    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.3.1/leaflet.js"),
                    
                    # covid map
                    tags$div(id="map_div_c", style="height:300px;width:auto;"),
                    
                    # Reading in formatted covid data that I exported above (see states.geojson - I added `var=` to read it in)
                    tags$script(src="statesCovidData.js"),
                    
                    # us states tutorial data
                    #tags$script(src="us-states.js"),
                    
                    # load our javascript code (need to load this last)
                    tags$script(src="my_map_script.js"),
                    
                    #plotOutput('ggplot_hospital'),
                    collapsible=TRUE
                )
                
              ),
              
              # Dygraphs
              fluidRow(
                box(width=6,
                    height=800,
  
                    # box title
                    #title = "State Trend", 
                    title=textOutput("dygraph_box_title_js_1"),
                    status = "success",
                    
                    # Time-series selector
                    selectInput("select_ts", "Select Timeframe",
                                c("30-Day History" = "ts_30d",
                                  "60-Day History" = "ts_60d",
                                  "Since First Infection" = "ts_fi",
                                  "Full History" = "ts_full")),
                    
                    #checkboxInput("sma_checkbox", "Include 6-Day SMA"),
                    
                    checkboxInput("gridlines_checkbox", "Include Gridlines"),
                    
                    # load text and table
                    tags$div(
                      #tags$p(verbatimTextOutput('someclicktext')),
                      #tags$p(verbatimTextOutput('someclicktext2'))
                      #dataTableOutput("table") 
                    ),
                    
                    # tutorial map
                    # tags$div(id="map_div", style="height:600px;width:auto;"),
                    
                    conditionalPanel(
                      condition = "output.covid_dyplot_1 == null",
                      #sliderInput("breakCount", "Break Count", min = 1, max = 50, value = 10)
                      textOutput("map_instructions")
                    ),
                    tags$div(id="dy_div_1", style="height:125px;width:auto;"),
                    dygraphOutput("covid_dyplot_1"),
                    footer=tags$a(href="https://covidtracking.com/", "Source data: COVID Tracking Project"),
                    
                    collapsible=FALSE
                    
                ),
                
                box(width=6,
                    height=800,
                    
                    # box title
                    title = "State Comparison", 
                    #title=textOutput("dygraph_box_title_js_2"),
                    status = "success",
                    
                    selectInput(inputId="select_second_state", 
                                label="Compare Second State", 
                                choices=state_names, 
                                selected='California', 
                                multiple = FALSE,
                                selectize = TRUE, 
                                width = NULL, 
                                size = NULL),
                    
                    tags$div(id="dy_div_2", style="height:150px;width:auto;"),
                    dygraphOutput("covid_dyplot_2"),
                    footer=tags$a(href="https://covidtracking.com/", "Source data: COVID Tracking Project"),
                    
                    collapsible=FALSE
                    
                )
              )
      ),
      
      ###################################################
      # ui/covid-map-js/end
      ###################################################
      
      ###################################################
      # ui/covid-map-r/start
      ###################################################
      tabItem(tabName="covid-map-r",
              
              fluidRow(
                
                column(width = 12, 
                       offset = 0.5,
                       # Select-a-metric Drop-down menu
                       selectInput(inputId="metric_select",
                                   label="Select Metric",
                                   choices=metrics_options,
                                   selected='positiveIncrease',
                                   multiple=FALSE)
                )
                
              ),
              
              fluidRow(
                
                box(width=6,
                    
                    # box title
                    #title = "COVID-19 Map", 
                    title=textOutput("map_box_title_r"),
                    
                    status = "success",
                    
                    
                    # load text and table
                    tags$div(
                      #tags$p(verbatimTextOutput('someclicktext1')),
                      #dataTableOutput("table") 
                      
                      # Leaflet Shiny Map
                      leafletOutput("leaflet_shiny")
                      
                    ),
                    
                    footer=tags$a(href="https://covidtracking.com/api", "Source data: COVID Tracking Project"),
                    collapsible=FALSE
   
                ),
                box(width=6,
                    
                    # box title
                    title = "Select a State from the Map", status = "success",
                    
                    # load text and table
                    plotlyOutput('ggplot_hospital'),
                    
                    collapsible=FALSE
                    
                )
              )
      ),
      
      ###################################################
      # ui/covid-map-r/end
      ###################################################
      
      ###################################################
      # ui/state-trends/start
      ###################################################
      tabItem(tabName="state-trends",
              fluidRow(
                box(width=12,
                    title = "State Trends (Interactive)", status = "success",
                    #"Some comments", br(), "Some more comments",
                    selectInput(inputId="select_metric_interactive", 
                                label="Select Metric", 
                                choices=state_metrics, 
                                selected='New Positive Cases', 
                                multiple = FALSE,
                                selectize = TRUE, 
                                width = NULL, 
                                size = NULL),

                    plotlyOutput("state_trends_facet_plotly"),
                    
                    footer=tags$a(href="https://covidtracking.com/api", "Source data: COVID Tracking Project"),
                    collapsible=TRUE
                )
              ),
              fluidRow(
                box(width=12,
                    title = "State Trends (Static)", status = "success",
                    #"Some comments", br(), "Some more comments",
                    selectInput(inputId="select_metric_static", 
                                label="Select Metric", 
                                choices=state_metrics, 
                                selected='New Positive Cases', 
                                multiple = FALSE,
                                selectize = TRUE, 
                                width = NULL, 
                                size = NULL),

                    plotOutput("state_trends_facet_plot"),
                    footer=tags$a(href="https://covidtracking.com/api", "Source data: COVID Tracking Project"),
                    collapsible=TRUE
                )
              )
              
      )
      ###################################################
      # ui/state-trends/end
      ###################################################
      
    )
    
    
  )
  
)



###################################################
# Server function
###################################################
server <- function(input, output, session) {
  
  #############################################################################
  # Message Menu
  #############################################################################
  output$messagesMenu <- renderMenu({

    dropdownMenu(type = "notifications",
                 notificationItem(
                   text = "Added interactive map",
                   icon("globe")
                 )
    )
    
  })
  
  #############################################################################
  # Heading
  #############################################################################
  data_date <- max(states_current$date)
  output$header_text <- renderText(paste0("COVID-19 Data Tracker (last updated ", data_date, ")"))
  
  ###################################################
  # server/covid-map-js/valuebox/start
  ###################################################
  
  # Total Cases US
  total_cases <- sum(states_current$positive)
  total_cases <- prettyNum(total_cases, big.mark = ",")

  output$firstBox <- renderValueBox({
    
      valueBox(
      paste0(total_cases), "Posiive Cases", icon = icon("head-side-mask"),
      color = "maroon"
    )
  })
  

  
  # Biggest Increase
  biggest_daily_increase <- max(states_current$positiveIncrease)
  increase_name <- states_current[states_current$positiveIncrease==biggest_daily_increase,]$name
  biggest_daily_increase <- prettyNum(biggest_daily_increase, big.mark = ",")
  
  output$secondBox <- renderValueBox({
    valueBox(
      paste0(biggest_daily_increase), paste0("Biggest Daily Increase is in ", increase_name), icon = icon("dumpster-fire"),
      color = "yellow"
    )
  })

  # Total Deaths US
  total_deaths <- sum(states_current$death)
  total_deaths <- prettyNum(total_deaths, big.mark = ",")
  
  output$thirdBox <- renderValueBox({
    valueBox(
      paste0(total_deaths), "Total Deaths", icon = icon("cross"),
      color = "purple"
    )
  })
  

  ###################################################
  # server/covid-map-js/valuebox/end
  ###################################################
  
  ###################################################
  # server/covid-map-js/start
  ###################################################
  
  # Map instructions (dyplot is hidden until a polygon is selected)
  output$map_instructions <- renderText("Select a state on the map to render a plot")
  
  # Second state instructions (dyplot is hidden until a polygon is selected)
  #output$second_state_instructions <- renderText("Select a second state from the dropdown to compare")
  
  # Map Box Title js
  output$map_box_title_js <- renderText(paste0("COVID-19 Observations as of ", max(states_geo@data$date, na.rm=TRUE)))
  # Can use a static geojson if I cant dynamically convert the sp polygons to json without geojsonio
  #output$map_box_title_js <- renderText(paste0("COVID-19 Observations (Choropleth last updated 2020-07-02"))
  
  # Dygraph Box Title js
  output$dygraph_box_title_js_1 <- renderText(paste0("COVID-19 Observations (last updated ", max(states_current$date, na.rm=TRUE), ")"))
  
  # this function is for forming the message string to JS 
  #myf <- function(x){sprintf("[\"%s\", \"%s\" ,\"%s\" ,%s ,%s]", as.character(x[1]), x[2], x[3], x[4], x[5])}
  
  # create the message string 
  #station_data <- apply(X=states_geojson, MARGIN=1, FUN=myf)
  #geo_message <- paste0('[',toString(states_geojson),']')
  
  #geo_message <- states_geojson_message
  
  # Send the geojson data that we load from source_covid_data.r to the js file
  #session$sendCustomMessage(type="get_states_geojson_from_shiny", geo_message)
  
  # Process the mouse click
  # Get the leaflet click data
  observeEvent(input$clicked,
               {
                 # Retrieve the clicked id from the js
                 clicked_name <<- isolate(input$clicked[1])
                 
                 # find the row in the data frame that matches this name
                 #rowdata <- states_daily[states_daily$name==clicked_name, ]
                 
                 # make this a global variable so we can refer to is outside of this function
                 #tabledata2 <- data.frame(station_list[station_list$station_id==clicked_ID, ])
                 
                 # return print for the verbatimTextOutput
                 #output$someclicktext <- renderText({clicked_name})
                 #output$someclicktext <- renderPrint({tabledata2['station_id']})
                 #output$someclicktext <- renderPrint(rowdata)
                
                 output$covid_dyplot_1 <- renderDygraph({
                   
                   # Am I getting the clicked_ID here?
                   #output$someclicktext2 <- renderText({clicked_ID})
                   
                   # Pull data based on the user selection
                   states_daily_df <- states_daily[states_daily$name==clicked_name, ]

                   # Convert to xts object
                   #states_daily_xts <- xts(states_daily_df, order.by = states_daily_df$date) # works but plots everything
                   
                   # calculate days since first infection
                   days_since_first_infection <- as.numeric(Sys.Date() - min(states_daily_df$date[states_daily_df$positive>0], na.rm=TRUE))
                   
                   # convert each of the series to xts objects 
                   p_ts <- xts(states_daily_df$positiveIncrease, order.by=states_daily_df$date)
                   h_ts <- xts(states_daily_df$hospitalizedCurrently, order.by=states_daily_df$date)
                   i_ts <- xts(states_daily_df$inIcuCurrently, order.by=states_daily_df$date)
                   d_ts <- xts(states_daily_df$death, order.by=states_daily_df$date)
                   
                   # interpolate NAs in preparation for calculating SMAs (na.approx or na.spline)
                   p_ts_i <- na.approx(p_ts)
                   
                   # calculate 7-day SMA
                   p_ts_sma <- SMA(x=p_ts_i, n=7)
                   
                   # combine them 
                   covid_ts <- cbind(p_ts, p_ts_sma, h_ts, i_ts, d_ts)
                   
                   
                   # Subset the XTS object based on user timeseries selection
                   covid_ts <- if (input$select_ts=='ts_30d') { 
                     covid_ts[paste0(Sys.Date()-30, '/'),]
                   } else if (input$select_ts=='ts_60d') {
                     covid_ts[paste0(Sys.Date()-60, '/'),]
                   } else if  (input$select_ts=='ts_fi') {
                     covid_ts[paste0(Sys.Date()-days_since_first_infection, '/'),]
                   } else {
                     covid_ts
                   }
                   
                   # Create the dygraph
                   dygraph(covid_ts, main=paste0("COVID-19 data for ", clicked_name, " as of ", max(states_daily_df$date)), height=600) %>%
                     dySeries("p_ts", label = "New Positive Cases") %>%
                     dySeries("SMA", label = "New Positive Cases (7-day SMA)") %>%
                     dySeries("h_ts", label = "Hospitalized (Current)") %>%
                     dySeries("i_ts", label = "In ICU (Current)") %>%
                     dySeries("d_ts", label = "Total Deaths (Cumulative)") %>%
                     # Weird... school server needed me to do this:
                     # dySeries("..1", label = "New Positive Cases") %>%
                     # dySeries("SMA", label = "New Positive Cases (7-day SMA)") %>%
                     # dySeries("..3", label = "Hospitalized (Current)") %>%
                     # dySeries("..4", label = "In ICU (Current)") %>%
                     # dySeries("..5", label = "Total Deaths (Cumulative)") %>%
                     #dyAxis("x", drawGrid = input$gridlines_checkbox) %>%
                     dyAxis("y", label = "Measurement")  %>%
                     dyOptions(drawGrid = input$gridlines_checkbox) %>%
                     dyRangeSelector(height = 100) %>%
                     dyLegend(width = 600, labelsDiv="dy_div_1", labelsSeparateLines=TRUE)
                   
                 })
                 
                 output$covid_dyplot_2 <- renderDygraph({
                   
                   # Am I getting the clicked_ID here?
                   #output$someclicktext2 <- renderText({clicked_ID})
                   
                   # Pull data based on the user selection
                   states_daily_df <- states_daily[states_daily$name==input$select_second_state, ]
                   
                   # Convert to xts object
                   #states_daily_xts <- xts(states_daily_df, order.by = states_daily_df$date) # works but plots everything
                   
                   # calculate days since first infection
                   days_since_first_infection <- as.numeric(Sys.Date() - min(states_daily_df$date[states_daily_df$positive>0], na.rm=TRUE))
                   
                   # convert each of the series to xts objects 
                   p_ts <- xts(states_daily_df$positiveIncrease, order.by=states_daily_df$date)
                   h_ts <- xts(states_daily_df$hospitalizedCurrently, order.by=states_daily_df$date)
                   i_ts <- xts(states_daily_df$inIcuCurrently, order.by=states_daily_df$date)
                   d_ts <- xts(states_daily_df$death, order.by=states_daily_df$date)
                   
                   # interpolate NAs in preparation for calculating SMAs (na.approx or na.spline)
                   p_ts_i <- na.approx(p_ts)
                   
                   # calculate 7-day SMA
                   p_ts_sma <- SMA(x=p_ts_i, n=7)
                   
                   # combine them 
                   covid_ts <- cbind(p_ts, p_ts_sma, h_ts, i_ts, d_ts)
                   
                   
                   # Subset the XTS object based on user timeseries selection
                   covid_ts <- if (input$select_ts=='ts_30d') { 
                     covid_ts[paste0(Sys.Date()-30, '/'),]
                   } else if (input$select_ts=='ts_60d') {
                     covid_ts[paste0(Sys.Date()-60, '/'),]
                   } else if  (input$select_ts=='ts_fi') {
                     covid_ts[paste0(Sys.Date()-days_since_first_infection, '/'),]
                   } else {
                     covid_ts
                   }
                   
                   # Create the dygraph
                   dygraph(covid_ts, main=paste0("COVID-19 data for ", input$select_second_state, " as of ", max(states_daily_df$date)), height=600) %>%
                     dySeries("p_ts", label = "New Positive Cases") %>%
                     dySeries("SMA", label = "New Positive Cases (7-day SMA)") %>%
                     dySeries("h_ts", label = "Hospitalized (Current)") %>%
                     dySeries("i_ts", label = "In ICU (Current)") %>%
                     dySeries("d_ts", label = "Total Deaths (Cumulative)") %>%
                     # Weird... school server needed me to do this:
                     # dySeries("..1", label = "New Positive Cases") %>%
                     # dySeries("SMA", label = "New Positive Cases (7-day SMA)") %>%
                     # dySeries("..3", label = "Hospitalized (Current)") %>%
                     # dySeries("..4", label = "In ICU (Current)") %>%
                     # dySeries("..5", label = "Total Deaths (Cumulative)") %>%
                     #dyAxis("x", drawGrid = input$gridlines_checkbox) %>%
                     dyAxis("y", label = "Measurement")  %>%
                     dyOptions(drawGrid = input$gridlines_checkbox) %>%
                     dyRangeSelector(height = 100) %>%
                     dyLegend(width = 600, labelsDiv="dy_div_2", labelsSeparateLines=TRUE)
                   
                 })
                 
               })

  ###################################################
  # server/covid-map-js/end
  ###################################################
  
  ###################################################
  # server/covid-map-r/start
  ###################################################
  
  # Map Box Title R
  output$map_box_title_r <- renderText(paste0("COVID-19 Observations (last updated ", max(states_geo@data$date, na.rm=TRUE), ")"))
  
  
  # Initialize reactive values
  reac <- reactiveValues(state="Virginia", metric='positiveIncrease')
  #reac$metric <<- input$metric_select
  
  # Make reac dependent on the input and map shape click
  observe ({ 
    reac$state <- input$leaflet_shiny_shape_click$id
    reac$metric <- input$metric_select
    
    output$leaflet_shiny <- renderLeaflet({
      
      # Melt the data
      states_geo_melt <- data.table::copy(states_geo_r)
      states_geo_melt@data <- melt(data.table(states_geo_r@data), id.vars=c('GEO_ID', 'STATE', 'NAME', 'date', 'state'))
      
      # Subset the data based on the input
      states_geo_melt@data <- subset(states_geo_melt@data, variable==reac$metric)
      #states_geo_melt@data <- subset(states_geo_melt@data, variable=='positiveIncrease')
      
      # Make this numeric
      states_geo_melt@data$value <- as.numeric(states_geo_melt@data$value)
      
      # create palette for metric
      metric <- reac$metric
      #metric <-'positiveIncrease'
      quantile(states_geo_melt@data$value, na.rm=TRUE)
      metric_bins <- quantile(states_geo_melt@data$value, na.rm=TRUE)
      metric_pal <- colorBin("YlOrRd", domain = states_geo_melt@data$value, bins=metric_bins)
      
      # normalize  values
      x <- states_geo_melt@data$value
      states_geo_melt@data$metric_norm = (x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
      
      # polygon labels
      labels <- sprintf(
        "<strong>%s</strong><br/>%s: %g",
        #states_geo@data$NAME, states_geo@data$positiveIncrease
        states_geo_melt@data$NAME, states_geo_melt@data$variable, states_geo_melt@data$value
      ) %>% lapply(htmltools::HTML)
      
      
      # create leaflet map!
      shiny_map <- leaflet(states_geo_melt) %>%
        
        setView(lng=-95.7, lat=37.1, zoom=3) %>%
        
        # Base groups
        addTiles(group = "default") %>%
        
        addProviderTiles(providers$Stamen.TonerLite, 
                         options=providerTileOptions(opacity = 0.5), 
                         group = "Toner Lite") %>%
        
        # Overlay groups
        addPolygons(group = "States",
                    fillColor=~metric_pal(value),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#667",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = labels,
                    layerId=~NAME,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        
        # note: addLegent might conflict with dygraph library (had error about xts)
        leaflet::addLegend(pal = metric_pal, 
                           values = ~value, 
                           opacity = 0.7, 
                           title = 'Increase in Positive Cases (Yesterday)',
                           position = "bottomright") %>%
        
        # Layers control
        addLayersControl(
          baseGroups = c("Default", "Toner Lite"),
          overlayGroups = c("States"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
      shiny_map
      
    })
    
    observeEvent(input$leaflet_shiny_shape_click, { # Look at the layerId field in the leaflet map
      click_data <- input$leaflet_shiny_shape_click
      print(click_data$id)
      #reac$state <- click_data$id
      #reac$metric <- input$metric_select
      
      # Melt the data
      states_daily_melt <- melt(data.table(states_daily), id.vars=c('date', 'state', 'name', 'fips.x', 'fips.y', 'pui', 'pum'))
      
      # Subset the data based on the input
      states_daily_melt_data <- subset(states_daily_melt, variable==reac$metric)
      #states_daily_melt_data <- subset(states_daily_melt, variable==input$metric_select)
      #states_daily_melt_data <- subset(states_daily_melt, variable==metric)
      
      # Generate ggplot elements using states_daily
      output$ggplot_hospital <- renderPlotly({
        
        p <- ggplot(data=subset(states_daily_melt_data, name==reac$state), aes(x=date, y=as.numeric(value), group=reac$state)) +
          geom_line(data=states_daily_melt_data, aes(x=date, y=as.numeric(value), group=name), color="lightgrey", size=1) +
          geom_line(position='identity', size=2) +
          ylab(reac$metric) + 
          xlab("") +
          ggtitle(paste0("Highlight: ", reac$state, " (", reac$metric, ")"))
        
        p
        #ggplotly(p)
      })
      
    })
    
  })
  
  ###################################################
  # server/covid-map-r/end
  ###################################################
  
  ###################################################
  # server/state-trends/start
  ###################################################
  
  #output$state_trends_facet_plot <- renderPlotly({
  output$state_trends_facet_plot <- renderPlot({
    state_trends_plot <- ggplot(states_daily, aes(x=date, y=positiveIncrease)) +
      geom_line() +
      facet_geo(~state, grid="us_state_grid2") +
      #facet_geo(~state, grid="us_state_grid2", scales="free_y") +
      scale_x_date() +
      theme_minimal() +
      ylab("Positive COVID Cases")

    state_trends_plot

  })

  output$state_trends_facet_plotly <- renderPlotly({
  #output$state_trends_facet_plot <- renderPlot({
    state_trends_plot <- ggplot(states_daily, aes(x=date, y=positiveIncrease)) +
      geom_line() +
      facet_geo(~state, grid="us_state_grid2") +
      #facet_geo(~state, grid="us_state_grid2", scales="free_y") +
      scale_x_date() +
      theme_minimal() +
      ylab("Positive COVID Cases")

    state_trends_plot

  })
  
  ###################################################
  # server/state-trends/end
  ###################################################
  
}
###################################################
# Call function
###################################################
shinyApp(ui, server)
###################################################################
# Data Visualization Final Project: NOAA Buoy Station EDA
# Author: Kevin LT Chan
# Version 0.1
# Updates:
# - Webscrapping Code completed
# - Leafet Map with Buoy completed
# - Buoy Info tabs completed
# - EDA Currently working (missing: 1 tab and headers + legend)
#
# To do: 
# - Write About 
# - Create World spin gif (NASA temperature change dataset)
# - Write Methodology 
# - Write function to auto fill in timeseries
# - Conduct timeseries decomposition
# - Forecasting tab
###################################################################
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)
library(leaflet)
library(tidyverse)
###################################################################

dashboardPagePlus(
  skin = "black",
  
  header = dashboardHeaderPlus(
    # Set height of dashboardHeader
    # tags$li(class = "dropdown",
    #         tags$style(".main-header {max-height: 38px}"),
    #         tags$style(".main-header .logo {height: 38px;}"),
    #         tags$style(".sidebar-toggle {height: 38px; padding-top: 1px !important;}"),
    #         tags$style(".navbar {min-height:38px !important}")),
    title = tagList(
      span(class = "logo-lg", "Data Visualization Project"), 
      img(src = "life.PNG"))

  ),
    sidebar = dashboardSidebar(
      # Adjust the sidebar
      # tags$style(".left-side, .main-sidebar {padding-top: 40px}"),
      sidebarMenu(
        menuItem("About", tabName = "about", icon = icon("info")),
        menuItem("Methodology",tabName = "method", icon = icon("info")),
        menuItem("Buoy Stations", tabName = "buoy", icon = icon("database"))
    )
    ),
    
    body = dashboardBody(
      tabItems(
        tabItem(tabName = "about",
                fluidRow(
                  box(
                    title = "About",
                    solidHeader = TRUE,
                    status = "primary",
                    width = 12,
                    collapsible = TRUE,
                    "text and more text"
                  ),
                  box(
                    title = "Team",
                    solidHeader = FALSE,
                    status = "warning",
                    collapsible = TRUE,
                    width = 12,
                    socialBox(
                      title = "Kevin LT Chan",
                      subtitle = "BSBA (BU), MSBARM(JHU)",
                      src = 'kevin.JPG',
                      width = 6,
                      closable = FALSE,
                      color = "aqua-active",
                      "Graduated with a B.S.B.A. degree (double-major in finance and accounting) at the Questrom School of Business, Boston University. He is currently pursuing a master’s degree in business analytics and risk management at Johns Hopkins University. Kevin is a serious table tennis player and has served as the captain of the Boston University Team."
                    ),
                    socialBox(
                      title = "name",
                      subtitle = "nanana",
                      src = 'img1.JPG',
                      width = 6,
                      closable = FALSE,
                      color = "aqua-active",
                      "text"
                    ),
                    socialBox(
                      title = "name",
                      subtitle = "nanana",
                      src = 'img1.JPG',
                      width = 6,
                      closable = FALSE,
                      color = "aqua-active",
                      "text"
                    ),
                    socialBox(
                      title = "name",
                      subtitle = "nanana",
                      src = 'img1.JPG',
                      width = 6,
                      closable = FALSE,
                      color = "aqua-active",
                      "text"
                    )
                    
                  )
                )
          
        ),
        tabItem(tabName = "method",
                fluidRow(
                  box(
                    width = 12
                  )
                )
        
        ),
        tabItem(tabName = "buoy",
                fluidRow(
                  box(
                    width = 12,
                    collapsible = TRUE,
                    leafletOutput("buoy_map")
                    
                  )
                  
                ),
                fluidRow(
                  box(
                    width = 6,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    status = "warning",
                    valueBoxOutput("station_num"),
                    valueBoxOutput("Owner"),
                    valueBoxOutput("Type"),
                    dataTableOutput("raw")
                    # verbatimTextOutput("missing")

                  ),
                  box(
                    width = 6,
                    status = "warning",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotOutput("historical")
                   
                  ),
                  br(),
                  box(
                    width = 12,
                    dateRangeInput('dateRange',
                                   label = paste("Date range input"),
                                   start = Sys.Date() - 3, end = Sys.Date() + 3,
                                   min = Sys.Date() - 10, max = Sys.Date() + 10,
                                   separator = " - ", format = "dd MM yyyy",
                                   startview = 'decade', language = 'ENG', weekstart = 1
                    )
                  ),
                  
                  tabBox(
                    title = "EDA",
                    id = "buoy_analysis",
                    height = "500px",
                    width = 12,
                    tabPanel("Historical",
                             dropdown(
                               pickerInput(inputId = "Historical_variable",
                                           label = "Variable",
                                           choices = list(
                                                          Temperature = list("Air Temperature"="Air_temp","Sea Temperature"="Sea_temp", "Air Sea Temperature Difference" = "Air_sea_temp"),
                                                          "Wind Speed" = list("Wind Speed"="Wind_speed")),
                                           choicesOpt = list(
                                                             content = c("<div style ='color:#F8766D'>Air Temperature</div>",
                                                                         "<div style ='color:#00BFC4'>Sea Temperature</div>",
                                                                         "<div style ='color:#C77CFF'>Air Sea Temperature Difference</div>",
                                                                         "<div style ='color:#7CAE00'>Wind Speed</div>")
                                           ),
                                           multiple = TRUE
                                           ),
                               circle = TRUE, icon = icon("gear"),
                               status = "danger", width = "300px",
                               tooltip = tooltipOptions(title = "Options")
                             ),
                             plotOutput("historical_eda")
                             ),
                    tabPanel("Month",
                             dropdown(
                               pickerInput(inputId = "Month_variable",
                                           label = "Chart",
                                           choices = list(
                                             Temperature = list("Air Temperature"="Air_temp","Sea Temperature"="Sea_temp", "Air Sea Temperature Difference" = "Air_sea_temp"),
                                             "Wind Speed" = list("Wind Speed"="Wind_speed")),
                                           choicesOpt = list(
                                             content = c("<div style ='color:#F8766D'>Air Temperature</div>",
                                                         "<div style ='color:#00BFC4'>Sea Temperature</div>",
                                                         "<div style ='color:#C77CFF'>Air Sea Temperature Difference</div>",
                                                         "<div style ='color:#7CAE00'>Wind Speed</div>")
                                           ),
                                           multiple = FALSE
                               ),
                               circle = TRUE, icon = icon("gear"),
                               status = "danger", width = "300px",
                               tooltip = tooltipOptions(title = "Options")
                             ),
                             plotOutput("month_eda")
                             ),
                    tabPanel("Trend")
                    
                  )
                )
                    
        )
    )
      
        
    ),
    footer = dashboardFooter()
)
  
  
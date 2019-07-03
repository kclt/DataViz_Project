###################################################################
# Data Visualization Final Project: NOAA Buoy Station EDA
# Author: Kevin LT Chan
# Version 0.3
# Updates:
# - Webscrapping Code completed
# - Buoy Station tab complete
# - 
#
# To do: 
# - Write About 
# - Create World spin gif (NASA temperature change dataset)[Done]
# - Write Methodology 
# - Conduct timeseries decomposition & stuff [Done]
# - Create animation tab
# - Create stat tab 
# - Forecasting tab [Done]
# - Collect Full Data
###################################################################
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)
library(leaflet)
library(tidyverse)
library(ggseas)
library(plotly)
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
      span(class = "logo-lg", "NOAA - Buoy Station Visualization"), 
      img(src = "life.PNG"))

  ),
    sidebar = dashboardSidebar(
      # Adjust the sidebar
      # tags$style(".left-side, .main-sidebar {padding-top: 40px}"),
      sidebarMenu(
        menuItem("About", tabName = "about", icon = icon("info")),
        menuItem("Methodology",tabName = "method", icon = icon("info")),
        menuItem("Buoy Stations", tabName = "buoy", icon = icon("database")),
        menuItem("Statistics", tabName = "stat", icon = icon("info")),
        menuItem("Timeline",tabName = "animation", icon = icon("info"))
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
                    h4("Motivation"),
                    "from the surface temperature blabalbalba -> increase in temperature -> global warming",
                    column(6,imageOutput("gif")),# insert the gif of the world spinning and temperature
                    column(6,"some text"),# insert the picture... 
                    tags$code("bbaldfbalsdbflabsdlf"),
                    h4("about this project"),
                    "Looks at buoy around the world and see whether sea and air temperature in different region has increase over the past decade years. Also look at the whether the difference between air and sea temperature has increase or not. "
                    
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
                      title = "Jason Zhang",
                      subtitle = "nanana",
                      src = 'img1.JPG',
                      width = 6,
                      closable = FALSE,
                      color = "aqua-active",
                      "text"
                    ),
                    socialBox(
                      title = "Chris Qiu",
                      subtitle = "nanana",
                      src = 'img1.JPG',
                      width = 6,
                      closable = FALSE,
                      color = "aqua-active",
                      "text"
                    ),
                    socialBox(
                      title = "Sutao Stephen Xie",
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
        tabItem(tabName = "stat",
                fluidRow(
                  box(
                    width = 12
                  )
                )),
        tabItem(tabName = "buoy",
                fluidRow(
                  box(
                    title = "Please Select a Buoy Station",
                    solidHeader = TRUE,
                    width = 4,
                    #collapsible = TRUE,
                    leafletOutput("buoy_map", height = 430)
                    
                  ),
                  tabBox(
                    title = "EDA",
                    id = "buoy_analysis",
                    height = "450px",
                    width = 8,
                    tabPanel("Overview",
                             plotlyOutput("overview")),
                    tabPanel("Data",
                             dataTableOutput("raw")
                    ),
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
                                           multiple = TRUE,
                                           selected = "Air_temp"
                               ),
                               circle = TRUE, icon = icon("gear"),
                               status = "danger", width = "300px",
                               tooltip = tooltipOptions(title = "Options")
                             ),
                             plotlyOutput("historical_eda")
                             
                    ),
                    tabPanel("Month",
                             #dropdown(
                               pickerInput(inputId = "Month_variable",
                                           #label = "Chart",
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
                               #circle = TRUE, icon = icon("gear"),
                               #status = "danger", width = "300px",
                               #tooltip = tooltipOptions(title = "Options")
                             #),
                             plotlyOutput("month_eda")
                    ),
                    tabPanel("Trend",
                            dropdown(
                              pickerInput(inputId = "Trend_variable",
                                          label = "Chart",
                                          choices = list(
                                            Area = list("Air Temperature"="Air_temp_f","Sea Temperature"="Sea_temp_f")),
                                          selected = "Air_temp_f"
                                          ),
                              circle = TRUE, icon = icon("gear"),
                              status = "danger", width = "300px",
                              tooltip = tooltipOptions(title = "Options")
                              ),
                            plotlyOutput("trend_eda")
                            )
                    
                  )
                  
                ),
                fluidRow(
                  box(
                    width = 12,
                    status = "warning",
                    dateRangeInput('dateRange',
                                   label = paste("Date range input"),
                                   start = Sys.Date() - 3, end = Sys.Date() + 3,
                                   min = Sys.Date() - 10, max = Sys.Date() + 10,
                                   separator = " - ", format = "dd MM yyyy",
                                   startview = 'decade', language = 'ENG', weekstart = 1
                    )
                  )
                  
                  
                )
                    
        ),
        tabItem(tabName = "animation",                
                fluidRow(
                  box(
                    width = 6
                  ),
                  box(
                    width = 6
                  ),
                  box(
                    width = 12,
                    sliderInput("yearSlider", "Year:", 
                                min = 1985, max = 2018, value = 1985, step = 1,
                                sep = "",
                                animate = animationOptions(interval = 700, loop = TRUE))
                  )
                )
        )
      )  
        
    ),
    footer = dashboardFooter()
)
  
  
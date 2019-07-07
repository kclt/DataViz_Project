###################################################################
# Data Visualization Final Project: NOAA Buoy Station EDA
# Author: Kevin LT Chan
# Version 0.3
# Updates:
# - Webscrapping Code completed
# - Buoy Station tab complete
#
# To do: 
# - Write About 
# - Write Methodology [Done]
# - Create World spin gif (NASA temperature change dataset)[Done]
# - Conduct timeseries decomposition & stuff [Done]
# - Create animation tab [Done] -> need to add text
# - Create stat tab[incomplete - not going to do it]
# - Collect Full Data [Done]
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
      span(class = "logo-lg", "Climate Warming: A Visualization Project"), 
      img(src = "life.PNG"))

  ),
    sidebar = dashboardSidebar(
      # Adjust the sidebar
      # tags$style(".left-side, .main-sidebar {padding-top: 40px}"),
      sidebarMenu(
        menuItem("About", tabName = "about", icon = icon("info")),
        menuItem("Methodology",tabName = "method", icon = icon("cogs")),
        menuItem("Buoy Stations", tabName = "buoy", icon = icon("globe-americas")),
        #menuItem("Statistics", tabName = "stat", icon = icon("chart-area")),
        menuItem("Timeline",tabName = "animation", icon = icon("chart-area"))
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
                    "Looks at buoy around the world and see whether sea and air temperature in different region has increase over the past decade years. Also look at the whether the difference between air and sea temperature has increase or not. ",
                    h4("Data Set"),
                    "data used is NOAA bitches"
                    
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
                    width = 12,
                    h3(strong("Methodology"), align = "center"),
                    p("During the building of this project, the main methods used are:"),
                    h4("Data Collection & Processing:"),
                    tags$ul(
                      tags$li("Data Scrapping"),
                      tags$li("Web Scrapping"),
                      tags$li("Data Cleaning"),
                      tags$ul(
                        tags$li("Combining Files"),
                        tags$li("Date Time Format inconsistency"),
                        tags$li("Padding Missing Dates"),
                        tags$li("Time Series: Seasonally Decomposed Missing Value Imputation")
                        
                      )
                    ),
                    h4("Data Visualization:"),
                    tags$ul(
                      tags$li("Raw Data Line Chart"),
                      tags$li("Monthly Temperature Variation Box & Whiskers Chart"),
                      tags$li("Time Series Decomposition"),
                      tags$li("World Map"),
                      tags$li("Gif")
                    ),
                    hr(),
                    h4("Data Scrapping ",tags$a(href="https://github.com/kclt/DataViz_Project/blob/master/Scripts/noaa_txt_download.R","[View Code]")),
                    p("As historical standard meteorological data for buoy are stored in separate text file for each separate year, we wrote a script to collect all the data from NOAA website for each buoy for each year."),
                    imageOutput("data_scrap",height = 100),
                    h4("Web Scrapping ",tags$a(href="https://github.com/kclt/DataViz_Project/blob/master/Scripts/noaa_meta.R","[View Code]")),
                    p("We use webscrapping methods to collect basic information about each buoy station which include:"),
                    tags$ul(
                      tags$li("Coordinates"),
                      tags$li("Ownership of Buoy"),
                      tags$li("Description of Buoy"),
                      tags$li("Type of Buoy"),
                      tags$li("URL to Buoy homepage")
                    ),
                    imageOutput("web_scrap", height = 350),
                    h4("Data Cleaning ",tags$a(href="https://github.com/kclt/DataViz_Project/blob/master/Scripts/noaa_combine_file.R","[View Code]")),
                    p("In order to make some visualization for our project, we first have to process our raw data of buoy station that we downloaded. Thus, we first combine for each buoy station’s separate text file for each year and generate a single csv output file. However, taking into consideration that data are reported in an hourly fashion, to keep the files size sensible, we reduce the dimension of the data into daily aggregate. Furthermore, over the years the date time format of the files has change thus to combine them we have to standardize the inconsistency to allow for time series to make sense.  After combining all the files, we noticed that there are missing dates in between the available dates, thus we have to add those back in and also for better time series analysis remove February 29th (leap year)."),
                    p("As to conduct any time series analysis, it would require for a full unbroken time series. We would thus have to filling in the missing values. Normally we would be able to use conventional methods such as last observation carry forward, but since it is observe there are sustained period (> couple of months) of missing or unavailable data, we would have to use either forecast/imputation to fill in the missing values. Here from existing data we observe seasonal trend, we opted to use seasonal decomposed missing value imputation method (Removes the seasonal component from the time series, performs imputation on the deseasonalized series and afterwards adds the seasonal component again).  Finally, after all that we generate a csv file that we would be able to use for visualizations."),
                    hr(),
                    h4("Raw Data Line Chart",tags$a(href="https://github.com/kclt/DataViz_Project/blob/master/Scripts/noaa_rawplot.R","[View Code]")),
                    p("The line chart provides a overview of the avaliable data for a buoy station: From this chart we can observe seasonality trends, and also difference between sea temperature and air temperature of a region"),
                    imageOutput("overviews", height = "50%"),
                    h4("Monthly Temperature Variation Box & Whiskers Chart"),
                    p("To observe how temperature fluctuates over the 12 month period, we create a box & whiskers chart. Furthermore, we would be able to observe extreme values & quantiles of temperature of observe some trends"),
                    imageOutput("month", height = "10%"),
                    h4("Time Series Decomposition"),
                    p("Due to the effect of seaonality, it is hard to observe whether there is a positive upwards trend. Thus we conduct a time series decomposition, which allows us to seperate the time series into season and trend."),
                    imageOutput("decomposition", height = "10%"),
                    h4("World Map"),
                    p("The world map in this visualization project serves two purpose:"),
                    tags$ul(
                      tags$li("Plot buoy on map"),
                      tags$li("View how temperature change from 1985 in each part of the world")
                    ),
                    h4("Gif ",tags$a(href="https://github.com/kclt/DataViz_Project/blob/master/Scripts/world_map_gif","[View Code]")),
                    p("Used to illstrate how the world has been warming up quicker than average over the past decade and is included as part of the motivation of the project.")
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
                    leafletOutput("buoy_map", height = 450)
                    
                  ),
                  tabBox(
                    title = "EDA",
                    id = "buoy_analysis",
                    height = "450px",
                    width = 8,
                    tabPanel("Overview",
                             plotlyOutput("overview"),
                             imageOutput("legend",height = 40)),
                    tabPanel("Data",
                             dataTableOutput("raw")
                    ),
                    tabPanel("Historical",
                             # dropdown(
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
                               # ),
                               # circle = TRUE, icon = icon("gear"),
                               # status = "danger", width = "300px",
                               # tooltip = tooltipOptions(title = "Options")
                             ),
                             plotlyOutput("historical_eda")
                             
                             
                    ),
                    tabPanel("Month",
                             # dropdown(
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
                             # circle = TRUE, icon = icon("gear"),
                             # status = "danger", width = "300px",
                             # tooltip = tooltipOptions(title = "Options")
                             # ),
                             plotlyOutput("month_eda")
                    ),
                    tabPanel("Trend",
                            # dropdown(
                              pickerInput(inputId = "Trend_variable",
                                          label = "Chart",
                                          choices = list(
                                          Area = list("Air Temperature"="Air_temp_f","Sea Temperature"="Sea_temp_f")),
                                          selected = "Air_temp_f",
                                          choicesOpt = list(
                                          content = c("<div style ='color:#F8766D'>Air Temperature</div>",
                                                      "<div style ='color:#00BFC4'>Sea Temperature</div>"
                                          ))),
                              # circle = TRUE, icon = icon("gear"),
                              # status = "danger", width = "300px",
                              # tooltip = tooltipOptions(title = "Options")
                              # ),
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
                    width = 7,
                    plotOutput("map_animate_1")
                  ),
                  box(
                    width = 5,
                    height = 420,
                    plotlyOutput("anomaly", height = 200),
                    uiOutput("animate_info")
                  ),
                  box(
                    width = 12,
                    status = "warning",
                    sliderInput("yearSlider", "Year:", 
                                min = 1986, max = 2018, value = 1986, step = 1,
                                sep = "",
                                animate = animationOptions(interval = 3000, loop = FALSE))
                  )
                )
        )
      )  
        
    ),
    footer = dashboardFooter(left_text = "Kevin LT Chan", right_text = "lchan11@jhu.edu")
)
  
  
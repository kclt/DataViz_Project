###################################################################
# Data Visualization Final Project: NOAA Buoy Station EDA
# Author: Kevin LT Chan
# Version 1.0
# Updates:
# - Webscrapping/datascrapping [Done]
# - Buoy Station tab [Done]
# - Write Methodology [Done]
# - Create World spin gif (NASA temperature change dataset)[Done]
# - Conduct timeseries decomposition & stuff [Done]
# - Create animation tab [Done]
# - Write About [done]

#
# To do: 
# - Create stat tab[incomplete - not going to do it]
# - Collect Full Data [incomplete - too large/inconsistent formating between US and others]
###################################################################
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)
library(tidyverse)
library(leaflet)
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
      span(class = "logo-lg", "Climate Warming"), 
      img(src = "life.png"))

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
                    h3(strong("Motivation"),align = "center"),
                    p("Climate warming/Global warming is one of the most severe problems people are facing. Nowadays, we can see a variety of reports and papers talking about how severe climate warming is [An example shown by the gif below: Showing how temperature has changed since 1985] and how will it affect the world as well as our life. One of most impressive reports is that Polar Bear Really Are Starving Because of Global Warming."),
                    column(12, align = 'center', imageOutput("gif")),# insert the gif of the world spinning and temperature
                    h3(strong("Data Sets"),align = "center"),
                    p("Two datasets are used in this visualization project:"),
                    tags$ul(
                      tags$li(tags$a(href="https://www.ndbc.noaa.gov/","National Data Buoy Center")),
                      tags$li(tags$a(herf="https://data.giss.nasa.gov/gistemp/","Giss Surface Temperature Analysis"))
                    ),
                    p("The National Data Buoy Center reports historical standard meterorological data of buoy floating in the sea. These data are used to visualize air temperature, sea temperature and wind speed of different areas around the world."),
                    p("The other one dataset used is GISS Surface Temperature Analysis; a high level aggregated dataset compared to data collected by buoys, but allows us to create visualization of temperature of the globe and conduct trend analysis."),
                    h3(strong("About this project"), align = "center"),
                    p("This project is a small data visualization project of buoy stations and temperature change/anomaly around the globe. By exploring how each buoy stations, we hope to be able to gain insights regarding how air temperature and sea temperature has changed over time. On the other hand looking a aggregated data to view over trends of temperature of the world."),
                    br(),
                    p("The Buoy Station Tab: An exploration of each buoy station collected - here we mainly focus on America buoy stations. In order to begin first select a buoy station. Then in each different tabs you would be able to explore:"),
                    tags$ul(
                      tags$li("Overview: Provide a brief interactive graph of the whole dataset"),
                      tags$li("Data: Provide user the ability to look at the full dataset"),
                      tags$li("Historical: Allows user to explore each variable [with the additional Air to Sea temperature difference] or multiple variable at the same time over the time series"),
                      tags$li("Month: A box chart that allows user to explore how temperature change over the month"),
                      tags$li("Time Series: A missing data filled time series that is seperated into seasonality and trend to offer user the ability to look at trend of an area around a specific station")
                    ),
                    p("The Date Range input allows user to change the date range which the above tabs are based off."),
                    br(),
                    p("Timeline Tab: An exploration of global temperature change and anmoaly."),
                    p("The Year Slider allows you to view information respective to that selected year [1986 - 2018]"),
                    p("Left box shows the temperature change from year 1985 of the world, while the right box contains 3 tabs:"),
                    tags$ul(
                      tags$li("Anomaly by Year: A bar chart that compares the anomaly (defined as temperature difference from the last decade average) by each region"),
                      tags$li("Anomaly Trend: A line chart that shows the anomaly trend till year selected"),
                      tags$li("Information: A info box for some trends observed by NOAA - mainly populated for year 2000 and after.")
                    )
                                     
                  ),
                    socialBox(
                      title = "Kevin LT Chan",
                      subtitle = "BSBA (BU), MSBARM(JHU), MPhil(Judge)",
                      src = 'kevin.jpg',
                      width = 12,
                      collapsible = FALSE,
                      closable = FALSE,
                      color = "aqua-active",
                      ""
                      )
                )
          
        ),
        tabItem(tabName = "method",
                fluidRow(
                  box(
                    width = 12,
                    h2(strong("Methodology"), align = "center"),
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
                      tags$li("Line Chart"),
                      tags$li("Box & Whiskers Chart"),
                      tags$li("Time Series Decomposition"),
                      tags$li("Bar Chart"),
                      tags$li("World Map"),
                      tags$li("Gif")
                    ),
                    hr(),
                    h3(strong("Data Collection & Processing"), align = "center"),
                    h4("Data Scrapping ",tags$a(href="https://github.com/kclt/DataViz_Project/blob/master/Scripts/noaa_txt_download.R","[View Code]")),
                    p("As historical standard meteorological data for buoy are stored in separate text file for each separate year, we wrote a script to collect all the data from NOAA website for each buoy for each year."),
                    imageOutput("data_scrap",height = 100),
                    p("For GISS Surface Temperature dataset, data are manually downloaded as DDOSS protection are implemented that doesn't allow for webscrapping."),
                    p("information respective to each year was scrapped from NOAA global climate report or googled"),
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
                    h3(strong("Data Visualizations"), align = "center"),
                    h4("Line Chart",tags$a(href="https://github.com/kclt/DataViz_Project/blob/master/Scripts/noaa_rawplot.R","[View Code]")),
                    p("The line chart provides a overview of the avaliable data for a buoy station: From this chart we can observe seasonality trends,general trends over a time series. A line chart provides the best context for a time series, as compared to bar charts as it links time together to allows us better understand trends over time. Line chart also allows us to more easily compare between variables, for example comparing the difference between air temperature and sea temperature."),
                    imageOutput("overviews", height = "50%"),
                    h4("Box & Whiskers Chart"),
                    p("To observe how temperature fluctuates over the 12 month period, we create a box & whiskers chart. Furthermore, we would be able to observe extreme values & quantiles of temperature of observe some trends"),
                    imageOutput("month", height = "10%"),
                    h4("Time Series Decomposition"),
                    p("Due to the effect of seaonality, it is hard to observe whether there is a positive upwards trend. As the upward trend view from an overview maybe caused by seasonality, thus we conduct a time series decomposition, which allows us to seperate the time series into season, trend and residual errors. Then from the trend line, we would be able to observe whether an region has shown an upward increasing trend over the specified time period or not."),
                    imageOutput("decomposition", height = "10%"),
                    h4("Bar Chart"),
                    p("Bar Charts are used in this visualization project to compare between different categories (in this case regions). Comparing the temperature anomalies between different regions around at a specific year"),
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
                    width = 6,
                    height = 370,
                    plotOutput("map_animate_1", height = 350)
                  ),
                  box(
                    id = "animation_info",
                    title = NULL,
                    width = 6,
                    height = 370,
                      plotlyOutput("anomaly_year", height = 350)
                      
                  )),
                fluidRow(
                  box(
                    width = 6,
                    height = 370,
                    plotlyOutput("anomaly", height = 350)
                  ),
                  box(
                    width = 6,
                    height = 370,
                    uiOutput("animate_info", height = 350)
                  ),
                  absolutePanel(
                    bottom = 20, right = 20, width = 300,
                    draggable = TRUE,
                    sliderInput("yearSlider", NULL, 
                                min = 1986, max = 2018, value = 1986, step = 1,
                                sep = "",
                                animate = animationOptions(interval = 3000, loop = FALSE))
                ))



        )
      )  
        
    ),
    footer = dashboardFooter(left_text = "Kevin LT Chan", right_text = "kevinchanloktin@gmail.com")
)
  
  
###################################################################
# Data Visualization Final Project: NOAA Buoy Station EDA
# Author: Kevin LT Chan
# Version 0.1
# See ui.R for updates and todo list
###################################################################
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)
library(tidyverse)
library(grid)
library(gridExtra)
library(lubridate)
source("./Scripts/noaa_rawplot.R")
###################################################################

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {

  marker_data <- read.csv("./Output/Meta/buoy_meta.csv")
  missing_files <- read.csv("./Error/Missing_files.csv")
    
  output$buoy_map <- renderLeaflet({
    
    
    m <- leaflet(data = marker_data) %>% 
         addTiles() %>% 
         addMarkers(popup = paste("<center><b><a href='",marker_data$URL,"'>",marker_data$Buoy,"</a></b></center>",
                                  "<br/>(",marker_data$Latitude," N , ",marker_data$Longitude," W)"))
    
    m
    
  })
  
  observe({
    
    

    click <- input$buoy_map_marker_click
      if(is.null(click)){
        return()
    }
  
    
    
    buoy_station <- marker_data[which(marker_data$Longitude == click$lng  & marker_data$Latitude == click$lat),1]
    
    output$historical <- renderPlot(
      noaa_rawplot(buoy_station)
    )
    
    output$raw <- renderDataTable(
      read_csv(paste0("./Output/Raw/",buoy_station,".csv"))[,1:4],
      options = list(pageLength = 5,
                     lengthMenu = c(5,10,15),
                     scrollX = FALSE,
                     searchHighlight=TRUE,
                     autoWidth = TRUE)
    )
    
    output$station_num <- renderValueBox({
      valueBox(
        "Station",paste(buoy_station),
        color = "purple"
      )
    })
    
    output$Type <- renderValueBox({
      valueBox(
        "Type",paste(marker_data[which(marker_data$Buoy == buoy_station),3]),
        color = "green"
      )
    })
    
    output$Owner <- renderValueBox({
      valueBox(
        "Owner","NOAA",
        color = "teal"
      )
    })
    
    # output$missing <- renderPrint(
    #   if (missing_files$Buoy == buoy_station){
    #     paste("Missing Year:",missing_files[which(missing_files$Buoy == buoy_station),2])
    #   }else {
    #     paste("No missing files")
    #   }
    # )
    
    buoy_raw <- reactive({
      read_csv(paste0("./Output/Raw/",buoy_station,".csv"))
    })
    
    b_date <- min(buoy_raw()$Date)
    e_date <- max(buoy_raw()$Date)
    
    updateDateRangeInput(session,'dateRange',
                         start = b_date,
                         end = e_date,
                         min = b_date,
                         max = e_date)
    
    output$historical_eda <- renderPlot({
      if ((("Wind_speed" %in% input$Historical_variable) & (length(input$Historical_variable)!= 1))){
        
              sendSweetAlert(
                session = session,
                title = "Error",
                text = "Temperature and Wind Speed should not be displayed at the same time",
                type = "error"
              )
        
      } else {
      
      ggplot(buoy_raw()[(buoy_raw()$Date >= input$dateRange[1] & buoy_raw()$Date <= input$dateRange[2]),],aes(x = Date))+
      scale_x_date(date_labels = "%b %Y")+
                                          {if ("Wind_speed" %in% input$Historical_variable){
                                            labs(y = "Wind Speed (m/s)")
                                          }else{labs(y = "Celsius")}}+
                                          {if ("Air_temp" %in% input$Historical_variable){
                                            geom_line(aes(y = Air_temp), 
                                                      color = "#F8766D",
                                                      size = 0.5)
                                          }}+
                                          {if ("Sea_temp" %in% input$Historical_variable){
                                            geom_line(aes(y = Sea_temp), 
                                                      color = "#00BFC4",
                                                      size = 0.5)
                                    
                                          }}+
                                          {if ("Air_sea_temp" %in% input$Historical_variable){
                                            geom_line(aes(y = Air_sea_temp), 
                                                      color = "#C77CFF",
                                                      size = 0.5)
                                          }}+
                                          {if ("Wind_speed" %in% input$Historical_variable){
                                            geom_line(aes(y = Wind_speed), 
                                                      color = "#7CAE00",
                                                      size = 0.5)
                                          }}
        
      }
    })
    
    
  output$month_eda <- renderPlot({
    ggplot(data = buoy_raw(), mapping = aes(x = as.factor(MM),
                                     na.rm = TRUE))+
                                     scale_x_discrete(breaks=c(1:12),
                                                     labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
                                     labs(x = "Month")+
                                     {if (input$Month_variable == "Sea_temp"){
                                       geom_boxplot(fill = "#00BFC4", aes(y = Sea_temp))
                                     }}+
                                     {if (input$Month_variable == "Air_temp"){
                                       geom_boxplot(fill="#F8766D",aes(y = Air_temp))
                                     }}+
                                     {if (input$Month_variable == "Air_sea_temp"){
                                       geom_boxplot(fill="#C77CFF", aes(y = Air_sea_temp))
                                     }}+
                                     {if (input$Month_variable == "Wind_speed"){
                                       geom_boxplot(fill="#7CAE00", aes(y = Wind_speed))
                                     }}
    
  })    
  })
})

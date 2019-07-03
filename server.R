###################################################################
# Data Visualization Final Project: NOAA Buoy Station EDA
# Version 0.2
# See ui.R for updates and todo list
###################################################################
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)
library(tidyverse)
library(ggseas)
library(grid)
library(gridExtra)
library(lubridate)
library(plotly)
###################################################################

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {

  marker_data <- read.csv("./Output/Meta/buoy_meta.csv")
  missing_files <- read.csv("./Error/Missing_files.csv")
  
    
  output$gif <- renderImage({
    list(src = "temp_change.gif",
         contentType = 'image/gif'
         
    )
  }, deleteFile = FALSE)
  
  output$buoy_map <- renderLeaflet({
    
    buoy_icon <- makeIcon(
      iconUrl = "buoy.PNG",
      iconWidth = 30,
      iconHeight = 30
    )
    
    
    m <- leaflet(data = marker_data) %>% 
         addTiles() %>%
         setView(lat = 0, lng = 0, zoom = 0.1) %>% 
         addMarkers(popup = paste("<center><b><a href='",marker_data$URL,"'>","Station ",marker_data$Buoy,"</a></b></center>",
                                  "<br/> Coordinates: (",marker_data$Latitude," N , ",marker_data$Longitude," W)",
                                  "<br/> Owner: NOAA",
                                  "<br/> Buoy Type: ", marker_data$Type,
                                  "<br/>",
                                  "<br/><center><b><a href='",marker_data$URL,"'>","Read More","</a></b></center>"), icon = buoy_icon)
    
    m
    
  })
  
  observe({
    
    

    click <- input$buoy_map_marker_click
      if(is.null(click)){
        return()
    }
  
    
    
    buoy_station <- marker_data[which(marker_data$Longitude == click$lng  & marker_data$Latitude == click$lat),1]
    buoy_raw <- reactive({
      read_csv(paste0("./Output/Raw/",buoy_station,".csv"))
    })
    
    b_date <- min(buoy_raw()$Date)
    e_date <- max(buoy_raw()$Date)
    y_axis = list("Air_temp"="Air Temperature (Celsius)","Sea_temp" = "Sea Temperature (Celsius)", "Air_sea_temp" = "Air Sea Temperature Difference (Celsius)", "Wind_speed" = "Wind Speed")  
    
    tmp2 <- ggplot(data = buoy_raw()[(buoy_raw()$Date >= input$dateRange[1] & buoy_raw()$Date <= input$dateRange[2]),])+
            geom_line(aes(x = Date, 
                          y = Air_temp), 
                      color = "#F8766D",
                      size = 0.5)+
            geom_line(aes(x = Date, 
                          y = Sea_temp), 
                      color = "#00BFC4",
                      size = 0.5)+
            theme(legend.position = "none")+
            scale_x_date(date_labels = "%b %Y")+
            labs(y = "Celsius")
    
    tmp3 <- ggplot(data = buoy_raw()[(buoy_raw()$Date >= input$dateRange[1] & buoy_raw()$Date <= input$dateRange[2]),])+
            geom_line(aes(x = Date, 
                          y = Wind_speed, 
                          color = "Wind Speed"), 
                      color = "#7CAE00", 
                      size =0.5)+
            scale_x_date(date_labels = "%b %Y")+
            labs(x = "Date", 
                 y = "Wind Speed (m/s)")
    
    #change 
    output$overview <- renderPlotly(
      subplot(tmp2,tmp3,
              shareX = TRUE,
              nrows = 2,
              titleY = TRUE)
    )
    
    output$raw <- renderDataTable(
      read_csv(paste0("./Output/Raw/",buoy_station,".csv"))[,1:4],
      options = list(pageLength = 5,
                     lengthMenu = c(5,10,15),
                     scrollX = FALSE,
                     searchHighlight=TRUE,
                     autoWidth = TRUE)
    )
    
    
    updateDateRangeInput(session,'dateRange',
                         start = b_date,
                         end = e_date,
                         min = b_date,
                         max = e_date)
    
    output$historical_eda <- renderPlotly({
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
    
    
  output$month_eda <- renderPlotly({
    
    
    ggplot(data = buoy_raw()[(buoy_raw()$Date >= input$dateRange[1] & buoy_raw()$Date <= input$dateRange[2]),], mapping = aes(x = as.factor(MM),
                                     na.rm = TRUE))+
                                     scale_x_discrete(breaks=c(1:12),
                                                     labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
                                     labs(x = "Month", y = paste0(y_axis[input$Month_variable]))+
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
  
  output$trend_eda <- renderPlotly({
    
    if (input$Trend_variable == "Air_temp_f"){
      ggsdc(data = buoy_raw(), mapping = aes(x = Date, y = Air_temp_f),
            method = "stl", s.window = "period", frequency = 365)+
        geom_line(color="#F8766D") +
        theme(plot.title = element_text(hjust = 0.5))+
        xlab("Time")+
        ylab("Air Temperature (Celsius)")+
        scale_x_date(date_labels = "%b %Y")+
        labs(title = "Time Series Decomposition Analysis")
    } else {
      ggsdc(data = buoy_raw(), mapping = aes(x = Date, y = Sea_temp_f),
            method = "stl", s.window = "period", frequency = 365)+
        geom_line(color="#00BFC4") +
        theme(plot.title = element_text(hjust = 0.5))+
        xlab("Time")+
        ylab("Sea Temperature (Celsius)")+
        scale_x_date(date_labels = "%b %Y")+
        labs(title = "Time Series Decomposition Analysis")
    }
    



     
  })
  
  })
})

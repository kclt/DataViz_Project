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
library(rworldmap)
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
      iconWidth = 20,
      iconHeight = 20
    )
    
    
    m <- leaflet(data = marker_data) %>% 
         addTiles() %>%
         #setView(lat = 0, lng = 0, zoom = 0.1) %>% 
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
    start_date <- Sys.Date() - 3
    end_date <- Sys.Date() + 3 

    if (input$dateRange[1] >= b_date & input$dateRange[1] >= e_date){
      start_date <- min(buoy_raw()$Date)
    } else if (input$dateRange[1] >= b_date){
      start_date <- input$dateRange[1]
    } else {start_date <- min(buoy_raw()$Date)}
    
    if (input$dateRange[2] >= e_date){
      end_date <- max(buoy_raw()$Date)
    } else {
      end_date <- input$dateRange[2]
    }
    
    y_axis = list("Air_temp"="Air Temperature (Celsius)","Sea_temp" = "Sea Temperature (Celsius)", "Air_sea_temp" = "Air Sea Temperature Difference (Celsius)", "Wind_speed" = "Wind Speed")  
    
    
    
    updateDateRangeInput(session,'dateRange',
                            start = start_date,
                            end = end_date,
                           min = b_date,
                           max = e_date)
    
    
    
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
  
  output$legend <- renderImage({
    list(src = "./www/legend.PNG")
  },deleteFile = FALSE)
  
  output$data_scrap <- renderImage({
    list(src = "./www/historical.PNG")
    
  },deleteFile = FALSE)
  
  output$web_scrap <- renderImage({
    list(src = "./www/meta.PNG")
  },deleteFile = FALSE)
  
  output$month <- renderImage({
    list(src = "./www/Month.PNG")
  },deleteFile = FALSE)
  
  output$overviews <- renderImage({
    list(src = "./www/raw.PNG")
  },deleteFile = FALSE)
  
  output$decomposition <- renderImage({
    list(src = "./www/decomposition.PNG")
  },deleteFile = FALSE)
  
  
  output$map_animate_1 <- renderPlot({
    agg_temp <- read_csv("./Output/aggregate/nasa_agg.csv")
    
    worldMap <- getMap()
    world.points <- fortify(worldMap)
    world.points$region <- world.points$id
    world.df <- world.points[,c("long","lat","group", "region")]
    
    breaks <- c(-4.1, -4, -2, -1, -.5, -.2, .2, .5, 1, 2, 4, 5.3)
    val = levels(cut(range(breaks), breaks = breaks))
    color = c("#FFFFFF", "#D9FFD9", "#99EEFF", "#3F94FE", "#77CAFD", "#FFFF4C",
              "#FFCC00", "#FF7E00", "#FF0000", "#5E0000", "#5E0000")
    ggplot() +
      geom_tile(data = agg_temp[,c(2,3,input$yearSlider-1982)], aes(x = lon, y = lat, fill = !!as.name(input$yearSlider)), alpha = 0.8) +
      scale_fill_manual(name="Interval", 
                        breaks = val,
                        values = color,
                        drop = FALSE
                        )+
      geom_path(data = world.df, aes(x = long, y = lat, group = group)) +
      scale_y_continuous(breaks = (-2:2) * 30) +
      scale_x_continuous(breaks = (-4:4) * 45) +
      theme(plot.title = element_text(hjust = 0.5))+
      labs(title = "Temperature Change (celsius) from 1985")+
      theme(panel.background = element_blank())
    
  })
  
  output$anomaly <- renderPlotly({
    anomaly <- read_csv("./Output/aggregate/anomly.csv")
    ggplot(data = anomaly[1:(input$yearSlider-1985),], aes(x = Year, y = Anomaly))+
      geom_smooth(method="lm", color = "#000000")+
      geom_point(color = "#FF0000")+
      ylim(min = 0, max = 1.1)+
      xlim(min = 1985, max = 2018)+
      labs(title = "temperature Anomly compared to 1951-1980 avg",
           y = "difference")+
      theme(plot.title = element_text(hjust = 0.49),
            panel.background = element_blank(),
            axis.text.y = element_text(size = 6))
    
  })
  
  information <- c(
    "Vienna Convention for the Protection of the Ozone Layer implemented"
  )
  
  output$animate_info <- renderUI({
    wellPanel(
      h5(paste(input$yearSlider,"information & Trend", sep = " "), align = "center"),
      p(information[input$yearSlider-1985])
    )
  })
  
})

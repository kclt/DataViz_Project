###################################################################
# Data Visualization Final Project: NOAA Buoy Station EDA
# Version 1.0
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
  
  output$gif <- renderImage({
    list(src = "./Output/gif/temp_change.gif",
         contentType = 'image/gif',
         height = 300
         
    )
  }, deleteFile = FALSE)
  
  output$bear <- renderImage({
    list(src = "./www/polar.jpg",
         height = 300)
  }, deleteFile = FALSE)
  
  output$buoy_map <- renderLeaflet({
    
    buoy_icon <- makeIcon(
      iconUrl = "./www/test.png",
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
    
    
    tryCatch({
      updateDateRangeInput(session,'dateRange',
                           start = start_date,
                           end = end_date,
                           min = b_date,
                           max = e_date)
      
    }, warning = function(x){
      updateDateRangeInput(session,'dateRange',
                           start = b_date,
                           end = e_date,
                           min = b_date,
                           max = e_date)
    }
    )

    
    
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
    color = c("#FFFFFF", "#D9FFD9", "#99EEFF",  "#77CAFD", "#3F94FE", "#FFFF4C",
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
    anomaly <- read_csv("./Output/aggregate/anomaly.csv")
    ggplot(data = anomaly[(anomaly$Year <= input$yearSlider),], aes(x = Year, y = Anomaly, color = Region))+
          geom_line()+
          ylim(min = -1, max = 1.5)+
          xlim(min = 1985, max = 2018)+
          labs(title = "Temperature Anomly",
               y = "Temperature difference to 1951-1980 avg")

  })
  
  output$anomaly_year <- renderPlotly({
    anomaly <- read_csv("./Output/aggregate/anomaly.csv")
    ggplot(data = anomaly[anomaly$Year == input$yearSlider,], aes(x = Region, fill = Region))+
      geom_bar(aes(y=Anomaly),stat="identity")+
      ylim(min=-1,max = 1.5)+
      theme(axis.text.x=element_text(angle = -45, hjust = 0))+
      labs(title = "Temperature Anomly",
           y = "Temperature difference to 1952-1980 avg")
    
  })
  
  information <- c(
    "Vienna Convention for the Protection of the Ozone Layer implemented",
    "no info from noaa",
    "no info from noaa",
    "no info from noaa",
    "no info from noaa",
    "no info from noaa",
    "no info from noaa",
    "no info from noaa",
    "no info from noaa",
    "no info from noaa",
    "no info from noaa",
    "no info from noaa",
    "no info from noaa",
    "no info from noaa",
    "no info from noaa",
    "Notable temperature extremes during 2001 included one of the harshest winters in decades across Siberia and Mongolia. Extreme minimum temperatures as low as -50°C (-70°F) were observed in the Kemerovo region of Russia during mid-January. High heat affected drought-plagued areas of the Middle East, with maximum temperatures reaching as high as 50°C (122°F) in parts of Pakistan during early May, resulting in dozens of deaths.",
    "Global temperatures in 2002 were 0.56°C (1.01°F) above the long-term (1880-2001) average",
    "Annual temperatures were above average across most land areas. The adjacent figure depicts warmer than average temperatures (for a 1961-1990 base period) that were widespread across much of the contiguous United States and Alaska, as well as most of Europe and Asia. Temperatures in these regions were 2-5°C (3.6-9.0°F) above the 1961-1990 average.
    <br/><br/> Notable temperature extremes during 2003 included a severe heatwave during summer of 2003 across Europe. Daily maximum temperatures ranged from 30-37°C (90-99°F) across France, Switzerland and the Mediterranean region, killing approximately 25,000 people. France had its warmest summer on record, and according to news reports, more than 14,000 people died of heat-related causes during the peak of the heat wave in late July and August. In North America, extreme cold winter temperatures resulted in an unusually high ice concentration across the Great Lakes. ",
    "Global temperatures in 2004 were 0.54°C (0.97°F) above the long-term (1880-2003) average, ranking 2004 the fourth warmest year on record.
    <br/><br/>Notable temperature extremes in 2004 included a severe heat wave that affected much of eastern Australia from February until the end of March. Many city and state temperature records were set as maximum temperatures reached 45°C (113°F).",
    "Warmer than average conditions occurred throughout most land areas of the world in 2005. The largest anomalies were widespread throughout high latitude regions of the Northern Hemisphere that include much of Russia, Scandinavia, Canada and Alaska. Temperatures in these regions were 3-5°C (5.4-9.0°F) above the 1961-1990 average. There were no widespread areas of negative anomalies.
",
    "Warmer than average conditions occurred throughout most land areas of the world again in 2006. The largest anomalies were present throughout high latitude regions of the Northern Hemisphere including much of North America, Scandinavia, China and Africa. Temperatures in these regions were 2-4°C (3.6-7.2°F)* above the 1961-1990 average. The only widespread area of negative anomalies occurred in central Russia.",
    "The global annual temperature for combined land and ocean surfaces in 2007 was +0.55°C (+0.99°F) above the 20th century average, ranking 5th warmest in the period of record. The effect of continued presence of La Niña conditions on the December global surface temperature resulted in a slightly lower ranking for the year as a whole. Globally averaged land temperatures were 1.02°C (1.84°F) while the ocean temperatures were 0.38°C (0.68°F) above average, ranking as the warmest and ninth warmest, respectively. The land and ocean surface temperatures for the Northern and Southern Hemisphere ranked second and tenth warmest, respectively.
    <br/><br/>Warmer-than-average temperatures occurred throughout the year in most land areas of the world, with the exception of cooler-than-average anomalies in the southern parts of South America. The largest warmer-than-average anomalies were present throughout high latitude regions of the Northern Hemisphere including much of North America, Europe, and Asia. Temperature anomalies in these regions ranged from 2-4°C (3.6-7.2°F) above the 1961-1990 average.",
    "The global January-December temperature for combined land and ocean surfaces was 0.49°C (0.88°F) above the 20th century average, tying with 2001 as the eighth warmest since records began in 1880. Globally averaged land temperatures were 0.81°C (1.46°F) above average, while the ocean temperatures were 0.37°C (0.67°F) above average, ranking as the sixth warmest and tenth warmest, respectively. Eight of the ten warmest years on record have occurred since 2001, part of a rise in temperatures of 0.5°C (0.9°F) since 1880.
    <br/><br/>Warmer-than-average temperatures occurred throughout the year in most land areas of the world, with the exception of cooler-than-average conditions across Colombia, parts of Alaska, central Canada, and the midwestern continental U.S. The warmest above-average temperatures occurred throughout high latitude regions of the Northern Hemisphere including much of Europe and Asia. Temperature anomalies in these regions ranged from 2-4°C (3.6-7.2°F) above the 1961-1990 average.",
    "Europe: UK mean temp during winter was 3.2 c, its coldest winter since 1996. An extratropical storm brought heavy snow to parts of UK. 
    <br/><br/> Asia: China suffered from its worst drought in five decades. Violent storms across centraChina.
    <br/><br/> America: Record floods on the red River in northern plains region. Wettest October since records began 115 years ago. Below average tornado season.
    <br/><br/> Australia: Record breaking heatwave affected southern Australia during jan-feb. Accompanying very dry conditions contribted to the development of deadily wildfires. Warmest August since national record.",
    "For 2010, the combined global land and ocean surface temperature tied with 2005 as the warmest such period on record, at 0.62°C (1.12°F) above the 20th century average of 13.9°C (57.0°F). 1998 is the third warmest year-to-date on record, at 0.60°C (1.08°F) above the 20th century average.
    <br/><br/>The year 2010 tied with 2005 as the warmest year since records began in 1880. The annual global combined land and ocean surface temperature was 0.62°C
    <br/><br/>Russia: high temperatures dominated parts of western Russia during June adn July.",
    "Europe: During August 2011, Europe experinced one of its word heat waves since 2003, France, Spain and Switzerland had their warmest spring on record. France had its warmest jan-oct on record and Spain experienced its warmest year since national record.
    <br/><br/> Asia: Heavy rainstorms everywhere.
    <br/><br/> America: Persistent, scorching heat across the United States during the summer contributed to several southern states experienceg their warmest summer since records begain in 1895. 
    <br/><br/> Alaska: Driest may sicne records begain in 1918",
    "Europe: Experienced an unusally dry spring, leading to extreme drought conditions, impacing crops, water supplies, and human health. Dryness also contributed to significant wildfires.
    <br/><br/> Eurasia: A cold wave affected most of the Eurasia during the mid-jan to mid-feb. This was the worst cold in at least 26 years in central and eastern Europe. More than 650 people died due to frigid conditions. Northeast China recorded minimum temperature ranging between -30c to -40c.
    <br/><br/> America: Nearly 2/3 of United States was in drought by the end of sept 2012. The palmer Drought Severity Index of 55 percent in June 2012 was the largest percentage since Dec 1956. The drought resulted in a multi-billion dollar aricultrual disaster.
    <br/><br/> Canada: Experienced its warmest summer since national records
    <br/><br/> Brazil: Severe Drought.",
    "Europe: UK experienced its coldest March and May since 1996. Overall, spring was the coldest since 1962.While Spain received more than triple its monthly average precipitation.
    <br/><br/> Asia: Parts of southern China experienced one of ther most sever heat waves during july and Aug. More than 300 stations recorded daily maximum temperature higher than 40 C. Over 40 people died due to the heat.
    <br/><br/> America: Drought conditions improved across the southeast and central US, but deteriorated in the Fat west during 2013. Cali had its driest year and oregon experienced its fourth driest year on record.",
    "Europe: As a whole, experienced its warmest year on record.
    <br/><br/> Asia: Japan had its wettest August, receiving nearly triple its monthly average.
    <br/><br/> America: Numerous Arctic air outbreaks in early 2014 set the stage for a cool year across the Midwest and Mississippi River Valley. Seven states had a top 10 cool year. Meanwhile, much of the west was warmer than average.
    <br/><br/> Mexico: Much of the year was warmer than average.
    <br/><br/> Canada: Had its coldest year since 1996. Winter was characterized by crippling cold temperatures and record snowfall, resulting in the coldest winter in 18 years.",
    "Europe: As a whole, experienced its second warmest year on record, behind 2014, with several countries had a top 5 year.
    <br/><br/> Asia: Much warmer than average conditions werer present across much of the continent. 2015 was the warmest year since records began in 1910. Hong Kong expereincing its warmest Jun-Aug period on record.
    <br/><br/> America: The US had its second warmest and theird wettest year since record.
    <br/><br/> Canada: parts of western Canada had their warmest summer on record. Moderate to extreme drought developed across parts of western Canada due to the unusal warmth and dryness.
    <br/><br/> Africa: 2015 was the second warmest year, behind 2010.
    <br/><br/> Arctic sea ice: the artic had its smallest anuual growth season.",
    "The Paris Agreement implemented (effective date: 2016-11-04)
    <br/><br/>Europe: Experienced its 3rd warmest year, behind 2014 and 2015. Making the past three years the threee warmest in the 107 continental record. The average winter temperature was record high.
    <br/>
    <br/> Asia: April, August, and Sept were each record warm, while Oct and Nov were both cooler than their long term average. Guangzhou recorded its first snow since 1967 and a low tem of 3.1 c was observed in Hong Kong.
    <br/>
    <br/> America: 2016 was the warmest year for North America since records began in 1910, surpassing the previous record set in 1998. and Alaska expereince the warmest year for the state since records begain in 1925. While large areas of record warmth for South America.
    <br/>
    <br/> Australia observed its 4th warmest year in it 107 year record.",
    "Europe: Much warmer than average conditions
    <br/><br/> Asia: Much warmer than average conditions and caused heavy precipitation(China, Nepal, Thailand)
    <br/><br/> America: 2017 national temperature was the third highest since 1895, behind 2012 and 2016.
    <br/><br/> Mexico: had its highest jan-oct temperature since record, besting the previous record set in 2016.
    <br/><br/> near to above average hurricane season around the world",
    "Europe: Much warmer than average conditions engulfed much of Europe for most of 2018, resulting in the warmest year on record.
    <br/><br/> Asia: Above average typhoon activity 
    <br/><br/> America: Above average hurrican activity and yearly percipitation was the third wettest year on record.
    <br/><br/> Australia: Had its thrid warmest year since national records
    <br/><br/> Overall warmer temperature observed around the world" 
    
    
  )
  
  output$animate_info <- renderUI({
    wellPanel(
      h5(paste(input$yearSlider,"information & Trend", sep = " "), align = "center"),
      HTML(information[input$yearSlider-1985])
    )
  })
  
})

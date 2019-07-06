noaa_rawplot <- function(buoy_station){
  
  tmp <- read_csv(paste0("./Output/Raw/",buoy_station,".csv"))
  
  tmp2 <- ggplot(data = tmp)+
    geom_line(aes(x = Date, 
                  y = Air_temp), 
              color = "#F8766D",
              size = 0.5)+
    geom_line(aes(x = Date, 
                  y = Sea_temp), 
              color = "#00BFC4",
              size = 0.5)+
    theme(legend.position = "none", 
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank())+
    scale_x_date(date_labels = "%b %Y")+
    labs(y = "Celsius")
  
  tmp3 <- ggplot(data = tmp)+
    geom_line(aes(x = Date, 
                  y = Wind_speed, 
                  color = "Wind Speed"), 
              color = "#7CAE00", 
              size =0.5)+
    scale_x_date(date_labels = "%b %Y")+
    labs(x = "Date", 
         y = "Wind Speed (m/s)")
  
  mylegend <- legendGrob(labels = c("Air Temperature", 
                                    "Sea Temperature", 
                                    "Wind Speed"),
                         ncol = 3,
                         pch = 15,
                         gp = gpar(col = c("#F8766D","#00BFC4","#7CAE00"), 
                                   fill = "gray"))
  
  b.date <- year(min(tmp$Date))
  e.date <- year(max(tmp$Date))
  
  grid.arrange(arrangeGrob(tmp2,tmp3, ncol = 1), 
               mylegend,heights=c(10, 1),
               top = textGrob(paste("Station",
                                    buoy_station,
                                    "Historical Information",
                                    "(",b.date,"-",e.date,")", 
                                    sep = " ")))
  
}
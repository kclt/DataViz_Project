noaa_meta <- function(buoy_station){
  url <- paste0("https://www.ndbc.noaa.gov/station_page.php?station=",buoy_station)
  
  des <- url %>% 
    read_html() %>% 
    html_nodes("h1") %>% 
    html_text() %>%
    trimws() %>% 
    as.character()
  
  des <- gsub(",","",des,fixed = TRUE)
  
  meta1 <- url %>% 
    read_html() %>% 
    html_nodes("#stn_metadata") %>% 
    trimws() %>% 
    as.character()
  
  type <- strsplit(meta1,"</b>")[[1]][2]
  type <- strsplit(type,"<b>")[[1]][2]
  
  lat <- word(meta1,start = 1, sep = " N ")
  lat <- word(lat,-1)
  lat <- as.numeric(str_extract(lat, "\\-*\\d+\\.*\\d*"))
  
  long <- word(meta1,start = 1, sep = " W ")
  long <- word(long,-1)
  long <- as.numeric(str_extract(long, "\\-*\\d+\\.*\\d*"))
  
  
  write(paste(buoy_station,des,type,lat,long,url,sep = ","),
        file = "./Output/Meta/buoy_meta.csv",
        append = TRUE)
}

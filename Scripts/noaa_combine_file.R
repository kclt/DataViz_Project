noaa_combine_file <- function(n,buoy_station){
  
  header <- scan(paste0("./Source/",buoy_station,"/",n), 
                 nlines = 1, 
                 what = character())
  data <- read.table(paste0("./Source/",buoy_station,"/",n), 
                     skip = 2, 
                     header = FALSE, 
                     fill = TRUE)[1:16]
  names(data) <- header[1:16]
  as.tibble(data)
  
  data.tidy <- data %>% 
    # Select a specific hour
    dplyr::filter(hh == 13) %>% 
    # Select wanted variables
    select(YY = contains("YY"), MM ,DD, 
           Air_temp = ATMP, 
           Sea_temp = WTMP, 
           Wind_speed = WSPD) %>%  
    #Dewpoint_temp = DEWP) %>% 
    # Due to changing naming convention of YYYY to YY we have to adjust for the change 
    mutate(YY = ifelse(str_length(YY)==2,str_c(19,YY,sep = "", collapse = NULL),YY)) %>% 
    mutate(Date = paste(as.character(YY),as.character(MM),as.character(DD), sep = "-")) %>%
    # Replace Missing values
    mutate(Air_temp = replace(Air_temp, Air_temp == 999.0,NA)) %>% 
    mutate(Sea_temp = replace(Sea_temp, Sea_temp == 999.0,NA)) %>%
    mutate(Wind_speed = replace(Wind_speed, Wind_speed == 99.0, NA)) %>% 
    mutate(Air_sea_temp = round(Air_temp-Sea_temp),-1) %>% 
    #mutate(Dewpoint_temp = replace(Dewpoint_temp, Dewpoint_temp == 999.0,NA)) %>% 
    mutate(Date = as.Date(Date)) %>% 
    select(Date,Air_temp,Sea_temp,Wind_speed,Air_sea_temp) 
  
  #Pipe data into one single dataframe
  if (is_empty(tmp.tidy))
  {
    tmp.tidy <- data.tidy
  } else
  {
    tmp.tidy <- tmp.tidy %>% full_join(data.tidy)
  } 
}

# Loop to combine file

for (j in station){
  tmp.tidy <- NULL
  for (i in list.files(path =paste0("./Source/",j), pattern = ".txt")){
    
    #combine multiple files
    tmp.tidy <- noaa_combine_file(i,j)
    
    #Add in missing date and remove leap yaer data for better analysis
    tmp.tidy <- tmp.tidy %>% 
      pad() %>% 
      dplyr::filter(!str_detect(Date,"02-29")) %>% 
      mutate(MM = month(Date))
    write_csv(tmp.tidy, path = paste0("./Output/Raw/",j,".csv"))
  }
}

# Time series composition requires full unbroke timeseries
for (k in station){
  tmp.times <- read_csv(paste0("./Output/Raw/",k,".csv"))
  b.date <- year(min(tmp.times$Date))
  e.date <- year(max(tmp.times$Date))
  
  t.series.a <- ts(tmp.times[,2],frequency = 365)
  tryCatch(t.series.a <- na.seadec(t.series.a, algorithm = "interpolation"),
           error = function(x){
             tryCatch(t.series.a <- na.locf(t.series.a, option = "locf"),
                      error = function(x)return())})
  
  
  t.series.s <- ts(tmp.times[,3],frequency = 365)
  tryCatch(t.series.s <- na.seadec(t.series.s, algorithm = "interpolation"),
           error = function(x){
             tryCatch(t.series.s <- na.locf(t.series.s, option = "locf"),
                      error = function(x)return())})
  
  tmp.times <- tmp.times %>%
    mutate(Air_temp_f = as.numeric(t.series.a)) %>%
    mutate(Sea_temp_f = as.numeric(t.series.s))
  
  write_csv(tmp.times, path = paste0("./Output/Raw/",k,".csv"))
}

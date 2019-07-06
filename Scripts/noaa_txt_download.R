noaa_txt_download <- function(buoy_station,start_year,end_year){
  
  # Create director folder for buoy_station
  dir.create(paste0("./Source/",buoy_station))
  
  # For loop function to save files from start year to end year
  for (year in start_year:end_year){
    
    url <- paste0("https://www.ndbc.noaa.gov/view_text_file.php?filename=",
                  buoy_station,"h",year,".txt.gz&dir=data/historical/stdmet/")
    
    tryCatch(download.file(url,
                           paste0("./Source/",
                                  buoy_station,"/", buoy_station, "_", year,".txt")),
             error = function(e) {
               # If file doesn't exist for that year write to missing file
               write(paste(buoy_station,year,sep = ","),
                     file = "./Error/Missing_files.csv",
                     append = TRUE)
             })
  }
}
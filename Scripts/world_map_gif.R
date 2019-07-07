# World map
worldMap <- getMap()
world.points <- fortify(worldMap)
world.points$region <- world.points$id
world.df <- world.points[,c("long","lat","group", "region")]

temp <- read.table("./Source/NASA/2018.txt", skip=1, header = TRUE, na.strings="9999.0000")
breaks <- c(-4.1, -4, -2, -1, -.5, -.2, .2, .5, 1, 2, 4, 5.3)
color_scal <- cbind(
  val = levels(cut(range(breaks), breaks = breaks)),
  col = c("#77CAFD", "#99EEFF", "#D9FFD9", "#FFFFFF",
          "#FFFF4C", "#FFCC00", "#FF7E00", "#FF0000", "#5E0000")
)
color_scal <- data.frame(color_scal, stringsAsFactors = FALSE)
colnames(color_scal) <- list("val", "col")
color_scal$col <- paste(color_scal$col,"FF", sep = "")
temp$interval <- cut(temp$array.i.j, breaks = breaks)

rotate_map <- function(angle = -74){
  ggplot() + 
    geom_tile(data = temp, aes(x = lon, y = lat, fill = interval), alpha = 0.8) +
    scale_fill_manual("interval", breaks = color_scal$val, values = color_scal$col) +
    geom_path(data = world.df, aes(x = long, y = lat, group = group)) +
    scale_y_continuous(breaks = (-2:2) * 30) +
    scale_x_continuous(breaks = (-4:4) * 45) +
    labs(title = "Temperature Change (celsius) from 1985 to 2018")+
    coord_map("ortho", orientation=c(61, angle, 0))
}


saveGIF({
  ani.options(nmax = 360)
  for(i in seq(0,360,by = 5)){
    print(rotate_map(i))
    print(i)
  }
}, interval = 0.1, outdir="./Output/gif", movie.name = "temp_change.gif")
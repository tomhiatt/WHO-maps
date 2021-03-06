# Check file for disputed areas 
# And also to see if it works.
# As presented in http://gamapserver.who.int/gho/gis/training/DMF_GIS2010_2_SOPSforWHOMaps.pdf

dta <- subset(n, year==2010)
dta$cat <- as.factor(dta$g_hbc22)

dta <- n[n$year==2010 & n$g_whoregion=="WPR",]
dta$cat <- cut(dta$tot_newrel,breaks=c(0,10,100,1000,10000,100000,Inf), right=FALSE) 
dta[dta$iso3=="AUS", "cat"] <- NA

p <- WHOmap.print(data=dta,legend.title="HBCs", map.title="High-burden countries",low.color="red", line.color="black", show=TRUE)
WHOmap.print(dta)
WHOmap.print(dta, shapefiles.path="C:/mapfiles/MapTemplate_generalized_2011/Shapefiles")

WHOmap.print(dta, zoom = "WPR")

p <- whomap(dta)
p <- WHOmap.print(data=dta,legend.title="HBCs", map.title="High-burden countries",low.color="red", line.color="black")

WHOmap.print(dta, colors=c('orange', 'black', 'red', 'blue', 'violet', 'green'))

WHOmap.print(dta, low.color='green', high.color='blue')

p + coord_cartesian(xlim=c(10, 40), ylim=c(-40, -20)) # Make sure South Africa doesn't cover up Lesotho and Swaziland
p + coord_cartesian(xlim=c(-25, 0), ylim=c(20, 30)) # (a) Western Sahara in grey
p + coord_cartesian(xlim=c(10, 40), ylim=c(40, 50)) # (b) Kosovo dashed
p + coord_cartesian(xlim=c(20, 40), ylim=c(20, 25)) # (c) Egypt-Sudan border dashed
p + coord_cartesian(xlim=c(25, 45), ylim=c(0, 10)) # (d) South Sudan-Kenya border dashed
p + coord_cartesian(xlim=c(20, 40), ylim=c(5, 15)) # (e) Sudan-South Sudan border dashed and Abyei area dotted and grey
p + coord_cartesian(xlim=c(30, 40), ylim=c(30, 35)) # (f) West Bank and Gaza Strip dashed
p + coord_cartesian(xlim=c(70, 85), ylim=c(30, 40)) # (g) Jammu and Kashmir/Aksai Chin dashed and grey
p + coord_cartesian(xlim=c(90, 100), ylim=c(25, 30)) # (h) Arunachal Pradesh same color as India with solid line
p + coord_cartesian(xlim=c(120, 130), ylim=c(35, 40)) # (i) Korean peninsula separated with dashed line

# Hadley's code for finding centre of a country
# These functions allow you to identify the centre of a country
wrap <- function(x) {
  n <- length(x)
  x[c(n, seq_len(n - 1))]
}
area <- function(x, y) {
  sum(wrap(x) * y - x * wrap(y)) / 2
}
centroid <- function (x1, y1) {
  n <- length(x1)
  wrap <- c(n, 1:(n - 1))
  x2 <- x1[wrap]
  y2 <- y1[wrap]
  a <- x1 * y2 - x2 * y1
  s <- sum(a) * 3
  if (s < 1e-3) {
    c(mean(x1), mean(y1))
  } else {
    c(sum((x1 + x2) * a)/s, sum((y1 + y2) * a)/s)
  }
}
info <- function(df) {
  info <- c(centroid(df$long, df$lat), area(df$lat, df$long))
  names(info) <- c("long", "lat", "area")
  info
}

# World boundaries and centres -----------------------------------------------

pieces <- ddply(subset(gworld, !hole), c("id", "piece"), info)
centres <- ddply(pieces, "id", summarise, 
                 lat = lat[which.max(area)], 
                 long = long[which.max(area)],
                 area = sum(area))
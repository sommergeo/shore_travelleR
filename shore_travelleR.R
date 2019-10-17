library(rgdal)
library(geosphere)
library(ggplot2)
library(readr)

setwd("C:/data/git/shore_travelleR/work") # your work folder here


#### Bathymetric distance gain --------------------------------------------

#Data
## Bathymetric lines
b <- readOGR(dsn = "../data", layer = "gebco_contours_1km_4326")
proj4string(b) # check CRS

## Sites
site <- SpatialPoints(cbind(31.1, -29.5)) # fake coordinates in this example
proj4string(site) <- CRS("+proj=longlat +datum=WGS84")

# Distance calculation
## Measure distance from site to closest shorline. Shorelines from 1m bathimetric data
df <- data.frame(elevation=integer(), distance=numeric(), x=numeric(), y=numeric())
for (i in seq(max(b$ELEV), min(b$ELEV), -1)){
  idist <- dist2Line(site, b[b$ELEV == i,])
  df[nrow(df)+1,] <- list(i, 
                          idist[1]/1000, # Distance in km
                          idist[2],
                          idist[3])
  rm(idist, i)}

## Plot distance-depth relationship
plt_distance_depth <- ggplot() +
  labs(x = "Shore distance km", y = "RSL m", caption = "") +
  geom_line(aes(x=df$distance, y=df$elevation)) 

plot(plt_distance_depth)

#### Distance reconstruction-----------------------------------------------
# Data
## Sea level data
sl <- read_table2("../data/waelbroeck2002_data.txt")

## Create timetable with 1ka steps
rec <- data.frame(timestep=seq(0, 400, 1)) # 1 ka until 400 ka BP

rec$sl <- approx(x=sl$Age, y=sl$RSL, xout=rec$timestep, method="linear")$y
rec$dist <- approx(x=df$elevation, y=df$distance, xout=rec$sl, method="linear")$y
rec$sl_upper <- approx(x=sl$Age, y=sl$'RSL+', xout=rec$timestep, method="linear")$y
rec$dist_upper <- approx(x=df$elevation, y=df$distance, xout=rec$sl_upper, method="linear")$y
rec$sl_lower <- approx(x=sl$Age, y=sl$'RSL-', xout=rec$timestep, method="linear")$y
rec$dist_lower <- approx(x=df$elevation, y=df$distance, xout=rec$sl_lower, method="linear")$y
write.csv(rec, file="coast_dist_SIB.csv")

## Plot distance reconstruction
plt_dist_age <- ggplot() +
  labs(x = "Age ka BP", y = "Shore distance km", caption = "") +
  scale_x_continuous(breaks=seq(min(rec$timestep), max(rec$timestep), 10), limits=c(130, 0), trans = "reverse") +
  scale_y_continuous(breaks=seq(0,100,5)) +
  geom_ribbon(data=rec, aes(ymin=rec$dist_lower, ymax=rec$dist_upper, x=rec$timestep), alpha=0.2, fill="#56B4E9") +
  geom_line(aes(x=rec$timestep, y=rec$dist), color="#56B4E9")
  
plot(plt_dist_age)



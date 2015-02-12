library(sp)

# experimental point
lat <- 48.043486
lon <- -74.140737

# create a point (longitude first)
point <- c(lon, lat)

# read the txt file with all the wheather stations
weather.stations <- read.table("/home/thalles/Desktop/R Weather Testing Code/ghcnd-stations.txt", 
     header = FALSE, fill=TRUE, col.names=c(1:15), encoding="UTF-8")

# calculate the distance from the experimental point to each of the
# points representing the weather stations.

# get the coordinates from the stations
coordinates <- weather.stations[,c(2,3)]

# adding some column headings
colnames(coordinates) <- c("lat", "lon")

# invert the order, to put longitude first and then latitude
coordinates <- coordinates[c("lon","lat")]

# compute the distance from the experimental point to each
# weather station
dist.km <- spDistsN1(as.matrix(coordinates), point, longlat=T)

# add the distances to the data frame
weather.stations <- cbind(weather.stations, dist.km)

# sort the data frame by distance DESCENDEND in order to get
# the closest weather stations
weather.stations <- weather.stations[order(dist.km), ]

# eliminate entire columns with NA added previously
weather.stations <- weather.stations[,colSums(is.na(weather.stations)) != nrow(weather.stations)]

# get only the closest stations
weather.stations <- head(weather.stations, n=20)
###################################################################
# new verify which of these stations has data available for the 
# chosen year
###################################################################

# read the txt file with information on all weather stations and
# the interval of years in which each stations has data collected
stations <- read.table("/home/thalles/Desktop/R Weather Testing Code/ghcnd-inventory.txt", 
                       header = FALSE,
                       col.names=c("StationID", "lat", "lon", "Info", "StartYear", "EndYear"))

# input year
start.year <- 2014
end.year <- 2014

# select only the weather stations which fall in the starting and ending 
# year specified 
stations <- subset(stations, start.year >= StartYear & end.year <= EndYear &
                     (substr(StationID, 1, 2) == "CA" | substr(StationID, 1, 2) == "US") &
                     ( Info == "TMAX" | Info == "TMIN" | Info == "PRCP" ) )


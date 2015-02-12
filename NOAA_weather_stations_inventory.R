library(sp)
library("plyr")
library("doParallel")

# experimental point
lat <- 48.043486
lon <- -74.140737
start.year <- 1995
end.year <- 2014


# create a point (longitude first)
point <- c(lon, lat)

# read the txt file with information on all weather stations and
# the interval of years in which each stations has data collected
stations <- read.table("/home/thalles/Desktop/R Weather Testing Code/ghcnd-inventory.txt", 
                       header = FALSE,
                       col.names=c("StationID", "lat", "lon", "Info", "StartYear", "EndYear"))


# select only the weather stations which fall in the starting and ending 
# year specified 
stations <- subset(stations, start.year >= StartYear & end.year <= EndYear &
         (substr(StationID, 1, 2) == "CA" | substr(StationID, 1, 2) == "US") &
         ( Info == "TMAX" | Info == "TMIN" | Info == "PRCP" ) )


##############################################

##############################################

# get the coordinates from the stations
coordinates <- stations[,c(2,3)]

# adding some column headings
colnames(coordinates) <- c("lat", "lon")

# invert the order, to put longitude first and then latitude
coordinates <- coordinates[c("lon","lat")]

# compute the distance from the experimental point to each
# weather station
distance.km <- spDistsN1(as.matrix(coordinates), point, longlat=T)

# add the distances to the data frame
stations <- cbind(stations, distance.km)

# sort the data frame by distance DESCENDEND in order to get
# the closest weather stations
stations <- stations[order(distance.km), ]


count = 0
closest.stations <- list()

# get the closest wheather stations from the [point] in which
# all of stations have TMAX, TMIN, and PRCP information available
for(sta in split(stations, stations$distance.km) )
{
  # check if all stations have TMAX, TMIN, and PRCP data available
  if (nrow(sta) == 3)
  {
    closest.stations <- append(closest.stations, list(sta))
    count = count + 1  
  }
  else
  {
    print("Rejected")
    print(sta)
  }
  
  if (count == 10)
    break
}
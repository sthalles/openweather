library(sp)
library("plyr")
library("doParallel")

# experimental point
# lat <- 48.043333
# lon <- -74.140556
# start.year <- 1995
# end.year <- 2014
# path.to.ghcnd.inventory = '/home/thalles/Desktop/R Weather Testing Code/ghcnd-inventory.txt'
# stations.from <- c("US", "CA")
# data <- c("TMAX", "TMIN", "PRCP")
# strict = TRUE
# n = 10

nearest.stations <- GetNearestStations(lat=48.043333, lon=-74.140556, start.year=1995, end.year=2014, 
                   stations.from=c("US", "CA"), data=c("TMAX", "TMIN", "PRCP"),
                   path.to.ghcnd.inventory = '/home/thalles/Desktop/R Weather Testing Code/ghcnd-inventory.txt')

GetNearestStations <- function( lat, lon, start.year, end.year = start.year, data = NULL,
                                path.to.ghcnd.inventory = 'ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt',
                                stations.from = NULL, strict = TRUE, n = 10 )
{
  # Given a coordinate point, it computes the closest weather station to that point
  # based on information provided by NOOA website at ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/
  #
  # Args:
  #   lat: latitude coordinate in decimal degrees form. e.g. 48.043486
  #
  #   lon: lontirude coodinate in decimal defrees form. e.g. -74.140737
  #
  #   start.year: numeric: year in which the weather information will be captured
  #
  #   end.year: numeric: in case of a range of years, this represents the upper limit,
  #              if not defined, its default value is [start.year]
  #  
  #   data: vector containing the specific weather information to be retrieved
  #          possible values consiste of TMIN, TMAX, PCPT. e.g. c("TMAX", "TMIN")
  #
  #   path.to.ghcnd.inventory: string: in case a user wants to use the file locally
  #                            locally stored, the path to the file is defined here.
  #                            By default it fetches information from:
  #                            ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt.
  #                            For better performance, it is recommended to download
  #                            the file 'ghcnd-inventory' from the website above
  #                            and locally read it specifying its path.
  #
  #   stations.from: vector: defines from which countries a user wants to retrieve 
  #                          weather information from. A value such c("CA", "US"),
  #                          will only considered the weather stations located at
  #                          Canada and the US. A list of countries
  #                          witch have weather stations cataloged can be seen at:
  #                          ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-countries.txt.
  #                          If not specified, all of the weather stations located 
  #                          at each country will be considered. 
  #
  #   strict: boolean: it defines whether or not a user wants each weather station
  #                    to strictly have the weather information required in the
  #                    [data] parameter. In other words, if the data parameter 
  #                    says it required TMAX and TMIN, i.e. data = c("TMAX", "TMIN"), the
  #                    function will only output the weather stations with these two
  #                    features available for retrieving.
  #
  #   n: numeric: defines the number of closest stations to be output
  
  # create a point (longitude first)
  point <- c(lon, lat)
  
  # read the txt file with information on all weather stations and
  # the interval of years in which each stations has data available
  stations <- read.table(path.to.ghcnd.inventory, 
                         header = FALSE,
                         col.names=c("StationID", "lat", "lon", "Info", "StartYear", "EndYear"))
  
  # select only the weather stations which fall in the starting and ending 
  # year specified 
  stations <- subset(stations, start.year >= StartYear & end.year <= EndYear)
  
  # subset the data based on weather station locations
  if (!is.null(stations.from))
  {
    # get the first to letters from the weather station ID
    s <- substr(stations$StationID, 1, 2)
    
    # get only the stations specified in the argument 
    stations <- stations[s %in% stations.from, ]
  }
  
  # subset the data based on which information needs to be retrieved 
  if (!is.null(data))
  {
    stations <- stations[stations$Info %in% data, ]
  } else 
  {
    # because no features were requested, strict is meaningless
    strict = FALSE
  }
  
  ##############################################
  ##############################################
  
  # get the coordinates from the stations
  coordinates <- stations[,c(2,3)]
  
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
  
  if (isTRUE(strict))
  {
    # get the closest wheather stations from the point defined by [lat] and [lon] in which
    # each one of the stations do have the features defined by the parameter [data]
    for(sta in split(stations, stations$distance.km) )
    {
      # check if all stations have the features defined by the [data] parameter
      if (identical(nrow(sta), length(data)))
      {
        closest.stations <- append(closest.stations, list(sta))
        count = count + 1  
      }
      
      if (identical(count, n))
        break
    }
    closest.stations
  } else {
    for (sta in split(stations, stations$distance.km))
    {
      closest.stations <- append(closest.stations, list(sta))
      count = count + 1  
      
      if (identical(count, n))
        break  
    }
  }
  closest.stations
  
}
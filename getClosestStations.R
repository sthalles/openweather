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


DownloadWeatherData <- function(year, ...)
{
  # defines the path in the NOAA servers where the weather data is
  path.to.file <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/"
  
  # name of the file
  # in the NOAA servers, each file ends with extension '.csv.gz'
  file.name <- paste(year, '.csv.gz', sep="")
  
  print (paste(path.to.file, file.name, sep=""))
  
  # download the file containing weather data for an
  # specific year from the NOAA's servers
  download.file(url= paste(path.to.file, file.name, sep=""), 
                destfile=file.name, 
                method='auto')
  
}


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
                         col.names=c("stationID", "lat", "lon", "feature", "startYear", "endYear"),
                         colClasses=c("factor", "numeric", "numeric", "factor", "integer", "integer"))
  
  # select only the weather stations which fall in the starting and ending 
  # year specified 
  stations <- subset(stations, start.year >= startYear & end.year <= endYear)
  
  # subset the data based on weather station locations
  if (!is.null(stations.from))
  {
    # get the first to letters from the weather station ID
    s <- substr(stations$stationID, 1, 2)
    
    # get only the stations specified in the argument 
    stations <- stations[s %in% stations.from, ]
  }
  
  # subset the data based on which information needs to be retrieved 
  if (!is.null(data))
  {
    stations <- stations[stations$feature %in% data, ]
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
  closest.stations <- data.frame()
  
  if (isTRUE(strict))
  {
    # get the closest wheather stations from the point defined by [lat] and [lon] in which
    # each one of the stations do have the features defined by the parameter [data]
    for(sta in split(stations, stations$distance.km) )
    {
      # check if all stations have the features defined by the [data] parameter
      if (identical(nrow(sta), length(data)))
      {
        closest.stations <- rbind(closest.stations, sta[1,])
        count = count + 1  
      }
      
      if (identical(count, n))
        break
    }
    closest.stations
  } else {
    for (sta in split(stations, stations$distance.km))
    {
      closest.stations <- rbind(closest.stations, sta[1,])
      count = count + 1  
      
      if (identical(count, n))
        break  
    }
  }
  l <- list( years = c(start.year, end.year), data = data, starting.point = point,
             closest.stations = list(closest.stations[c("stationID", "lat", "lon", "distance.km")]))
    
}





GetWeatherData <- function(nearest.stations, path.to.weather.data=NULL)
{
  # get actual weathe data from the NOAA's servers
  # the input list for this function has to be the list ouput 
  # by function getNearestStations
  
  # get only the station ID records
  stations.id <- droplevels(as.data.frame(nearest.stations[[4]])$stationID)
  
  # get only the desired information, which is the kind of data a user
  # is looking for, e.g. TMAX, TMIN, TPRCP....
  data <-nearest.stations$data
  
  # get the year interval
  from.to.year <- nearest.stations$years
  
  # if path.to.weather.data is NULL, download the data from NOAA's servers
  if (is.null(path.to.weather.data)){
    path.to.weather.data <- DownloadWeatherData <- function(from.to.year)
  }
  
  path.to.weather.data <- '/home/thalles/Desktop/R\\ Weather\\ Testing\\ Code/2000/2000.csv'
  
  # get the file's number of lines to make the read.csv funcion more efficient
  lines <- system(paste("wc -l", path.to.weather.data, sep=" "), intern = T)
  
  # regular expression to extract only the number from the output
  m <- regexpr("[1-9]*", lines, perl=TRUE) 
  lines <- as.numeric(regmatches(lines, m))
  
  # read the file containing the weather data
  df <- read.csv("/home/thalles/Desktop/R Weather Testing Code/2000/2000.csv",
                 header=F,
                 col.names=c("stationID", "date", "feature", "value", "NULL", "NULL", "degree", "NULL"),
                 colClasses=c("character", "factor", "character", "integer", "NULL", "NULL", "character", "NULL"),
                 nrows = lines, comment.char = "")
  
  
  # get only the desired stations
  df <- df[df$stationID %in% stations.id, ]
  
  # filter only for the requested data 
  df <- df[df$feature %in% data, ]
  
  # divides the data into chunks of weather stations
  weather.stations.list <- split(df, df$stationID )
  
  # get info of the one stations with data regarding a whole year
  station1 <- l[[1]]
  
  stations <- stations[order(distance.km), ]
  
}



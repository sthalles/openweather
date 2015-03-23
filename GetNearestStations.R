library(sp)

nearest.stations <- GetNearestStations(lat=48.043333, lon=-74.140556, start.year=2000, end.year=2000,
                                       path.to.ghcnd.inventory = "/Users/thalles/Desktop/openweather/ghcnd-inventory.txt",
                                       working.dir = '/Users/thalles/Desktop/openweather')


GetNearestStations <- function( lat, lon, start.year, end.year = start.year, data = NULL,
                                path.to.ghcnd.inventory = NULL,
                                working.dir = getwd(), stations.from = NULL, strict = TRUE, n = 10 )
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
  #   path.to.ghcnd.inventory: string: Path to the ghcnd-inventory.txt file.
  #                            If a user supplies the right path to the 
  #                            file called ghcnd-inventory.txt, then it will be 
  #                            read normally, however, if the file path is incorrect,
  #                            or not specified, then an attempt to find the file in
  #                            the current working directory will be made, if no 
  #                            success, the file will be downloaded 
  #                            from the NOAA's servers before the reading attempt.
  #                            In case of downloading, the file will be located 
  #                            in the directory defined working.dir.
  #                            By default it fetches information from:
  #                            ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt.
  #                            For better performance, it is recommended to download
  #                            the file 'ghcnd-inventory' from the website above
  #                            and locally read it specifying its path.
  #
  #   working.dir: string: path to where the ghcnd-inventory.txt file will be 
  #                        stored. If any path is defined it will be placed in
  #                        the current working directory. Note, if path.to.ghcnd.inventory 
  #                        defined, this argument becomes meaningless. 
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
  
  # if the argument path.to.ghcnd.inventory is not defined
  if (is.null(path.to.ghcnd.inventory))
  {
    # file targeted
    ghcnd.inventory <- "ghcnd-inventory.txt"
    
    # search for "ghcnd-inventory.txt" in the current working directory
    # get all files with .txt extenssion from the working directory
    file.names <- Sys.glob(file.path(working.dir, "*.txt"))
    
    # define the future or current path of the file ghcnd.inventory.txt
    path.to.ghcnd.inventory <- file.path(working.dir, ghcnd.inventory)
    
    if (!(ghcnd.inventory %in% basename(file.names)))
    {
      # download the file from NOAA's servers      
      download.file(url= "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt", 
                    destfile = path.to.ghcnd.inventory,  
                    method = 'auto')
    }
  } else {
    # in case of a defining path 
    # check the path for correctness 
    
    if (!file.exists(path.to.ghcnd.inventory))
      stop("The function could not find the file 'ghcnd.inventory'. Recheck the path.to.ghcnd.inventory argument.")
  }
  
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
  
  # if stations data frame is empty, there is no data available for these
  # arguments settings
  if (nrow(stations) == 0)
  {
    stop("No data available for these arguments settings.")
  }
  
  # subset the data based on weather station locations
  if (!is.null(stations.from))
  {
    # get the first to letters from the weather station ID
    s <- substr(stations$stationID, 1, 2)
    
    # get only the stations specified in the argument 
    stations <- stations[s %in% stations.from, ]
    
    if (nrow(stations) == 0)
    {
      stop("No data available for the location(s) required.")
    }
  }
  
  # subset the data based on which information needs to be retrieved 
  if (!is.null(data))
  {
    stations <- stations[stations$feature %in% data, ]
    
    if (nrow(stations) == 0)
    {
      stop("No data available for these/this kind of data.")
    }
    
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
  
  closest.stations <- droplevels(closest.stations)
  
  l <- list( years = c(start.year, end.year), data = data, starting.point = point,
             closest.stations = list(closest.stations[c("stationID", "lat", "lon", "startYear", "endYear", "distance.km")]))
  
}
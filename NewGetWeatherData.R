library(sp)
library(data.table)
library(R.utils)

id <- c(1,1,1,2,2,3,3,3,3)
X <- c("A", "B", "C", "C", "D", "A", "C", "D", "E")
df1 <- data.frame(id, X)


id <- c(1,2,3,4)
X1 <- c("33", "44", "55", "XX")
X2 <- c("66", "77", "88", "XX")
df2 <- data.frame(id, X1, X2)

merge(df1, df2, by="id")


##########################################################
##########################################################

f <- read.table("/Users/thalles/Desktop/openweather/CHM00050788.dly", fill=T)

##########################################################
##########################################################

nearest.stations <- GetNearestStations(lat=48.043333, lon=-74.140556,
                                       path.to.ghcnd.stations = NULL,
                                       working.dir = '/Users/thalles/Desktop/openweather', stations.from = c("CA"))



GetNearestStations <- function (lat, lon, radio = NULL,
                           path.to.ghcnd.stations = NULL,
                           working.dir = getwd(), stations.from = NULL, n = NULL)
{
  # Function designed to return the nearest weather stations from a given coordinate point. 
  # 
  # Args:
  #   lat: numeric: latitude coordinate in decimal degrees form. e.g. 48.043486
  #
  #   lon: numeric: lontirude coodinate in decimal defrees form. e.g. -74.140737
  #
  #   radio: numeric: if defined, it selects the closest stations to the point which
  #          fall inside a given radios. Note that if radios is defined, the parameter
  #          [n] becomes meaningless. 
  #
  #   path.to.ghcnd.stations: string: path to the 'ghcnd-stations.txt' file, which contains
  #                           all available weather stations. We encourage users 
  #                           to download this file directly from the NCDC's servers
  #                           through ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt.
  #                           In the case of nom-definition of argument, 
  #                           this function will firstly search for this file in the
  #                           currently working directory, if it does not find, 
  #                           it will directly download the file from the above link,
  #                           place, and read it from the current working directory. 
  #
  #   working.dir: string: path to where the ghcnd-stations.txt file will be 
  #                        stored. If any path is defined it will be placed in
  #                        the current working directory. Note, if path.to.ghcnd.inventory 
  #                        is properly defined, this argument becomes meaningless. 
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
  #   n: numeric: defines the number of closest stations to be output.
  
  # verify if the lat and lon parameters were decrared
  if (missing(lat) | missing(lon)) {
    stop("Please, especify a coordinate point through arguments lat and lon.")
  }
  
  # if the argument path.to.ghcnd.stations is not defined
  if (is.null(path.to.ghcnd.stations)) {
    # file targeted
    ghcnd.stations <- "ghcnd-stations.txt"

    # search for "ghcnd-stations.txt" in the current working directory
    # get all files with .txt extenssion from the working directory
    file.names <- Sys.glob(file.path(working.dir, "*.txt"))
    
    # define the future or current path of the file ghcnd.stations.txt
    path.to.ghcnd.stations <- file.path(working.dir, ghcnd.stations)
    
    if (!(ghcnd.stations %in% basename(file.names))) {
      print("Downloading the ghcnd-stations.txt file from 'ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt'")
      
      # download the file from NOAA's servers      
      download.file(url= "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt", 
                    destfile = path.to.ghcnd.stations,  
                    method = 'auto')
    }
  } else {
    # in case of a defining path 
    # check the path for correctness 
    if (!file.exists(path.to.ghcnd.stations))
      stop("The function could not find the file 'ghcnd.stations'. Recheck the path.to.ghcnd.stations argument.")
  }
  
  # function to remove leading and trailling whitespace
  trim <- function( x ) {
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
  }
  
  # get the file's number of lines to make the read.csv funcion more efficient
  lines <- trim(system(paste("wc -l", path.to.ghcnd.stations, sep=" "), intern = T))
  
  # regular expression to extract only the number from the output
  m <- regexpr("[0-9]*", lines, perl=TRUE) 
  lines <- as.numeric(regmatches(lines, m))
  
  # read the schema. It is provided in the readme.txt
  # in the ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ directory
  Schema <- read.table(text = 
    "ID           1-11   factor
    LATITUDE     13-20   numeric
    LONGITUDE    22-30   numeric
    ELEVATION    32-37   numeric
    STATE        39-40   'NULL'
    NAME         42-71   character
    GSNFLAG      73-75   'NULL'
    HCNFLAG      77-79   'NULL'
    WMOID        81-85   'NULL'", header = FALSE, stringsAsFactors = FALSE)
  
  # Split the second column and extract the upper part of the range. Then, use diff to find out the widths
  Widths <- as.numeric(sapply(strsplit(as.character(Schema$V2), "-"), `[`, 2))
  Widths <- c(Widths[1], diff(Widths))
  
  # Now, you can use read.fwf. The schema file also provides the column names for us.
  stations <- read.fwf("/Users/thalles/Desktop/openweather/ghcnd-stations.txt", widths = Widths, 
                  col.names = Schema$V1, strip.white = TRUE, comment.char = "",
                  nrows = lines, colClasses=Schema$V3)
  
  # get only the stations from the specified countries
  # subset the data based on weather station locations
  if (!is.null(stations.from)) {
    # get the first to letters from the weather station ID
    s <- substr(stations$ID, 1, 2)
    
    # get only the stations specified in the argument 
    stations <- stations[s %in% stations.from, ]
    
    if (nrow(stations) == 0) {
      stop("No data available for the location(s) required.")
    }
  }
  
  ############################
  # CALCULATING DISTANCE
  ############################
  
  # create a point (longitude first)
  point <- c(lon, lat)
  
  # get the coordinates from the stations
  coordinates <- stations[,c(2,3)]
  
  # invert the order, to put longitude first and then latitude
  coordinates <- coordinates[c("LONGITUDE","LATITUDE")]
  
  # compute the distance from the experimental point to each
  # weather station
  distance.km <- spDistsN1(as.matrix(coordinates), point, longlat=T)
  
  # add the distances to the data frame
  stations <- cbind(stations, distance.km)
  
  # sort the data frame by distance DESCENDEND in order to get
  # the closest weather stations
  stations <- stations[order(distance.km), ]
  
  # verify if the radio argument is set
  if(!is.null(radio)) {
    # get only the stations within the specified radio
    stations <- stations[stations$distance.km <= radio, ]
    
    # the radio parameter has priority over the n argument
    n = NULL 
  }
  
  if (!is.null(n)) {
    # return just the n nearest stations from a given point
    stations <- stations[1:n, ]
  }
  
  l <- list( starting.point = point,
             df = stations ) 
}


#######################################
#######################################


weather.data <- GetNSWeatherData (nearest.stations, path.to.weather.data = '/Users/thalles/Desktop/openweather/2014.csv', 
                           data.type = c("TMAX", "TMIN"))


GetNSWeatherData <- function(stations, path.to.weather.data = NULL, year = NULL,
                           data.type = c("TMAX", "TMIN"), working.dir=getwd(), n=10, strict=T )
{
  # This function outputs whether data from stations in a given year.
  # Args:
  #   stations: list object output by function GetNearestStations
  #
  #   path.to.weather.data: character string defining the local path to 'YYYY.csv'
  #                         file downloaded from ncdc.noaa's servers, where YYYY represents
  #                         the desired year. If not specified, the function will try
  #                         to find this file in the current working directory. If
  #                         it does not find it, it will download the file specified
  #                         by the 'year' argument, and place it in the workind 
  #                         directory.
  #
  #   year: numeric, it defined which year of weather dada will be download. 
  #         Notice, if 'path.to.weather.data' is proper defined, this argument
  #         becomes, meaningless. 
  #
  #   data.type: character vector containing the specific weather information to be retrieved
  #          possible values: TMIN, TMAX, PCPT. e.g. c("TMAX", "TMIN"), more information
  #          can be seen at ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt.
  #          
  
  # if the 'path.to.weather.data' is not defined, first try to find 
  # in the current working directory, if it is not in there, try to download
  # the file specified by the 'year' argument
#   if (is.null(path.to.weather.data)) {
#     
#     # check if the year argument is set
#     if (!is.null(year)) {
#       # defines the target file
#       targeted.file <- paste0(year, ".csv.gz")
#       
#       # search in the current workig directory for all files with .csv.gz extenssion
#       file.names <- Sys.glob(file.path(working.dir, "*.csv.gz"))
#       
#       # define the current or future path for the file YYYY.csv.gz
#       path.to.weather.data <- file.path(working.dir, targeted.file)
#       
#       # verify if the file is in the directory
#       if (!(targeted.file %in% basename(file.names))) # if it is not
#       {      
#         # if the file is not found, download it
#         download.file(url= paste0("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/", targeted.file), 
#                       destfile = path.to.weather.data,
#                       method='auto', mode = 'w')
#         
#       }
#     } else {
#       # if both arguments, 'path.to.weather.data' and 'year' are not defined,
#       # throws an error
#       stop("Please, provide a valid local path to a weather data file through 
#            the 'path.to.weather.data' argument, or provide a value for the 'year', 
#            argument.")
#     }
#   } 
  
  # function to remove leading and trailling whitespace
  trim <- function( x ) {
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
  }
  
  # get the file's number of lines to make the read.csv funcion more efficient
  lines <- trim(system(paste("wc -l", path.to.weather.data, sep=" "), intern = T))
  
  # regular expression to extract only the number from the output
  m <- regexpr("[0-9]*", lines, perl=TRUE) 
  lines <- as.numeric(regmatches(lines, m))

######################################################################
#   CODE MARKED AS DEPRECATED!!!  
#   print(paste("Reading" , path.to.weather.data, sep=""))
#   print("It might take a little while...")
#   
#  # read the file containing the weather data
#   weather.data <- read.csv(path.to.weather.data,
#                  header=F,
#                  col.names=c("ID", "DATA", "TYPE", "VALUE", "NULL", "NULL", "DEGREE", "NULL"),
#                  colClasses=c("factor", "factor", "factor", "numeric", "NULL", "NULL", "character", "NULL"),
#                  nrows = lines, comment.char = "")
#   
#   print("Done reading!")
######################################################################
  
  weather.data = fread(path.to.weather.data, sep="auto",
                     select=c(1,2,3,4,7),
                     colClasses=c("factor", "factor", "factor", "numeric", "NULL", "NULL", "character", "NULL"),
                     data.table=FALSE,
                     nrows = lines,
                     header=FALSE)
  
  names(weather.data) <- c("ID", "DATA", "TYPE", "VALUE", "DEGREE")

  # get only the desired stations
  weather.data <- weather.data[weather.data$ID %in% droplevels(stations$df$ID), ]
  
  if (nrow(weather.data) == 0) {
    stop("No data available for the location(s) required.")
  }
  
  # verify if the user wants specific data types to be fetched
  if ( !is.null(data.type) ) {
    # get only the data types required
    weather.data <- weather.data[weather.data$TYPE %in% data.type, ]
    
    if (nrow(weather.data) == 0)
    {
      stop(paste("No data available for these/this kind of data:", data.type), sep=" ")
    }
  }
  
  closest.stations = list()
  number.of.stations <- 0
  
  for( id in 1:nrow(stations$df) ) {
    station <- stations$df[id, ]
    
    t <- weather.data[weather.data$ID %in% station$ID, ]
    
    if (nrow(t) > 0) {
      t <- merge(t, station, by='ID')
      closest.stations <- append(closest.stations, list(t))
      number.of.stations = number.of.stations + 1
    }
    if (number.of.stations == n) {
      break
    }
  }
  
  closest.stations

  l <- list()
  if (isTRUE(strict))
  {
    # get the closest wheather stations from the point defined by [lat] and [lon] in which
    # each one of the stations do have the features defined by the parameter [data]
    for(station in closest.stations )
    {
      number.of.types <- nlevels(as.factor(station$TYPE))
      
      for (day in split(station, station$DATA)){
        # check if all stations have the features defined by the [data.type] parameter
        if (nrow(day) == length(data.type)) {
          l <- rbind(l, day) 
        } else {
          warning("Some stations do not have all the features.")
        }
      }
    }
  }
  split(l, l$ID)
}




# # get the date
# date <- sta1$DATA
# 
# # convert it to as.date
# date <- as.Date(date, "%Y%m%d")

# year <- format(date, "%Y")
# month <- format(date, "%m")
# day <- format(date, "%d")
# 
# sta1 <- cbind(sta1, year)
# sta1 <- cbind(sta1, month)
# sta1 <- cbind(sta1, day)

#split based on date
#day.list <- split(sta1, sta1$DATA)

new.list <- list()
for( sta1 in weather.data) {
  
  #sta1 <- as.data.frame(weather.data[[2]])
  date <- sta1$DATA
  
  # convert it to as.date
  date <- as.Date(date, "%Y%m%d")
  
  year <- format(date, "%Y")
  month <- format(date, "%m")
  day <- format(date, "%d")
  
  sta1 <- cbind(sta1, year)
  sta1 <- cbind(sta1, month)
  sta1 <- cbind(sta1, day)
  
  new.df <- data.frame()
  
  for( df in split(sta1, sta1$DATA)) {
    # get the data type which will be transformed into columns
    col <- as.vector(df$TYPE)
    
    # get the values
    val <- as.data.frame(t(df$VALUE))
    names(val) <- col
    
    df <- cbind(df, val)
    
    # remove the the TYPE and VALUE columns
    df$TYPE <- NULL
    df$VALUE <- NULL
    
    # remove the second row (it is useless now)
    if (nrow(df) > 1)
      df <- df[-c(2), ]
    
    # save it to the new data set
    new.df <- rbind(new.df, df)
  }
  
  new.list <- append(new.list, list(new.df))
}


f <- new.list[[2]]
for (g in split(f, f$month)) print(nrow(g))



# get the data type which will be transformed into columns
col <- c(t(df$TYPE))

# get the values
val <- as.data.frame(t(df[ , 4]))
names(val) <- col

df <- cbind(df, val)
df <- df[-c(2), ]
# # get only the stations from the specified countries
# # subset the data based on weather station locations
# if (!is.null(weather.data))
# {
#   # get the first to letters from the weather station ID
#   s <- substr(weather.data$ID, 1, 2)
#   
#   # get only the stations specified in the argument 
#   weather.data <- weather.data[s %in% stations.from, ]
# 
#   weather.data <- weather.data[weather.data$TYPE %in% data, ]
#   
#   if (nrow(weather.data) == 0)
#   {
#     stop("No data available for the location(s) required.")
#   }
# }
# 
# weather.data <- merge(weather.data, stations, by='ID')
# 
# ############################
# ############################
# 
# 
# # get the coordinates from the stations
# coordinates <- weather.data[,c(6,7)]
# 
# # invert the order, to put longitude first and then latitude
# coordinates <- coordinates[c("LONGITUDE","LATITUDE")]
# 
# # compute the distance from the experimental point to each
# # weather station
# distance.km <- spDistsN1(as.matrix(coordinates), point, longlat=T)
# 
# # add the distances to the data frame
# weather.data <- cbind(weather.data, distance.km)
# 
# # sort the data frame by distance DESCENDEND in order to get
# # the closest weather stations
# weather.data <- weather.data[order(distance.km), ]
# 
# weather.stations.list <- split(weather.data, weather.data$ID )
# 
# 
# # write.table(weather.data, sep=",", file="test.csv")

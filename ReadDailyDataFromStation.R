out <- GetNearestStationData(nearest.stations, element=c("TMAX", "TMIN"))


GetNearestStationData <- function(nearest.stations, element = NULL, working.dir = getwd()) {
  # get the data frame with the weather stations information
  df <- as.data.frame(nearest.stations$closest.stations)
  
  # list containing the weather data for each weather station in the
  # [nearest.stations] data frame
  # each entry of the list represents weather data for one of the weather stations
  output <- list()
  
  # loop through each line of the weather stations data frame
  for (row in 1:nrow(df)) {
    # get row by row
    station <- df[row, ]
    
    # firstly, get the station id and add the ".dly" extension 
    station.id <- station$stationID
    station.id <- paste(station.id, ".dly", sep="")
    
    # Secondly, get the start and end year desired
    start.year <- nearest.stations$years[1]
    end.year <- nearest.stations$years[2]
    
    # get the lat and long coordinates of each station
    LAT <- station$lat
    LON <- station$lon

    
    # read the schema. It is provided in the readme.txt
    # in the ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ directory
    Schema <- read.table(text = "
                         ID            1-11   character
                         YEAR         12-15   numeric
                         MONTH        16-17   numeric
                         ELEMENT      18-21   character
                         DAY1         22-26   numeric
                         MFLAG1       27-27   character
                         QFLAG1       28-28   character
                         SFLAG1       29-29   character
                         DAY2         30-34   numeric
                         MFLAG2       35-35   character
                         QFLAG2       36-36   character
                         SFLAG2       37-37   character
                         DAY3         38-42   numeric
                         MFLAG3       43-43   character
                         QFLAG3       44-44   character
                         SFLAG3       45-45   character
                         DAY4         46-50   numeric
                         MFLAG4       51-51   character
                         QFLAG4       52-52   character
                         SFLAG4       53-53   character
                         DAY5         54-58   numeric
                         MFLAG5       59-59   character
                         QFLAG5       60-60   character
                         SFLAG5       61-61   character
                         DAY6         62-66   numeric
                         MFLAG6       67-67   character
                         QFLAG6       68-68   character
                         SFLAG6       69-69   character
                         DAY7         70-74   numeric
                         MFLAG7       75-75   character
                         QFLAG7       76-76   character
                         SFLAG7       77-77   character
                         DAY8         78-82   numeric
                         MFLAG8       83-83   character
                         QFLAG8       84-84   character
                         SFLAG8       85-85   character
                         DAY9         86-90   numeric
                         MFLAG9       91-91   character
                         QFLAG9       92-92   character
                         SFLAG9       93-93   character
                         DAY10        94-98   numeric
                         MFLAG10       99-99   character
                         QFLAG10       100-100   character
                         SFLAG10       101-101   character
                         DAY11         102-106   numeric
                         MFLAG11       107-107   character
                         QFLAG11       108-108   character
                         SFLAG11       109-109   character
                         DAY12         110-114   numeric
                         MFLAG12       115-115   character
                         QFLAG12       116-116   character
                         SFLAG12       117-117   character
                         DAY13         118-122   numeric
                         MFLAG13       123-123   character
                         QFLAG13       124-124   character
                         SFLAG13       125-125   character
                         DAY14         126-130   numeric
                         MFLAG14       131-131   character
                         QFLAG14       132-132   character
                         SFLAG14       133-133   character
                         DAY15         134-138   numeric
                         MFLAG15       139-139   character
                         QFLAG15       140-140   character
                         SFLAG15       141-141   character
                         DAY16         142-146   numeric
                         MFLAG16       147-147   character
                         QFLAG16       148-148   character
                         SFLAG16       149-149   character
                         DAY17         150-154   numeric
                         MFLAG17       155-155   character
                         QFLAG17       156-156   character
                         SFLAG17       157-157   character
                         DAY18         158-162   numeric
                         MFLAG18       163-163   character
                         QFLAG18       164-164   character
                         SFLAG18       165-165   character
                         DAY19         166-170   numeric
                         MFLAG19       171-171   character
                         QFLAG19       172-172   character
                         SFLAG19       173-173   character
                         DAY20         174-178   numeric
                         MFLAG20       179-179   character
                         QFLAG20       180-180   character
                         SFLAG20       181-181   character
                         DAY21         182-186   numeric
                         MFLAG21       187-187   character
                         QFLAG21       188-188   character
                         SFLAG21       189-189   character
                         DAY22         190-194   numeric
                         MFLAG22       195-195   character
                         QFLAG22       196-196   character
                         SFLAG22       197-197   character
                         DAY23         198-202   numeric
                         MFLAG23       203-203   character
                         QFLAG23       204-204   character
                         SFLAG23       205-205   character
                         DAY24         206-210   numeric
                         MFLAG24       211-211   character
                         QFLAG24       212-212   character
                         SFLAG24       213-213   character
                         DAY25         214-218   numeric
                         MFLAG25       219-219   character
                         QFLAG25       220-220   character
                         SFLAG25       221-221   character
                         DAY26         222-226   numeric
                         MFLAG26       227-227   character
                         QFLAG26       228-228   character
                         SFLAG26       229-229   character
                         DAY27         230-234   numeric
                         MFLAG27       235-235   character
                         QFLAG27       236-236   character
                         SFLAG27       237-237   character
                         DAY28         238-242   numeric
                         MFLAG28       243-243   character
                         QFLAG28       244-244   character
                         SFLAG28       245-245   character
                         DAY29         246-250   numeric
                         MFLAG29       251-251   character
                         QFLAG29       252-252   character
                         SFLAG29       253-253   character
                         DAY30         254-258   numeric
                         MFLAG30       259-259   character
                         QFLAG30       260-260   character
                         SFLAG30       261-261   character
                         DAY31         262-266   numeric
                         MFLAG31       267-267   character
                         QFLAG31       268-268   character
                         SFLAG31       269-269   character",
                         header = FALSE, stringsAsFactors = FALSE)
    
    # Split the second column and extract the upper part of the range. Then, use diff to find out the widths
    Widths <- as.numeric(sapply(strsplit(as.character(Schema$V2), "-"), `[`, 2))
    Widths <- c(Widths[1], diff(Widths))
    
    # check if the weather file for the specified station id exists in the working dir
    # if the file does NOT exist, try to download it before reading
    if (!file.exists(file.path(working.dir, station.id))) {
      # download the file from NOAA's servers      
      download.file(url= paste("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/", station.id, sep=""), 
                    destfile = paste(working.dir, station.id, sep="/"),  
                    method = 'auto')
    }
    
    # Now, you can use read.fwf. The schema file also provides the column names for us.
    daily.data <- read.fwf(file.path(working.dir, station.id), widths = Widths, 
                           col.names = Schema$V1, strip.white = TRUE, comment.char = "",
                           colClasses=Schema$V3)
    
    
    # subset only the data for the specified year
    daily.data <- subset(daily.data, YEAR >= start.year & YEAR <= end.year)
    
    # if after the subset data.year is empty, it means that there is no data
    # for the year(s) defined, so skip this loop iteration
    if (nrow(daily.data) == 0) {
      warning(paste("There is no data for the station: ", station.id, sep=""))
      next 
    }
    
    # subset the data to only get the data with the required elements
    if (!is.null(element)) {
      
      daily.data <- daily.data[daily.data$ELEMENT %in% element,]
      
      if (nrow(daily.data) == 0)
      {
        stop("No data available for these/this kind of element.")
      }
    }
    
    # add the staion's coordinate to the data frame
    daily.data <- cbind(daily.data, LAT)
    daily.data <- cbind(daily.data, LON)
    
    # add the weather data to the output list
    output <- append(output, list(daily.data))
  }
  
  
  # returns the weather data for each weather station
  output
}

########################################################################
########################################################################

tmax <- GetMAXTemperature(out[[1]])
tmin <- GetMINTemperature(out[[3]])

data.max <- data.frame()

for(i in out) {
  row <- GetMAXTemperature(i)
  data.max <- rbind(data.max, row[1, ])
}

data.min <- data.frame()

for(i in out) {
  row <- GetMINTemperature(i)
  data.min <- rbind(data.min, row[1,])
}

GetMAXTemperature <- function(weather.data) {
  # Given a data frame containing the weather data from one specific station,
  # this function returns a data frame with only the Max temperature data element.
  # Function originally designed to work with the output from GetNearestStationData, 
  # which once outputs a list with data frame, only the data frame must be passed 
  # rather than the whole list.
  
  # subset the data frame to get only the TMAX data element
  weather.data <- weather.data[weather.data$ELEMENT %in% c('TMAX'), ]
  
  # creates a string vector containing the column names of the ID, MONTH and 
  # daily TEMPERATURES.
  # The [days] variable will hold strings like "ID", "lat", "lon", "MONTH", "DAY1", "DAY2", ... "DAY31"
  days <- c('ID', "LAT", "LON", 'MONTH')

  for( day in 1:31){
    days <- c(days, paste('DAY', day, sep=''))
  }
  
  # subset to get only the temperature and months columns
  weather.data <- weather.data[days]
}


GetMINTemperature <- function(weather.data) {
  # GetMinTemperature function works in the same way as GetMaxTemperature does.
  # but outputting only the MIN temperature
    
  # subset the data frame to get only the TMIN data element
  weather.data <- weather.data[weather.data$ELEMENT %in% c('TMIN'), ]
  
  # creates a string vector containing the column names to the MONTH and 
  # daily TEMPERATURES.
  # The [days] variable will hold strings like 'ID', "LAT", "LON", 'MONTH', "DAY1", "DAY2", ... "DAY31"
  days <- c('ID', "LAT", "LON", 'MONTH')
  for( day in 1:31){
    days <- c(days, paste('DAY', day, sep=''))
  }
  
  # subset to get only the temperature and months columns
  weather.data <- weather.data[days]
}

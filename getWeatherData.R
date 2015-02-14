#split -n 10000 --elide-empty-files --additional-suffix='.csv'

# creates a folder in the working directory where
# the file with weather data downloaded from the NOAA
# website will be places
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

getWeatherData <- function()
{
  
}

read.table(gzfile("/home/thalles/2000.csv.gz"))   

DownloadWeatherData(2000)

file.path <- '/home/thalles/Desktop/R\\ Weather\\ Testing\\ Code/2000/2000.csv'

# get the file's number of lines
lines <- system(paste("wc -l", file.path, sep=" "), intern = T)

# use regular expression to extract only the number from the output
m <- regexpr("[1-9]*", lines, perl=TRUE) 
lines <- as.numeric(regmatches(lines, m))

df <- read.csv("/home/thalles/Desktop/R Weather Testing Code/2000/2000.csv",
               header=F,
               colClasses=c("character", "factor", "character", "integer", "character", "character", "character", "character"),
               nrows = lines)


# top stations reffers to the nearest stations output by function getClosestStations
top.stations <- c('CA007075800', 'CA00707DBD4', 'CA007072816', 'CA007067658', 'CA007065639',
'CA007063560', 'CA007066685', 'CA007061288', 'CA007077570', 'CA007036855')

df <- df[df$V1 %in% top.stations, ]

# divides the data into chunks of weather stations
l <- split(df, df$V1 )
 
# get info of the one stations with data regarding a whole year
station1 <- l[[1]]

stations <- stations[order(distance.km), ]


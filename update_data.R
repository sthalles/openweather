path.to.ghcnd.stations <- '/Users/thalles/Desktop/openweather/ghcnd-stations.txt'

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
    STATE        39-40   character
    NAME         42-71   character
    GSNFLAG      73-75   character
    HCNFLAG      77-79   character
    WMOID        81-85   character", header = FALSE, stringsAsFactors = FALSE)

# Split the second column and extract the upper part of the range. Then, use diff to find out the widths
Widths <- as.numeric(sapply(strsplit(as.character(Schema$V2), "-"), `[`, 2))
Widths <- c(Widths[1], diff(Widths))

# Now, you can use read.fwf. The schema file also provides the column names for us.
stations <- read.fwf("/Users/thalles/Desktop/openweather/ghcnd-stations.txt", widths = Widths, 
                     col.names = Schema$V1, strip.white = TRUE, comment.char = "",
                     nrows = lines, colClasses=Schema$V3)

# save with R.Data extension
save(stations, file="ghcnd-stations.RData")

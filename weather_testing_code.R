# from a specific point expressed in coordinates degree, minute and second, 
# set a url from environment canada website, query the website's server and 
# get the results.

library(XML)

# 47°56'42.44"N
# 73°41'44.66"W

# coordinate in degree, minute, second format
latdeg <- 47
latmin <- 56
latsec <- 42

londeg <- 73
lonmin <- 41
lonsec <- 44

start.year <- 2015
end.year <- 2015

radio <- 200

# set the url for fetching weather stations within a certain radio.
# this url is from enviroment canada which although does not provide a API for 
# fetching information easely, it does have a search option where we input 
# a coordinate point and it outputs the neareste weather stations from that point
url <- paste("http://climate.weather.gc.ca/advanceSearch/searchHistoricDataStations_e.html?",
             "searchType=stnProx&timeframe=1&txtRadius=", radio, "&selCity=&selPark=&optProxType=",
             "custom&txtCentralLatDeg=", latdeg, "&txtCentralLatMin=", latmin, 
             "&txtCentralLatSec=", latsec, "&txtCentralLongDeg=", londeg, 
             "&txtCentralLongMin=", lonmin, "&txtCentralLongSec=", lonsec, 
             "&optLimit=yearRange&StartYear=", start.year, "&EndYear=1996",
             "&Year=1999&Month=2&Day=8&selRowPerPage=50&cmdProxSubmit=Search", sep="")

# fetch the html content from the environment canada website under the 
# url previously set
doc <- htmlTreeParse(url, useInternalNodes=T)

# get the name of the weather stations
stations <- xpathApply(doc, "//div[@class='span-2 row-end row-start margin-bottom-none station wordWrap stnWidth']", xmlValue)

# apparently, this class name is shared with some other information, so that,
# I need to filter the result out in order to get only the distances
distances <- xpathApply(doc, "//div[@class='span-1 row-end row-start margin-bottom-none day_mth_yr wordWrap']", xmlValue)
distances <- distances[ seq(1,length(distances),4) ]

# create a data set with the stations' names and distance from a stating point
df <- data.frame(stations = sapply(stations,c), distances = sapply(distances,c))

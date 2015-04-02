library(sp)
library(automap)

# testing point
x <- 46.043333
y <- -72.140556

nearest.stations <- GetNearestStations(lat=x, lon=y, start.year=2000, end.year=2000,
                                       path.to.ghcnd.inventory = "/Users/thalles/Desktop/openweather/ghcnd-inventory.txt",
                                       working.dir = '/Users/thalles/Desktop/openweather', n=20)


out <- GetNearestStationData(nearest.stations, element=c("TMAX", "TMIN"))


data.max <- data.frame()

for(i in out) {
  # get only the MAX temperature
  row <- GetMAXTemperature(i)
  
  # get only data from the month of January
  row <- subset(row, MONTH==1)
  
  data.max <- rbind(data.max, row[1, ])
}

data.min <- data.frame()

for(i in out) {
  # Get only the MIN temperature
  row <- GetMINTemperature(i)
  
  # get only data from the month of January
  row <- subset(row, MONTH==1)
  
  data.min <- rbind(data.min, row)
}

####################
# preparing the data
####################

# get only data for the first day
data.max <- data.max[c("ID", "LAT", "LON", "DIST", "MONTH", "DAY1")]
data.min <- data.min[c("ID", "LAT", "LON", "DIST", "MONTH", "DAY1")]

# identify the NA values
# replace each -9999 value per NA
data.max[data.max == -9999] <- NA
data.min[data.min == -9999] <- NA

# eliminate rows with NA
data.max <- data.max[complete.cases(data.max), ]
data.min <- data.min[complete.cases(data.min), ]

# get only the 8 closest
data.max <- data.max[1:8, ]
data.min <- data.min[1:8, ]

# convert the degree's values
data.max$DAY1 <- data.max$DAY1 / 10
data.min$DAY1 <- data.min$DAY1 / 10

####################
####################

# For each station on the data frame, predict its TMIN using the other
# seven stations as parameters
# TMIN station reasoning
data.min.pred <- data.frame()

# predict tmax for each of the stations coordinates
for (row in 1:nrow(data.min)) {
  # get the row from the data frame
  temp.df <- data.min
  
  # get the nth row of the df
  df.row <- temp.df[row, ]
  
  # remove that row
  temp.df <- temp.df[-c(row), ]
  
  # transform temp.df in SpatialPointsDataFrame
  coordinates(temp.df) = ~LAT + LON
  
  # create a point with the removed station coordinates
  new.data = SpatialPoints(cbind(LAT=df.row$LAT, LON=df.row$LON))
  
  # make the prediction
  kriging_result = autoKrige(DAY1~LAT+LON, temp.df, new.data)
  
  data.min.pred <- rbind(data.min.pred, cbind(df.row, as.data.frame(kriging_result$krige_output)))
}

# For each station on the data frame, predict its TMAX using the other
# seven stations as parameters
# TMAX station reasoning
data.max.pred <- data.frame()

# predict tmax for each of the stations coordinates
for (row in 1:nrow(data.max)) {
  # get the row from the data frame
  temp.df <- data.max
  
  # get the nth row of the df
  df.row <- temp.df[row, ]
  
  # remove that row
  temp.df <- temp.df[-c(row), ]
  
  # transform temp.df in SpatialPointsDataFrame
  coordinates(temp.df) = ~LAT + LON
  
  # create a point with the removed station coordinates
  new.data = SpatialPoints(cbind(LAT=df.row$LAT, LON=df.row$LON))
  
  # make the prediction
  kriging_result = autoKrige(DAY1~LAT+LON, temp.df, new.data)
  
  data.max.pred <- rbind(data.max.pred, cbind(df.row, as.data.frame(kriging_result$krige_output)))
}


#######
# Make the predictions using the testing point

data.max.spdf <- data.max
data.min.spdf <- data.min

# transform temp.df in SpatialPointsDataFrame
coordinates(data.max.spdf) = ~LAT + LON
coordinates(data.min.spdf) = ~LAT + LON

# create a point with the removed station coordinates
new.data = SpatialPoints(cbind(LAT=x, LON=y))

# make the prediction
kriging_result.Tmax = autoKrige(DAY1~LAT+LON, data.max.spdf, new.data)
kriging_result.Tmin = autoKrige(DAY1~LAT+LON, data.min.spdf, new.data)


##############
# ERROR Reasoning

# RMSE reasoning for MIN temperature predictions
# calculate the difference between the observed value and the predicted value
data.min.pred$ERROR <- data.min.pred$DAY1 - data.min.pred$var1.pred

data.min.pred$ERROR_2 <- data.min.pred$ERROR ** 2

RMSE <- sqrt( sum(data.min.pred$ERROR_2) / 8 )

TMIN.report <- list(TMIN_RMSE = RMSE)

# RMSE reasoning for MAX temperature predictions
# calculate the difference between the observed value and the predicted value
data.max.pred$ERROR <- data.max.pred$DAY1 - data.max.pred$var1.pred

data.max.pred$ERROR_2 <- data.max.pred$ERROR ** 2

RMSE <- sqrt( sum(data.max.pred$ERROR_2) / 8 )

TMAX.report <- list(TMAX_RMSE = RMSE)

# add the prediction MAX and MIN temperatures to the result list
TMAX.report <- append(TMAX.report, list(Pred_TMAX_Point = as.data.frame(kriging_result.Tmax$krige_output)))
TMIN.report <- append(TMIN.report, list(Pred_TMIN_Point = as.data.frame(kriging_result.Tmin$krige_output)))

# append the data frame with station's predictions to the list
TMAX.report <- append(TMAX.report, list(Stations_TMAX_Pred = data.max.pred))
TMIN.report <- append(TMIN.report, list(Stations_TMAX_Pred = data.max.pred))

# save the results.
save(TMAX.report, file="TMAX.report.RData")
save(TMIN.report, file="TMIN.report.RData")

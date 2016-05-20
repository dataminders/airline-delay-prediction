library(data.table)
library(lubridate)
library(Matrix)
library(xgboost)
library(caret)
library(readr)
library(stringr)

train <- fread("../Data/prepared_fe_train.csv")
test <- fread("../Data/prepared_fe_test.csv")
weather <- read_csv("../Data/weather.csv")

weather <- as.data.table(weather)
weather[, DAY := day(date)]
weather[, MONTH := month(date)]
weather[, YEAR := year(date)]
weather[, date_utc := NULL]
weather[, date := NULL]
weather[, time := NULL]
weather[, zip := NULL]
weather[, city := NULL]
weather[, time_cst := NULL]
setkey(weather, time_blk, airport_code, DAY, MONTH, YEAR) #6485048
weather <- unique(weather)
weather[is.na(precipitation_in), precipitation_in := 0]
weather[is.na(events), events := "NoAny"]
weather[is.na(temperature_f), temperature_f := mean(weather[['temperature_f']], na.rm = TRUE)]
weather[is.na(dew_point_f), dew_point_f := -999]
weather[is.na(humidity), humidity := -999]
weather[is.na(sea_level_pressure_in), sea_level_pressure_in := -999]
weather[is.na(visibility_mph), visibility_mph := -999]
weather[is.na(gust_speed_mph), gust_speed_mph := 0]
weather[is.na(wind_dir_degrees), wind_dir_degrees := 0]
weather[is.na(wind_direction), wind_direction := 'Calm']
weather[is.na(wind_speed_mph), wind_speed_mph := 'Calm']
weather[wind_speed_mph == 'Calm', wind_speed_mph := -999]
weather[is.na(conditions), conditions := 'Unknown']

setnames(weather, 'airport_code', 'ORIGIN')
setnames(weather, 'time_blk', 'DEP_TIME_BLK')
setnames(weather, 'DAY', 'DAY_OF_MONTH')
weather[, state := NULL]

setkey(train, ORIGIN, DEP_TIME_BLK, YEAR, MONTH, DAY_OF_MONTH)
setkey(test, ORIGIN, DEP_TIME_BLK, YEAR, MONTH, DAY_OF_MONTH)
setkey(weather, ORIGIN, DEP_TIME_BLK, YEAR, MONTH, DAY_OF_MONTH)

weather[, ORIGIN := str_sub(ORIGIN, 2)]

train <- merge(train, weather, all.x = TRUE, by = c('ORIGIN', 'DEP_TIME_BLK', 'YEAR', 'MONTH', 'DAY_OF_MONTH'),
               suffixes = c('', '_origin'))
test  <- merge(test, weather, all.x = TRUE, by = c('ORIGIN', 'DEP_TIME_BLK', 'YEAR', 'MONTH', 'DAY_OF_MONTH'),
               suffixes = c('', '_origin'))

setnames(weather, 'ORIGIN', 'DEST')
setnames(weather, 'DEP_TIME_BLK', 'ARR_TIME_BLK')

setkey(train, DEST, ARR_TIME_BLK, YEAR, MONTH, DAY_OF_MONTH)
setkey(test, DEST, ARR_TIME_BLK, YEAR, MONTH, DAY_OF_MONTH)
setkey(weather, DEST, ARR_TIME_BLK, YEAR, MONTH, DAY_OF_MONTH)

train <- merge(train, weather, all.x = TRUE, by = c('DEST', 'ARR_TIME_BLK', 'YEAR', 'MONTH', 'DAY_OF_MONTH'),
               suffixes = c('', '_dest'))
test  <- merge(test, weather, all.x = TRUE, by = c('DEST', 'ARR_TIME_BLK', 'YEAR', 'MONTH', 'DAY_OF_MONTH'),
               suffixes = c('', '_dest'))

# Merge phase
prepare <- function(dt, fe = c("MONTH", "ORIGIN")) {
  features_cont <- c('temperature_f', 'dew_point_f', 'humidity', 'sea_level_pressure_in', 'visibility_mph',
                     'gust_speed_mph', 'wind_dir_degrees', 'wind_dir_degrees', 'precipitation_in')
  sufs <- c('_dest', '')
  dt <- copy(dt)
  EVAL <- function(...)eval(parse(text=paste0(...)),envir=parent.frame(2))
  for (suf in sufs) {
      for (feature in paste0(features_cont, suf)) {
          EVAL("dt[is.na(", feature, ") | is.nan(", feature, "), ",
               feature, ":= mean(dt[['", feature,"']], na.rm = TRUE)]")
      }
  }
  max_c <- function(x){names(sort(table(x), decreasing = TRUE))[1]}
  feature_cat <- c('wind_direction', 'wind_speed_mph', 'events', 'conditions')
  for (suf in sufs) {
    for (feature in paste0(feature_cat, suf)) {
      EVAL("dt[is.na(", feature, ") | is.nan(", feature, "), ",
           feature, ":= max_c(dt[['", feature,"']])]")
    }
  }
  return(dt)
}

train <- prepare(train, NULL)
test <- prepare(test, NULL)

write.csv(train , "../Data/train_weather_prepared_full.csv", quote = FALSE, row.names = FALSE)
write.csv(test, "../Data/test_weather_prepared_full.csv", quote = FALSE, row.names = FALSE)




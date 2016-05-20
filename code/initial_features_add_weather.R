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

train <- merge(train, weather, all.x = TRUE, by = c('ORIGIN', 'DEP_TIME_BLK', 'YEAR', 'MONTH', 'DAY_OF_MONTH'))
test  <- merge(test, weather, all.x = TRUE, by = c('ORIGIN', 'DEP_TIME_BLK', 'YEAR', 'MONTH', 'DAY_OF_MONTH'))

# Merge phase
prepare <- function(dt, fe = c("MONTH", "ORIGIN")) {
    dt[is.na(temperature_f) | is.nan(temperature_f), temperature_f := mean(dt[['temperature_f']], na.rm = TRUE)]
    dt[is.na(dew_point_f) | is.nan(dew_point_f), dew_point_f := mean(dt[['dew_point_f']], na.rm = TRUE)]
    dt[is.na(humidity) | is.nan(humidity), humidity := mean(dt[['humidity']], na.rm = TRUE)]
    dt[is.na(sea_level_pressure_in) | is.nan(sea_level_pressure_in), sea_level_pressure_in := mean(dt[['sea_level_pressure_in']], na.rm = TRUE)]
    dt[is.na(visibility_mph) | is.nan(visibility_mph), visibility_mph := mean(dt[['visibility_mph']], na.rm = TRUE)]
    dt[is.na(gust_speed_mph) | is.nan(gust_speed_mph), gust_speed_mph := mean(dt[['gust_speed_mph']], na.rm = TRUE)]
    dt[is.na(precipitation_in) | is.nan(precipitation_in), precipitation_in := mean(dt[['precipitation_in']], na.rm = TRUE)]
    dt[is.na(wind_dir_degrees) | is.nan(wind_dir_degrees), wind_dir_degrees := mean(dt[['wind_dir_degrees']], na.rm = TRUE)]
    max_c <- function(x){names(sort(table(x), decreasing = TRUE))[1]}
    dt[is.na(wind_direction) | is.nan(wind_direction), wind_direction := max_c(dt[['wind_direction']])]
    dt[is.na(wind_speed_mph) | is.nan(wind_speed_mph), wind_speed_mph := max_c(dt[['wind_speed_mph']])]
    dt[is.na(events) | is.nan(events), events := max_c(dt[['events']])]
    dt[is.na(conditions) | is.nan(conditions), conditions := max_c(dt[['conditions']])]
    return(dt)
}

train <- prepare(train, NULL)
test <- prepare(test, NULL)

write.csv(train , "../Data/train_weather_prepared.csv", quote = FALSE, row.names = FALSE)
write.csv(test, "../Data/test_weather_prepared.csv", quote = FALSE, row.names = FALSE)




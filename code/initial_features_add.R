library(data.table)
library(Matrix)
library(xgboost)
library(caret)
train_2014_part1 <- fread("../Data/CAX_Train_2014_Jan_to_Jun.csv")
train_2014_part2 <- fread("../Data/CAX_Train_2014_Jul_to_Dec.csv")
train2015 <- fread("../Data/CAX_Train_2015.csv")
test <- fread("../Data/CAX_TestSet.csv")

# TODO: add feature: was previously any delay for this combination of dest org

train   <- rbindlist(list(train_2014_part1, train_2014_part2, train2015))
rm(train_2014_part1)
rm(train_2014_part2)
rm(train2015)

to_character <- c('FL_NUM', 'DEST_AIRPORT_ID', 'ORIGIN_AIRPORT_ID')

train[, (to_character) := lapply(.SD, as.character), .SDcols = c(to_character)]
test[, (to_character) := lapply(.SD, as.character), .SDcols = c(to_character)]

rm(to_character)

train[, num_arrival_all := .N, by = c('MONTH', 'DAY_OF_MONTH', 'YEAR', 'ARR_TIME_BLK')]
test[, num_arrival_all := .N, by = c('MONTH', 'DAY_OF_MONTH', 'YEAR', 'ARR_TIME_BLK')]
train[, num_dep_all := .N, by = c('MONTH', 'DAY_OF_MONTH', 'YEAR', 'DEP_TIME_BLK')]
test[, num_dep_all := .N, by = c('MONTH', 'DAY_OF_MONTH', 'YEAR', 'DEP_TIME_BLK')]
train[, num_arrival_dest := .N, by = c('MONTH', 'DAY_OF_MONTH', 'YEAR', 'ARR_TIME_BLK', 'DEST_AIRPORT_ID')]
test[, num_arrival_dest := .N, by = c('MONTH', 'DAY_OF_MONTH', 'YEAR', 'ARR_TIME_BLK', 'DEST_AIRPORT_ID')]
train[, num_dep_ori := .N, by = c('MONTH', 'DAY_OF_MONTH', 'YEAR', 'DEP_TIME_BLK', 'ORIGIN_AIRPORT_ID')]
test[, num_dep_ori := .N, by = c('MONTH', 'DAY_OF_MONTH', 'YEAR', 'DEP_TIME_BLK', 'ORIGIN_AIRPORT_ID')]
train[, ARR_HOUR := trunc(CRS_ARR_TIME/100)]
test[, ARR_HOUR := trunc(CRS_ARR_TIME/100)]
train[, DEP_HOUR := trunc(CRS_DEP_TIME/100)]
test[, DEP_HOUR := trunc(CRS_DEP_TIME/100)]
train[, NUM_FLIGHTS_TAIL := .N, by = c("TAIL_NUM", "YEAR", "MONTH", "DAY_OF_MONTH")]
test[, NUM_FLIGHTS_TAIL := .N, by = c("TAIL_NUM", "YEAR", "MONTH", "DAY_OF_MONTH")]
train[, same_state := 1*(ORIGIN_STATE_ABR == DEST_STATE_ABR)]
test[, same_state := 1*(ORIGIN_STATE_ABR == DEST_STATE_ABR)]

holidays <- c('2014-01-01', '2014-01-20', '2014-02-17', '2014-05-26',
              '2014-07-04', '2014-09-01', '2014-10-13', '2014-11-11',
              '2014-11-28', '2014-12-25',
              '2015-01-01', '2015-01-20', '2015-02-17', '2015-05-26',
              '2015-07-04', '2015-09-01', '2015-10-13', '2015-11-11',
              '2015-11-28', '2015-12-25')
              
holidayDates <- as.Date(holidays)

daysToHoliday <- function(year, month, day){
  currDate <- as.Date(paste(year,month,day,sep = '-'), format= "%Y-%m-%d")
  numDays <- as.numeric(min(abs(currDate-holidayDates)))
  return(numDays)
}

datesOfTrain <- unique(train[,c("YEAR", "MONTH", "DAY_OF_MONTH"),with=FALSE])
datesOfTest <- unique(test[,c("YEAR", "MONTH", "DAY_OF_MONTH"),with=FALSE])

dates <- rbind(datesOfTrain, datesOfTest)
rm(datesOfTrain)
rm(datesOfTest)

dates$HDAYS <- mapply(daysToHoliday, dates[['YEAR']], dates[['MONTH']], dates[['DAY_OF_MONTH']]) 

train <- merge(train, dates, by = c('YEAR', 'MONTH', 'DAY_OF_MONTH'))
test <- merge(test, dates, by = c('YEAR', 'MONTH', 'DAY_OF_MONTH'))

write.csv(train, "../Data/prepared_fe_train.csv", quote = FALSE, row.names = FALSE)
write.csv(test, "../Data/prepared_fe_test.csv", quote = FALSE, row.names = FALSE)

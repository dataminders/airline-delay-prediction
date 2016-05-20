library(data.table)
library(Matrix)
library(xgboost)
# Need to add weather.
# LB 0.69523
train <- fread('../Data/train_weather_prepared_full.csv')
response <- train[['ARR_DEL15']]
test <- fread('../Data/test_weather_prepared_full.csv')[, ARR_DEL15 := NULL][, ARR_DEL15 := NA]
id <- test[['id']]

n <- nrow(train)
total <- rbindlist(list(copy(train)[, id := NULL],copy(test)[, id := NULL]), use.names = TRUE)
rm(train)
rm(test)
gc()
total[, temperature_f := NULL]
total[, sea_level_pressure_in := NULL]
if (FALSE) {
  total[, num_arr_dest := .N, by = c('MONTH', 'DAY_OF_MONTH', 'YEAR', 'DEST_AIRPORT_ID')]
  total[, num_arr_origin := .N, by = c('MONTH', 'DAY_OF_MONTH', 'YEAR', 'ORIGIN_AIRPORT_ID')]
  total[, mean_arr_del_by_fl_num := mean(ARR_DEL15, na.rm = TRUE), by = 'FL_NUM']
  total[, mean_arr_del_by_tail_num := mean(ARR_DEL15, na.rm = TRUE), by = 'TAIL_NUM']
  total[, mean_arr_del_by_dest_num := mean(ARR_DEL15, na.rm = TRUE), by = 'DEST_AIRPORT_ID']
  total[, mean_arr_del_by_orig_num := mean(ARR_DEL15, na.rm = TRUE), by = 'ORIGIN_AIRPORT_ID']
  total[, mean_dest_orig_delay := mean(ARR_DEL15, na.rm = TRUE), by = c('DEST_AIRPORT_ID', 'ORIGIN_AIRPORT_ID')]
  
  total[is.na(mean_arr_del_by_fl_num),   mean_arr_del_by_fl_num := 0]
  total[is.na(mean_arr_del_by_tail_num), mean_arr_del_by_tail_num := 0]
  total[is.na(mean_arr_del_by_dest_num), mean_arr_del_by_dest_num := 0]
  total[is.na(mean_arr_del_by_orig_num), mean_arr_del_by_orig_num := 0]
  total[is.na(mean_dest_orig_delay), mean_dest_orig_delay := 0]
}
total[, is_fl_num_delay := (1 * sum(ARR_DEL15, na.rm = TRUE) > 0), by = 'FL_NUM']
total[, is_tail_num_delay := (1 * sum(ARR_DEL15, na.rm = TRUE) > 0), by = 'TAIL_NUM']
total[, is_dest_orig_delay := (1 * sum(ARR_DEL15, na.rm = TRUE) > 0), by = c('DEST_AIRPORT_ID', 'ORIGIN_AIRPORT_ID')]

total[is.na(ARR_DEL15), ARR_DEL15 := 0]
total[, c("FL_NUM", "TAIL_NUM") := NULL]

full <- sparse.model.matrix(ARR_DEL15 ~ ., data = total)

s    <- colSums(full)
fil  <- (s < 150)
full <- full[, -which(fil)]

train_sp <- full[1:n, ]
test_sp  <- full[(n+1):nrow(full), ]

rm(full)
gc()

dtest <- xgb.DMatrix(data = test_sp)

rm(test_sp)

dtraining <- xgb.DMatrix(data = train_sp[1:7000000, ], label = response[1:7000000])
dval <- xgb.DMatrix(data = train_sp[7000001:8496930, ], label = response[7000001:8496930])

dtrain <- xgb.DMatrix(data = train_sp, label = response)

rm(train_sp)
gc()

param <- list(objective           = "binary:logistic",
              booster             = "gbtree",
              eval_metric         = "auc",
              eta                 = 0.1,
              max_depth           = 10,
              colsample_bytree    = 0.4,
              gamma               = 4,
              subsample           = 0.5)


watchlist1 <- list(valid = dval, train = dtraining)
watchlist2 <- list(train = dtrain)

clf.valid <- xgb.train(params  = param,
                       data    = dtraining,
                       nrounds = 5000,
                       verbose = 1,
                       nfold   = 4,
                       watchlist = watchlist1,
                       early.stop.round = 40,
                       print.every.n = 10)

write.csv(predict(clf.valid, dval), "../cv/xgb_0013_weather.csv", quote = FALSE, row.names = FALSE)
print(clf.valid$bestScore)
print(clf.valid$bestInd)
clf <- xgb.train(params  = param,
                 data    = dtrain,
                 nrounds = clf.valid$bestInd + 15,
                 verbose = 1,
                 nfold   = 4,
                 watchlist = watchlist2,
                 print.every.n = 10)

preds <- predict(clf, dtest)
subs <- data.table(id = id, ARR_DEL15 = preds)
write.csv(subs, "../lb/xgboost_0013_weather.csv", quote = FALSE, row.names = FALSE)
library(data.table)
library(Matrix)
library(xgboost)
# [1] 0.693787 - this is CV
# [1] 547
train <- fread('../Data/prepared_fe_train.csv')
response <- train[['ARR_DEL15']]
test <- fread('../Data/prepared_fe_test.csv')[, ARR_DEL15 := NULL][, ARR_DEL15 := NA]
id <- test[['id']]

n <- nrow(train)
total <- rbindlist(list(copy(train)[, id := NULL],copy(test)[, id := NULL]), use.names = TRUE)
rm(train)
rm(test)
gc()

total[, num_arr_dest := .N, by = c('MONTH', 'DAY_OF_MONTH', 'YEAR', 'DEST_AIRPORT_ID')]
total[, num_arr_origin := .N, by = c('MONTH', 'DAY_OF_MONTH', 'YEAR', 'ORIGIN_AIRPORT_ID')]
total[, mean_arr_del_by_fl_num := mean(ARR_DEL15, na.rm = TRUE), by = 'FL_NUM']
total[, mean_arr_del_by_tail_num := mean(ARR_DEL15, na.rm = TRUE), by = 'TAIL_NUM']
total[, mean_arr_del_by_dest_num := mean(ARR_DEL15, na.rm = TRUE), by = 'DEST_AIRPORT_ID']
total[, mean_arr_del_by_orig_num := mean(ARR_DEL15, na.rm = TRUE), by = 'ORIGIN_AIRPORT_ID']
total[, mean_dest_orig_delay := mean(ARR_DEL15, na.rm = TRUE), by = c('DEST_AIRPORT_ID', 'ORIGIN_AIRPORT_ID')]
total[, mean_arr_del_by_dest_car := mean(ARR_DEL15, na.rm = TRUE), by = c('DEST_AIRPORT_ID', 'UNIQUE_CARRIER')]
total[, mean_arr_del_by_orig_car := mean(ARR_DEL15, na.rm = TRUE), by = c('ORIGIN_AIRPORT_ID', 'UNIQUE_CARRIER')]

total[is.na(mean_arr_del_by_fl_num), mean_arr_del_by_fl_num := 0]
total[is.na(mean_arr_del_by_tail_num), mean_arr_del_by_tail_num := 0]
total[is.na(mean_arr_del_by_dest_num), mean_arr_del_by_dest_num := 0]
total[is.na(mean_arr_del_by_orig_num), mean_arr_del_by_orig_num := 0]
total[is.na(mean_dest_orig_delay), mean_dest_orig_delay := 0]
total[is.na(mean_arr_del_by_dest_car), mean_arr_del_by_dest_car := 0]
total[is.na(mean_arr_del_by_orig_car), mean_arr_del_by_orig_car := 0]

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
dtrain <- xgb.DMatrix(data = train_sp, label = response)

rm(train_sp)
rm(test_sp)
gc()

param <- list(objective           = "binary:logistic", 
              booster             = "gbtree",
              eval_metric         = "auc",
              eta                 = 0.05,
              max_depth           = 15,
              colsample_bytree    = 0.4,
              gamma               = 6,
              subsample           = 0.8)

watchlist2 <- list(train = dtrain)

clf <- xgb.train(params  = param, 
                 data    = dtrain, 
                 nrounds = 600,
                 verbose = 1,
                 nfold   = 4,
                 watchlist = watchlist2,
                 print.every.n = 10)

preds <- predict(clf, dtest)
subs <- data.table(id = id, ARR_DEL15 = preds)
write.csv(subs, "../lb/xgboost_0005.csv", quote = FALSE, row.names = FALSE)
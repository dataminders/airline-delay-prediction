library(data.table)

read_sub <- function(filename) {
  dt <- fread(filename)
  dt[, ARR_DEL15 := rank(ARR_DEL15)/nrow(dt)]
  setkey(dt, id)
  dt <- unique(dt)[order(-id)]
  return(dt)
}

sub1 <- read_sub("../lb/submission_fm_ftrl_epoch1.csv")
sub2 <- read_sub("../lb/xgboost_0005.csv")
sub3 <- read_sub("../lb/xgboost_0009_weather.csv")
sub4 <- read_sub("../lb/xgboost_0010_weather.csv")
sub5 <- read_sub("../lb/xgboost_0013_weather.csv")

setnames(sub1, "ARR_DEL15", "ARR_DEL15_1")
setnames(sub2, "ARR_DEL15", "ARR_DEL15_2")
setnames(sub3, "ARR_DEL15", "ARR_DEL15_3")
setnames(sub4, "ARR_DEL15", "ARR_DEL15_4")
setnames(sub5, "ARR_DEL15", "ARR_DEL15_5")

train_same_moths <- fread("~/Workspace/kaggle/Airline-Delay/Data/CAX_Train_2014_Jul_to_Dec.csv", select = c('MONTH', 'ARR_DEL15'))[MONTH %in% c(7,8,9)][, .(cor_coef = mean(ARR_DEL15)), by = 'MONTH']
test <- fread("~/Workspace/kaggle/Airline-Delay/Data/CAX_TestSet.csv", select = c('id', 'MONTH'))
test_w <- merge(test, train_same_moths, by = 'MONTH')[, MONTH := NULL]

total <- Reduce(function(x,y) { merge(x,y,by='id')},
                list(sub1, sub2, sub3, sub4, sub5, test_w))

par <- c(0.21, 0.05, 0.33, 0.25, 0.16)

total[, new := par[1] * ARR_DEL15_1 + par[2] * ARR_DEL15_2 + 
        par[3] * ARR_DEL15_3 + par[4] * ARR_DEL15_4 + par[5] * ARR_DEL15_5]
total[, new := new * cor_coef]
total[, new := rank(new)/nrow(total)]
sub <- subset(total, select = c('id', 'new'))
setnames(sub, 'new', 'ARR_DEL15')
write.csv(sub, "~/Desktop/opt2.csv", quote = FALSE, row.names = FALSE)

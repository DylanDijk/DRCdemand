library(data.table)
#Formatting Data to Make easier to Cluster
traindf <- t(Irish_adj_train$indCons)
traindf <- data.frame(ID = rownames(traindf), traindf, row.names = NULL)

testdf <- t(Irish_adj_test$indCons)
testdf <- data.frame(ID = rownames(testdf), testdf, row.names = NULL)

extratrain <- Irish_adj_train$extra
extratest <- Irish_adj_test$extra

# Merge training and testing data frames with the cluster data frame
merged_train <- merge(traindf, hc4, by = "ID")[,-1]
merged_test <- merge(testdf, hc4, by = "ID")[,-1]

# Data Frame with each cluster with summed electricity demand
traindt <- data.table(merged_train)
sumtrain_dt <- traindt[, lapply(.SD, sum), by = cluster]

testdt <- data.table(merged_test)
sumtest_dt <- testdt[, lapply(.SD, sum), by = cluster]

# DOW to weekday and weekend
library(forcats)
extratest$dow <- fct_collapse(extratest$dow,
                              Weekend = c('Sun', 'Sat'),
                              Weekday = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri'))

extratrain$dow <- fct_collapse(extratrain$dow,
                               Weekend = c('Sun', 'Sat'),
                               Weekday = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri'))
load("data/Irish_adj.RData")
load("data/Irish_adj_train.RData")
load("data/Irish_adj_test.RData")

# loading the clusters
load(file = "data/hclust4.RData")
head(hc4)

# train and test split
# random 2 days from each month



# create dataset for each cluster
# for each time point
clust_agg_data_y = vector(mode = "list", length = nlevels(hc4$cluster))
names(clust_agg_data_y) = nlevels(hc4$cluster)
clust_agg_data_x = vector(mode = "list", length = nlevels(hc4$cluster))
names(clust_agg_data_x) = levels(hc4$cluster)
for(i in levels(hc4$cluster)){
  clust_agg_data_y[[i]] = vector(mode = "list", length = 48)
  names(clust_agg_data_y[[i]]) = 1:48
  clust_agg_data_x[[i]] = vector(mode = "list", length = 48)
  names(clust_agg_data_x[[i]]) = 1:48
  for(j in 1:48){
    print(c(i,j))
    clust_agg_data_y[[i]][[j]] = unname(rowSums(Irish_adj_train$indCons[,hc4$ID[hc4$cluster == i]]))[Irish_adj_train$extra$tod == (j-1)]
    clust_agg_data_x[[i]][[j]] = Irish_adj_train$extra[Irish_adj_train$extra$tod == (j-1),] 
  }
}

clust_agg_data_y_test = vector(mode = "list", length = nlevels(hc4$cluster))
names(clust_agg_data_y_test) = nlevels(hc4$cluster)
clust_agg_data_x_test = vector(mode = "list", length = nlevels(hc4$cluster))
names(clust_agg_data_x_test) = levels(hc4$cluster)
for(i in levels(hc4$cluster)){
  clust_agg_data_y_test[[i]] = vector(mode = "list", length = 48)
  names(clust_agg_data_y_test[[i]]) = 1:48
  clust_agg_data_x_test[[i]] = vector(mode = "list", length = 48)
  names(clust_agg_data_x_test[[i]]) = 1:48
  for(j in 1:48){
    print(c(i,j))
    clust_agg_data_y_test[[i]][[j]] = unname(rowSums(Irish_adj_test$indCons[,hc4$ID[hc4$cluster == i]]))[Irish_adj_test$extra$tod == (j-1)]
    clust_agg_data_x_test[[i]][[j]] = Irish_adj_test$extra[Irish_adj_test$extra$tod == (j-1),] 
  }
}





plot(y = clust_agg_data_y[[1]][[1]][1:50], type = "l", x = clust_agg_data_x[[1]][[1]]$dateTime[1:50])
clust_agg_data_x[[1]][[1]]
# for each time point we want to create a model
library(glmnet)
model1 = cv.glmnet(x = model.matrix(~., clust_agg_data_x[[1]][[1]][,c("toy", "dow", "temp")]),
                   y = clust_agg_data_y[[1]][[1]], alpha = 0)
plot(model1)

plot(predict(model1, model.matrix(~., clust_agg_data_x[[1]][[1]][,c("toy", "dow", "temp")])), type = "l")
points(clust_agg_data_y[[1]][[1]], type = "l", col = "red")

#### look at predictions on test set ####
# for cluster 1, need to make each of the 48 models
models_1 = vector(mode = "list", length = 48)
for(i in 1:48){
  models_1[[i]] = cv.glmnet(x = model.matrix(~., clust_agg_data_x[[1]][[i]][,c("toy", "dow", "temp")]),
                                     y = clust_agg_data_y[[1]][[i]], alpha = 0)
}

# predictions on test set
day1_test = c(sapply(clust_agg_data_y_test[[1]], `[`, 1))
plot(day1_test, type = "l")

day1_test_x = lapply(clust_agg_data_x_test[[1]], function(x) x[1,] )
day1_test_x = do.call(rbind, day1_test_x)

day1_pred = c(lapply(day1_test_x, 1, function(x) predict(model1, model.matrix(~., clust_agg_data_x[[1]][[1]][,c("toy", "dow", "temp")]))))
day1_pred = vector(length = 48, mode = "list")
for(i in 1:48){
  day1_pred[[i]] =  predict(models_1[[i]], model.matrix(~., day1_test_x[i,][,c("toy", "dow", "temp")]))
}

plot(day1_test, type = "l")
points(unlist(day1_pred), col = "red", type = "l")

# Then look at using smoothing
library(forecast)
sesf = holt(clust_agg_data_y[[1]][[1]])
plot(sesf$fitted[1:50], x = clust_agg_data_x[[1]][[1]]$dateTime[1:50], type = "l")
plot(sesf$fitted, x = clust_agg_data_x[[1]][[1]]$dateTime, type = "l")
points(y = clust_agg_data_y[[1]][[1]][1:50], type = "l", x = clust_agg_data_x[[1]][[1]]$dateTime[1:50])

ses()




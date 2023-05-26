load("data/Irish_adj.RData")

# loading the clusters
load(file = "data/hclust4.RData")
head(hc4)

# train and test split
# random 2 days from each month
library(dplyr)

# Extract month and day from the POSIXt column
df <- Irish_adj$extra %>%
  mutate(month = lubridate::month(dateTime),
         day = lubridate::day(dateTime)) %>%
         select(time, month, day)

# Group the data by month
grouped_df <- df %>%
  group_by(month)

sampled_days <- grouped_df %>%
  sample_n(2, replace = FALSE) %>%
  ungroup()

# Select all rows for the sampled days
df$month %in% sampled_days$month

sampled_df <- grouped_df %>%
  filter(day %in% sampled_days$day)

test_data = sampled_df$time
train_data = Irish_adj$extra$time[-test_data]

# create dataset for each cluster
# for each time point
clust_agg_data_y = vector(mode = "list", length = nlevels(hc4$cluster))
names(clust_agg_data_y) = nlevels(hc4$cluster)
clust_agg_data_x = vector(mode = "list", length = nlevels(hc4$cluster))
names(clust_agg_data_x) = nlevels(hc4$cluster)
for(i in levels(hc4$cluster)){
  clust_agg_data_y[[i]] = vector(mode = "list", length = 48)
  names(clust_agg_data_y[[i]]) = 1:48
  clust_agg_data_x[[i]] = vector(mode = "list", length = 48)
  names(clust_agg_data_x[[i]]) = 1:48
  for(j in 1:48){
    print(c(i,j))
    clust_agg_data_y[[i]][[j]] = unname(rowSums(Irish_adj$indCons[,hc4$ID[hc4$cluster == i]]))[Irish_adj$extra$tod == (j-1)]
    clust_agg_data_x[[i]][[j]] = Irish_adj$extra[Irish_adj$extra$tod == (j-1),] 
  }
}


plot(clust_agg_data_y[[1]][[47]], type = "l")
clust_agg_data_x[[1]][[1]]
# for each time point we want to create a model
library(glmnet)
model1 = cv.glmnet(x = model.matrix(~., clust_agg_data_x[[1]][[1]][,c("toy", "dow", "temp")]),
                   y = clust_agg_data_y[[1]][[1]], alpha = 0)
plot(model1)

plot(predict(model1, model.matrix(~., clust_agg_data_x[[1]][[1]][,c("toy", "dow", "temp")])), type = "l")
points(clust_agg_data_y[[1]][[1]], type = "l", col = "red")



clust_agg_data
View(Irish_adj$indCons)
View(Irish_adj$extra)




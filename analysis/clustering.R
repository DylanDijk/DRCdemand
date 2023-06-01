library(tidyverse)
library(DRCdemand)

# Load data
data("Irish_adj_train")
data("Irish_adj")

survey <- Irish_adj$survey

load("data/Irish_adj_train.RData")
indCons_train <- Irish_adj_train$indCons

# Replace meanDem with that calculated for training data only
survey <- survey %>%
  mutate(meanDem = colMeans(indCons_train))

extra <- Irish_adj_train$extra

# Calculate the weekly profile of each household (average demand at each tod at each dow)
weekly_profile <- DRCdemand::weekly_profile(Irish_adj_train)

surveydem <- dplyr::left_join(survey, weekly_profile)
dem <- dplyr::left_join(survey[,c(1,2)], weekly_profile)

### Hierarchical clustering using complete linkage
set.seed(1)
# Using all data, survey and weekly and mean
hc_all <- stats::hclust(DRCdemand::gowers_distance(surveydem[,-1]),
                        method = "complete")
# Using only survey data, nothing on weekly profile or mean demand
hc_fixed <- stats::hclust(DRCdemand::gowers_distance(survey[,-c(1,2)]),
                          method = "complete")
# Using only demand data, weekly and mean, no survey information
hc_dem <- stats::hclust(DRCdemand::gowers_distance(dem[,-1]),
                        method = "complete")

# Generate individual data frames with clustering IDs and large data frame
hcdf <- data.frame(ID = survey$ID)
varnames <- colnames(hcdf)
for(num_clust in 2^(0:9)){
  hcdf <- cbind(hcdf,
              as.factor(stats::cutree(hc_all, k = num_clust)),
              as.factor(stats::cutree(hc_fixed, k = num_clust)),
              as.factor(stats::cutree(hc_dem, k = num_clust)))
  varnames <- cbind(varnames,
                    paste0("cluster",num_clust,"all"),
                    paste0("cluster",num_clust,"fixed"),
                    paste0("cluster",num_clust,"dem"))
  hcdf_ind <- data.frame(ID = survey$ID,
                   cluster_all = as.factor(stats::cutree(hc_all, k = num_clust)),
                   cluster_fixed = as.factor(stats::cutree(hc_fixed, k = num_clust)),
                   cluster_dem = as.factor(stats::cutree(hc_dem, k = num_clust)))
  save(hcdf_ind, file = paste0("data/clusters/hclust",num_clust,".Rdata"))
}
colnames(hcdf) <- varnames
save(hcdf, file = "data/clusters/hclust.Rdata")

## Random clustering
# Generate individual data frames with clustering IDs and large data frame
set.seed(1)
rand <- data.frame(ID = survey$ID)
varnames <- colnames(rand)
for(num_clust in 2^(0:9)){
  cl <- sample(1:num_clust, size = nrow(survey), replace = TRUE)
  rand <- cbind(rand,
                as.factor(cl))
  varnames <- cbind(varnames,
                    paste0("cluster",num_clust,"all"))
  rand_ind <- data.frame(ID = survey$ID,
                         cluster_all = as.factor(cl))
  save(rand_ind, file = paste0("data/clusters/random",num_clust,".Rdata"))
}
colnames(rand) <- varnames
save(rand, file = "data/clusters/random.Rdata")

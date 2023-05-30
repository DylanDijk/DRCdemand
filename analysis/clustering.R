library(tidyverse)
library(DRCdemand)

# Load data
load("data/Irish_adj.RData")
survey <- Irish_adj$survey

load("data/Irish_adj_train.RData")
indCons_train <- Irish_adj_train$indCons
survey <- survey %>%
  mutate(meanDem.train = colMeans(indCons_train))

### Hierarchical clustering using complete linkage
set.seed(1)
hc_all <- stats::hclust(DRCdemand::gowers_distance(survey[,-c(1,2)]),
                        method="complete")
hc_fixed <- stats::hclust(DRCdemand::gowers_distance(survey[,-c(1,2,12)]),
                          method="complete")
hc_dem <- stats::hclust(DRCdemand::gowers_distance(survey[,12]),
                          method="complete")

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

## k-means clustering using meanDem.train
# Generate individual data frames with clustering IDs and large data frame
set.seed(1)
kmdf <- data.frame(ID = survey$ID)
varnames <- colnames(kmdf)
for(num_clust in 2^(0:9)){
  km <- survey %>%
    select(meanDem.train) %>%
    kmeans(centers = num_clust)
  kmdf <- cbind(kmdf,
                as.factor(km$cluster))
  varnames <- cbind(varnames,
                    paste0("cluster",num_clust,"dem"))
  kmdf_ind <- data.frame(ID = survey$ID,
                     cluster_all = as.factor(km$cluster))
  save(kmdf_ind, file = paste0("data/clusters/kmeans",num_clust,".Rdata"))
}
colnames(kmdf) <- varnames
save(kmdf, file = "data/clusters/kmeans.Rdata")

## Random clustering
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

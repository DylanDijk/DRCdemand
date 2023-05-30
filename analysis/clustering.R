library(tidyverse)
library(DRCdemand)

# Load data
load("data/Irish_adj.RData")
survey <- Irish_adj$survey

load("data/Irish_adj_train.RData")
indCons_train <- Irish_adj_train$indCons
survey <- survey %>%
  mutate(meanDem.train = colMeans(indCons_train))

# Hierarchical clustering using complete linkage
hc_all <- stats::hclust(DRCdemand::gowers_distance(survey[,-c(1,2)]),
                        method="complete")
hc_fixed <- stats::hclust(DRCdemand::gowers_distance(survey[,-c(1,2,12)]),
                          method="complete")

# Generate individual data frames with clustering IDs
for(num_clust in 2^(0:9)){
  hc <- data.frame(ID = survey$ID,
                   cluster_all = as.factor(cutree(hc_all, k = num_clust)),
                   cluster_fixed = as.factor(cutree(hc_fixed, k = num_clust)))
  save(hc, file = paste0("data/clusters/hclust",num_clust,".Rdata"))
}

# Generate large data frame with all clustering
hc <- data.frame(ID = survey$ID)
varnames <- colnames(hc)
for(num_clust in 2^(0:9)){
  hc <- cbind(hc,
              as.factor(stats::cutree(hc_all, k = num_clust)),
              as.factor(stats::cutree(hc_fixed, k = num_clust)))
  varnames <- cbind(varnames,
                    paste0("cluster",num_clust,"all"),
                    paste0("cluster",num_clust,"fixed"))
}
colnames(hc) <- varnames
save(hc, file = "data/clusters/hclust.Rdata")

library(tidyverse)

# Load data
load("data/Irish_adj.RData")
survey <- Irish_adj$survey
n <- nrow(survey)

# Calculate gowers distance
gowers_dist_mat <- matrix(0, nrow=n, ncol=n)
for(i in 1:n){
  gowers_dist_mat[,i] <- gower::gower_dist(survey[i,-1],survey[,-1])
}
gowers_dist <- as.dist(gowers_dist_mat)

# Hierarchical clustering using complete linkage
hc <- hclust(gowers_dist, method="complete")

# Generate data frame with clusering IDs
hc4 <- data.frame(ID = survey$ID,
                  cluster = as.factor(cutree(hc, k = 4)))

save(hc4, file = "data/hclust4.RData")

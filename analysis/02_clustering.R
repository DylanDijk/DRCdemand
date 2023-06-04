# Load data, packages and plot settings from previous analysis script
source("analysis/01_introduction.R")

survey <- Irish_adj$survey
indCons_train <- Irish_adj_train$indCons

# Replace meanDem with that calculated for training data only
# Scale and center cts variables
survey <- survey %>%
  mutate(meanDem = scale(colMeans(indCons_train))[,1],
         BUILT.YEAR = scale(BUILT.YEAR)[,1])

extra <- Irish_adj_train$extra

# Calculate the weekly profile of each household (average demand at each tod at each dow)
weekly_profile <- DRCdemand::weekly_profile(Irish_adj_train)

# Create tibbles of the different data sets for clustering
surveydem <- dplyr::left_join(survey, weekly_profile)
dem <- dplyr::left_join(survey[,c(1,2)], weekly_profile)

### Hierarchical clustering using complete linkage
set.seed(1)
library(dendextend) # For plotting

# Calculate distance using all data, survey and weekly and mean
gd_all <- DRCdemand::gowers_distance(surveydem[,-1])
# Distance plot (not included)
# factoextra::fviz_dist(gd_all, gradient = list(low = cbpal[1], mid = "white", high = cbpal[2]))

hc_all <- stats::hclust(gd_all,
                        method = "complete")
dhc_all <- as.dendrogram(hc_all)

# Plot clustering results
DRCdemand::plot_clusters(dhc_all, 4, cbpal)
DRCdemand::plot_clusters(dhc_all, 8, cbpal)

# Using only survey data, nothing on weekly profile or mean demand
gd_fixed <- DRCdemand::gowers_distance(survey[,-c(1,2)])

# Distance plot (not included)
# factoextra::fviz_dist(gd_fixed, gradient = list(low = cbpal[1], mid = "white", high = cbpal[2]))

hc_fixed <- stats::hclust(gd_fixed,
                          method = "complete")
dhc_fixed <- as.dendrogram(hc_fixed)

# Plot clusterings
DRCdemand::plot_clusters(dhc_fixed, 4, cbpal)
DRCdemand::plot_clusters(dhc_fixed, 8, cbpal)

# Using only demand data, weekly and mean, no survey information
gd_dem <- DRCdemand::gowers_distance(dem[,-1])

# Distance plot (not included)
# factoextra::fviz_dist(gd_all, gradient = list(low = cbpal[1], mid = "white", high = cbpal[2]))

hc_dem <- stats::hclust(gd_dem,
                        method = "complete")
dhc_dem <- as.dendrogram(hc_dem)

# Plot clusterings
DRCdemand::plot_clusters(dhc_dem, 4, cbpal)
DRCdemand::plot_clusters(dhc_dem, 8, cbpal)

# Generate individual data frames with clustering IDs and large data frame
# Save for later ease of use
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
#  hcdf_ind <- data.frame(ID = survey$ID,
#                   cluster_all = as.factor(stats::cutree(hc_all, k = num_clust)),
#                   cluster_fixed = as.factor(stats::cutree(hc_fixed, k = num_clust)),
#                   cluster_dem = as.factor(stats::cutree(hc_dem, k = num_clust)))
  #save(hcdf_ind, file = paste0("data/clusters/hclust",num_clust,".Rdata"))
}
colnames(hcdf) <- varnames
save(hcdf, file = "data/clusters/hclust.Rdata")

### Random clustering
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
#  rand_ind <- data.frame(ID = survey$ID,
#                         cluster_all = as.factor(cl))
#  save(rand_ind, file = paste0("data/clusters/random",num_clust,".Rdata"))
}
colnames(rand) <- varnames
save(rand, file = "data/clusters/rand.Rdata")

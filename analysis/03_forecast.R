library(parallel)
library(DRCdemand)

#For Hierarchical Clustering
load("data/clusters/hclust.Rdata")
load("data/Irish_adj_test.RData")
load("data/Irish_adj_train.RData")

# Define number of clusters we are interested in for analysis
num_clusters <- 8
# Calculate the relevant column index
index <- 3*log(num_clusters, base = 2) + 2

# Generate sobj for each type of clustering and store in list
sumobj8 <-list(
  all = DRCdemand::clustersum(Irish_adj_train, Irish_adj_test, hcdf, index),
  fixed = DRCdemand::clustersum(Irish_adj_train, Irish_adj_test, hcdf, index + 1),
  dem = DRCdemand::clustersum(Irish_adj_train, Irish_adj_test, hcdf, index + 2))

loop <- list('all', 'fixed', 'dem')
for (i in loop){

  # Choose amount of cores
  cl <- makeCluster(4)

  clusterEvalQ(cl, c(library(rstan), library(data.table), library(rstanarm), library(DRCdemand)))

  # Use sumobjs from clustersum() fitted in previous script

  clusterExport(cl, c('dataprocess', 'estimate','fitmodel', 'sumobj8', 'stan_code', 'parfit', 'i'))

  # 2 being chosen as number of cores to do MCMC chain in parallel
  # 1:8 as 8 clusters in sumobj8, change for sumobj4

  Models <- parLapply(cl, 1:8, function(x){DRCdemand::parfit(sumobj8[[i]],   stan_code, x, 2)})

  varname <- paste(i, 8, sep = '')

  assign(varname, Models)

  loc = paste('C:/Users/Admin/Documents/models/', varname, '.RData', sep = '')

  save(list = varname, file = loc)

  stopCluster(cl)

  #To Reduce Ram Usage
  rm(varname)

  rm(Models)
}


# Fitting for random clustering following same format as previous

cl <- makeCluster(4)

clusterEvalQ(cl, c(library(rstan), library(data.table), library(rstanarm), library(DRCdemand)))

clusterExport(cl, c('dataprocess','estimate','fitmodel', 'randsumobj8', 'stan_code', 'parfit'))

rand8 <- parLapply(cl, 1:8, function(x){DRCdemand::parfit(randsumobj8, stan_code, x, 2)})

# Save for later use
save(rand8, file = 'C:/Users/Admin/Documents/models/rand8.RData')

stopCluster(cl)

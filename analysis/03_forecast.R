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


#Creating Model Matrices for each Cluster and Training Data points.

# 8 cluster Heirachical ALL

load("~/models/all8.RData")

all8est <- list(NULL)
for (i in 1:8){
  estobj <- DRCdemand::estimate(sumobj8[['all']], all8,i)
  all8est[[i]] <- estobj
}

save(all8est, file = 'C:/Users/Admin/Documents/estobjs/all8est.RData')

gc()

# 8 cluster Heirachical DEM

load("~/sumobjs/sumobj8.Rdata")

load("~/models/dem8.RData")

dem8est <- list(NULL)
for (i in 1:8){
  estobj <- DRCdemand::estimate(sumobj8[['dem']], dem8,i)
  dem8est[[i]] <- estobj
}

save(dem8est, file = 'C:/Users/Admin/Documents/estobjs/dem8est.RData')

rm(dem8)

gc()


# 8 cluster Hierarchical FIXED

load("~/sumobjs/sumobj8.Rdata")

load("~/models/fixed8.RData")

fixed8est <- list(NULL)
for (i in 1:8){
  estobj <- DRCdemand::estimate(sumobj8[['fixed']], fixed8,i)
  fixed8est[[i]] <- estobj
}

save(fixed8est, file = 'C:/Users/Admin/Documents/estobjs/fixed8est.RData')

rm(fixed8)

gc()

# 8 Cluster Random.

load("~/sumobjs/randsumobj8.Rdata")

load("~/models/rand8.RData")

rand8est <- list(NULL)
for (i in 1:8){
  estobj <- DRCdemand::estimate(randsumobj8, rand8,i)
  rand8est[[i]] <- estobj
}

save(rand8est, file = 'C:/Users/Admin/Documents/estobjs/rand8est.RData')

rm(rand8)

gc()


# 4 cluster Hierarchical ALL

load("~/sumobjs/sumobj4.Rdata")

load("~/models/all4.RData")

all4est <- list(NULL)
for (i in 1:4){
  estobj <- DRCdemand::estimate(sumobj4[['all']], all4,i)
  all4est[[i]] <- estobj
}

save(all4est, file = 'C:/Users/Admin/Documents/estobjs/all4est.RData')

rm(hc4allmodels, all4)

gc()

# 4 cluster Heirachical DEM

load("~/sumobjs/sumobj4.Rdata")

load("~/models/dem4.RData")

dem4est <- list(NULL)
for (i in 1:4){
  estobj <- DRCdemand::estimate(sumobj4[['dem']], hc4dem,i)
  dem4est[[i]] <- estobj
}

save(dem4est, file = 'C:/Users/Admin/Documents/estobjs/dem4est.RData')

rm(hc4dem)

gc()

# 4 cluster Heirachical fixed

load("~/sumobjs/sumobj4.Rdata")

load("~/models/fixed4.RData")

fixed4est <- list(NULL)
for (i in 1:4){
  estobj <- DRCdemand::estimate(sumobj4[['fixed']], fixed4,i)
  fixed4est[[i]] <- estobj
}

save(fixed4est, file = 'C:/Users/Admin/Documents/estobjs/fixed4est.RData')

rm(fixed4)

gc()

# 4 Cluster Random.

load("~/sumobjs/randsumobj4.Rdata")

load("~/models/rand4.RData")

rand4est <- list(NULL)
for (i in 1:4){
  estobj <- DRCdemand::estimate(randsumobj4, rand4models,i)
  rand4est[[i]] <- estobj
}

save(rand4est, file = 'C:/Users/Admin/Documents/estobjs/rand4est.RData')

rm(rand4models)

gc()


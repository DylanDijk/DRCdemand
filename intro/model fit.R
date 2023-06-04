
library(parallel)
library(DRCdemand)
loop <- list('dem')

for (i in loop){
  
  cl <- makeCluster(4)
  
  clusterEvalQ(cl, c(library(rstan), library(data.table), library(rstanarm), library(DRCdemand)))
  
  clusterExport(cl, c('dataprocess', 'estimate','fitmodel', 'sumobj8', 'stan_code', 'parfit', 'i'))
  
  Models <- parLapply(cl, 1:8, function(x){DRCdemand::parfit(sumobj8[[i]],   stan_code, x, 2)})
  
  varname <- paste(i, 8, sep = '')
  
  assign(varname, Models)
  
  loc = paste('C:/Users/Admin/Documents/models/', varname, '.RData', sep = '')
  
  save(list = varname, file = loc)
  
  stopCluster(cl)
}


cl <- makeCluster(4)

clusterEvalQ(cl, c(library(rstan), library(data.table), library(rstanarm), library(DRCdemand)))

clusterExport(cl, c('dataprocess','estimate','fitmodel', 'sumobj4', 'stan_code', 'parfit'))


hc4demModels <- parLapply(cl, 1:4, function(x){DRCdemand::parfit(sumobj4[['dem']], stan_code, x, 2)})

save(hc4demModels, file = 'C:/Users/Admin/Documents/models/dem4.RData')

stopCluster(cl)



cl <- makeCluster(4)

clusterEvalQ(cl, c(library(rstan), library(data.table), library(rstanarm), library(DRCdemand)))

clusterExport(cl, c('dataprocess','estimate','fitmodel', 'randsumobj8', 'stan_code', 'parfit'))


rand8 <- parLapply(cl, 1:8, function(x){DRCdemand::parfit(randsumobj8, stan_code, x, 2)})

save(rand8, file = 'C:/Users/Admin/Documents/models/rand8.RData')

stopCluster(cl)



library(DRCdemand)

load("~/estobjs/all4est.RData")
load("~/estobjs/dem4est.RData")
load("~/estobjs/fixed4est.RData")
load("~/estobjs/rand4est.RData")

all4day <- totaldem(all4est, 4, 1)

dem4day <- totaldem(dem4est, 4, 1)

fix4day <- totaldem(fixed4est, 4, 1)

ran4day <- totaldem(rand4est, 4, 1)

RMSE4 <- data.frame(all4day$rmse, dem4day$rmse, fix4day$rmse, ran4day$rmse)


for (i in 2:24){
  all4day <- totaldem(all4est, 4, i)
  
  dem4day <- totaldem(dem4est, 4, i)
  
  fix4day <- totaldem(fixed4est, 4, i)
  
  ran4day <- totaldem(rand4est, 4, i)
  
  RMSE4 <- rbind(RMSE4, data.frame(all4day$rmse, dem4day$rmse, fix4day$rmse, ran4day$rmse))
  }

RMSE4 <- rbind(RMSE4, colMeans(RMSE4))

colnames(RMSE4) <- c('ALL', 'DEM', 'FIX', 'RAN')


#rownames(RMSE4[25,]) <- 'colmeans'

## For 8 Clusters

load("~/estobjs/all8est.RData")
load("~/estobjs/dem8est.RData")
load("~/estobjs/fixed8est.RData")
load("~/estobjs/rand8est.RData")

all8day <- totaldem(all8est, 8, 1)

dem8day <- totaldem(dem8est, 8, 1)

fix8day <- totaldem(fixed8est, 8, 1)

ran8day <- totaldem(rand8est, 8, 1)

RMSE8 <- data.frame(all8day$rmse, dem8day$rmse, fix8day$rmse, ran8day$rmse)


for (i in 2:24){
  all8day <- totaldem(all8est, 8, i)
  
  dem8day <- totaldem(dem8est, 8, i)
  
  fix8day <- totaldem(fixed8est, 8, i)
  
  ran8day <- totaldem(rand8est, 8, i)
  
  RMSE8 <- rbind(RMSE8, data.frame(all8day$rmse, dem8day$rmse, fix8day$rmse, ran8day$rmse))
}

RMSE8 <- rbind(RMSE8, colMeans(RMSE8))

colnames(RMSE8) <- c('ALL', 'DEM', 'FIX', 'RAN')

#rownames(RMSE8[25,]) <- 'colmeans'

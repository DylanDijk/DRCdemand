library(DRCdemand)
library(tidyverse)
ggplot2::theme_set(theme_bw())
cbpal <- RColorBrewer::brewer.pal(8, "Set2")[c(3,4,1,2,5,6,7,8)]
library(rstan)

load("~/models/all8.RData")

posterior <- extract(all8[[1]][[1]])


all8cl1betas <- data.frame(t(colMeans(posterior$beta)))

colnames(all8cl1betas) <- c(paste('beta', 0:47, sep = ''), 'TimeOfYear', 'Temp', 'Weekday', 'Weekend')
for (i in 2:48){
  posterior <- extract(all8[[1]][[i]])
  betas <- colMeans(posterior$beta)
  all8cl1betas <- rbind(all8cl1betas, colMeans(posterior$beta))
}

#load("~/models/all8cl1betas.RData")

# Convergence plots

ggplot() + geom_line(aes(x = 1:4000, y = posterior$beta[,51])) +
  labs(title = 'Convergence of Weekday on Cluster 1, Time Point 0 Model: All8 Clustering', x = 'Iteration', y = 'Beta[51]')+
  scale_color_manual(values = cbpal)

ggplot() + geom_line(aes(x = 1:4000, y = posterior$beta[,49])) +
  labs(title = 'Convergence of TimeOfYear on Cluster 1, Time Point 0 Model: All8 Clustering', x = 'Iteration', y = 'Beta[49]')+
  scale_color_manual(values = cbpal)

ggplot() + geom_line(aes(x = 1:4000, y = posterior$beta[,1])) +
  labs(title = 'Convergence of Time Point 0 on Cluster 1, Time Point 0 Model: All8 Clustering', x = 'Iteration', y = 'Beta[1]')+
  scale_color_manual(values = cbpal)

# Density plots for coefficients

stan_dens(all8[[1]][[1]], pars =  (dimnames(all8[[1]][[1]])[3])$parameters[1])

stan_dens(all8[[1]][[1]], pars =  (dimnames(all8[[1]][[1]])[3])$parameters[49])

stan_dens(all8[[1]][[1]], pars =  (dimnames(all8[[1]][[1]])[3])$parameters[51])

# Total demand plots

all8_day1 <- totaldem(all8est, 8, 1)
all8df <- all8_day1$df

ggplot(all8df) + geom_point(aes(x = Time, y = all8df[,2], colour = 'True')) + geom_line(aes(x = Time, y = Estimate, colour = 'Estimate')) +
  geom_line(aes(x = Time, y = Lower, colour = '95% CI')) + geom_line(aes(x = Time, y = Upper, colour = '95% CI')) +
  labs(title = 'Total Demand Estimate for all8 Clustering for Testing Day 1', y = 'Total Demand', x = 'Time Point') +
  scale_color_manual(values = cbpal)


all4_day1 <- totaldem(all4est, 4, 1)
all4df <- all4_day1$df

ggplot(all4df) + geom_point(aes(x = Time, y = all4df[,2], colour = 'True')) + geom_line(aes(x = Time, y = Estimate, colour = 'Estimate')) +
  geom_line(aes(x = Time, y = Lower, colour = '95% CI')) + geom_line(aes(x = Time, y = Upper, colour = '95% CI')) +
  labs(title = 'Total Demand Estimate for all4 Clustering for Testing Day 1', y = 'Total Demand', x = 'Time Point') +
  scale_color_manual(values = cbpal)

fix4_day1 <- totaldem(fixed4est, 4, 1)
fix4df <- fix4_day1$df

ggplot(fix4df) + geom_point(aes(x = Time, y = fix4df[,2], colour = 'True')) + geom_line(aes(x = Time, y = Estimate, colour = 'Estimate')) +
  geom_line(aes(x = Time, y = Lower, colour = '95% CI')) + geom_line(aes(x = Time, y = Upper, colour = '95% CI')) +
  labs(title = 'Total Demand Estimate for fixed4 Clustering for Testing Day 1', y = 'Total Demand', x = 'Time Point') +
  scale_color_manual(values = cbpal)

fix8_day1 <- totaldem(fixed8est, 4, 1)
fix8df <- fix8_day1$df

ggplot(fix8df) + geom_point(aes(x = Time, y = fix8df[,2], colour = 'True')) + geom_line(aes(x = Time, y = Estimate, colour = 'Estimate')) +
  geom_line(aes(x = Time, y = Lower, colour = '95% CI')) + geom_line(aes(x = Time, y = Upper, colour = '95% CI')) +
  labs(title = 'Total Demand Estimate for fixed8 Clustering for Testing Day 1', y = 'Total Demand', x = 'Time Point')+
  scale_color_manual(values = cbpal)

rand8_day1 <- totaldem(rand8est, 8, 1)
rand8df <- rand8_day1$df

ggplot(rand8df) + geom_point(aes(x = Time, y = rand8df[,2], colour = 'True')) + geom_line(aes(x = Time, y = Estimate, colour = 'Estimate')) +
  geom_line(aes(x = Time, y = Lower, colour = '95% CI')) + geom_line(aes(x = Time, y = Upper, colour = '95% CI')) +
  labs(title = 'Total Demand Estimate for rand8 Clustering for Testing Day 1', y = 'Total Demand', x = 'Time Point')+
  scale_color_manual(values = cbpal)

# RMSE Investigations

load("data/estobjs/all4est.RData")
load("data/estobjs/dem4est.RData")
load("data/estobjs/fixed4est.RData")
load("data/estobjs/rand4est.RData")

all4day <- DRCdemand::totaldem(all4est, 4, 1)

dem4day <- DRCdemand::totaldem(dem4est, 4, 1)

fix4day <- DRCdemand::totaldem(fixed4est, 4, 1)

ran4day <- DRCdemand::totaldem(rand4est, 4, 1)

RMSE4 <- data.frame(all4day$rmse, dem4day$rmse, fix4day$rmse, ran4day$rmse)


for (i in 2:24){
  all4day <- DRCdemand::totaldem(all4est, 4, i)

  dem4day <- DRCdemand::totaldem(dem4est, 4, i)

  fix4day <- DRCdemand::totaldem(fixed4est, 4, i)

  ran4day <- DRCdemand::totaldem(rand4est, 4, i)

  RMSE4 <- rbind(RMSE4, data.frame(all4day$rmse, dem4day$rmse, fix4day$rmse, ran4day$rmse))
  }

RMSE4 <- rbind(RMSE4, colMeans(RMSE4))

colnames(RMSE4) <- c('ALL', 'DEM', 'FIX', 'RAN')

save(RMSE4, file = "data/RMSE4.RData")

#rownames(RMSE4[25,]) <- 'colmeans'

## For 8 Clusters

load("data/estobjs/all8est.RData")
load("data/estobjs/dem8est.RData")
load("data/estobjs/fixed8est.RData")
load("data/estobjs/rand8est.RData")

all8day <- DRCdemand::totaldem(all8est, 8, 1)

dem8day <- DRCdemand::totaldem(dem8est, 8, 1)

fix8day <- DRCdemand::totaldem(fixed8est, 8, 1)

ran8day <- DRCdemand::totaldem(rand8est, 8, 1)

RMSE8 <- data.frame(all8day$rmse, dem8day$rmse, fix8day$rmse, ran8day$rmse)


for (i in 2:24){
  all8day <- DRCdemand::totaldem(all8est, 8, i)

  dem8day <- DRCdemand::totaldem(dem8est, 8, i)

  fix8day <- DRCdemand::totaldem(fixed8est, 8, i)

  ran8day <- DRCdemand::totaldem(rand8est, 8, i)

  RMSE8 <- rbind(RMSE8, data.frame(all8day$rmse, dem8day$rmse, fix8day$rmse, ran8day$rmse))
}

RMSE8 <- rbind(RMSE8, colMeans(RMSE8))

colnames(RMSE8) <- c('ALL', 'DEM', 'FIX', 'RAN')

save(RMSE8, file = "data/RMSE8.RData")

#rownames(RMSE8[25,]) <- 'colmeans'

# Density plots for RMSE
plt_data4 <- as_tibble(RMSE4[-25,]) %>%
  mutate(Day = 1:24,
         Clusters = factor(rep("4 Clusters",24), levels = c("4 Clusters","8 Clusters"))) %>%
  pivot_longer(cols = c(ALL, DEM, FIX, RAN))

plt_data8 <- as_tibble(RMSE8[-25,]) %>%
  mutate(Day = 1:24,
         Clusters = factor(rep("8 Clusters",24), levels = c("4 Clusters","8 Clusters"))) %>%
  pivot_longer(cols = c(ALL, DEM, FIX, RAN))

plt_data <- rbind(plt_data4, plt_data8)

ggplot(plt_data) +
  aes(x = value, colour = name) +
  facet_grid(Clusters ~ name) +
  geom_density(aes(fill = name), alpha=0.6) +
  scale_color_manual(values = cbpal) +
  scale_fill_manual(values = cbpal)+
  labs(x = "RMSE", y = "Density", colour = "Clustering",
    fill = "Clustering")

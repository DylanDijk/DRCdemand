load(file = "data/Irish_adj.RData")

library(kernlab)

data_y = Irish_adj$indCons[,1]
plot(data_y[1:1000], type = "l")
data_x = Irish_adj$extra[,c("toy", "dow", "tod", "temp")]

plot(x = data_x$toy, y = data_y, type = "l")


gram_mat = function(data_x = data_x, psi = 1){
  rbf = rbfdot(sigma=psi)
  W = kernelMatrix(rbf, as.matrix(data_x))
  return(W)
}



k_n  = function(x, data_x, psi){
  
  n = length(data_x)
  k_n = vector(length = n)
  
  rbf = rbfdot(sigma=psi)
  
  for(i in 1:n){
    k_n[i] = rbf(x, data_x[i])
  }
  
  return(k_n)
}



log_likeli = function(params = c(1,1), data_x = data_x, data_y = data_y){
  
  ss = gram_mat(data_x, psi = params[2]) + params[1] * diag(length(data_x))
  
  1/2 * (  log(det(ss))  + data_y %*% solve(ss) %*% data_y )
} 


optim(c(2,1), log_likeli)









```{r}
posteri_mean = function(x, data_x, data_y, psi, lambda){
  k_n(x, data_x, psi) %*% (solve((gram_mat(data_x, psi) + lambda * diag(length(data_x))))) %*% data_y
}
```

```{r}
posteri_kernel = function(x,y, psi, lambda){
  rbf = rbfdot(sigma=psi)
  rbf(x,y) -  k_n(x, data_x, psi) %*% (solve((gram_mat(data_x, psi) + lambda * diag(length(data_x))))) %*%  k_n(y, data_x, psi)
}
```

### Confidence region

```{r}
conf_up = function(x, data_x, data_y, psi, lambda){
  posteri_mean(x, data_x, data_y, psi, lambda) + (1 - pnorm(q = -0.025)) * sqrt(posteri_kernel(x,x,psi, lambda))
}

conf_low = function(x, data_x, data_y, psi, lambda){
  posteri_mean(x, data_x, data_y, psi, lambda) - (1 - pnorm(q = -0.025)) * sqrt(posteri_kernel(x,x,psi, lambda))
}
```

```{r}
data_x = boneData_male$age
```

```{r}
x = seq(min(boneData_male$age), max(boneData_male$age), 0.1)
y_up = vector(length = length(x))
y_low = vector(length = length(x))

for(i in 1:length(x)){
  y_up[i] = conf_up(x = x[i], data_x = boneData_male$age, data_y = boneData_male$spnbmd, psi = 1.1, lambda = 0.02)
  y_low[i] = conf_low(x = x[i], data_x = boneData_male$age, data_y = boneData_male$spnbmd, psi = 1.1, lambda = 0.02)
}
```

```{r}
x = seq(min(boneData_male$age), max(boneData_male$age), 0.1)
y = vector(length = length(x))

for(i in 1:length(x)){
  y[i] = posteri_mean(x = x[i], data_x = boneData_male$age, data_y = boneData_male$spnbmd, psi = 1.1, lambda = 0.02)
}

plot(x = boneData_male$age, y = boneData_male$spnbmd, col = "red", cex = 0.5)
lines(x[order(x)] , y[order(x)])
lines(x[order(x)] , y_up[order(x)], col = "blue")
lines(x[order(x)] , y_low[order(x)], col = "blue")
```



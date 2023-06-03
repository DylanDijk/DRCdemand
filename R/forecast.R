#' Summing Data based on Clustering Data set
#'
#' @param training Irish_Training_Data
#' @param testing Irish_Testing_Data
#' @param clustdat Clustering Data (with ID column in first position)
#' @param clustcol Column of Clusters
#'
#' @return Training and Testing Data sets with Total Energy Consumption for Each Cluster, and split Extra Info into Training and Testing
#' @export
#'
clustersum <- function(training, testing, clustdat, clustcol){
  # Check if the input is a list
  if (!is.list(training) | !is.list(testing)) {
    stop("Input is not a list.")
  }

  # Check if the input list has at least length two
  if (length(training) < 2 | length(testing) < 2) {
    stop("Input list does not have at least length two.")
  }

  # Check if the 'extra' data frame exists in the input list
  if ((!("extra" %in% names(training)) || !is.data.frame(training$extra)) | (!("extra" %in% names(testing)) || !is.data.frame(testing$extra))) {
    stop("Input list does not contain the 'extra' data frame.")
  }

  # Check if the 'indCons' data frame exists in the input list
  if ((!("indCons" %in% names(training)) || !is.data.frame(training$indCons)) | (!("indCons" %in% names(testing)) || !is.data.frame(testing$indCons))) {
    stop("Input list does not contain the 'indCons' data frame.")
  }
  traindf <- t(training$indCons)
  traindf <- data.frame(ID = rownames(traindf), traindf, row.names = NULL)

  testdf <- t(testing$indCons)
  testdf <- data.frame(ID = rownames(testdf), testdf, row.names = NULL)

  extratrain <- training$extra
  extratest <- testing$extra

  clustdf <- as.data.frame(cbind(clustdat[,1], clustdat[,clustcol]))

  colnames(clustdf) <- c('ID', 'cluster')

  # Merge training and testing data frames with the cluster data frame
  merged_train <- merge(traindf, clustdf , by = "ID")[,-1]
  merged_test <- merge(testdf, clustdf, by = "ID")[,-1]

  # Data Frame with each cluster with summed electricity demand
  traindt <- data.table::data.table(merged_train)
  sumtrain_dt <- traindt[, lapply(.SD, sum), by = 'cluster']

  testdt <- data.table::data.table(merged_test)
  sumtest_dt <- testdt[, lapply(.SD, sum), by = 'cluster']

  extratest$dow <- forcats::fct_collapse(extratest$dow,
                                Weekend = c('Sun', 'Sat'),
                                Weekday = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri'))

  extratrain$dow <- forcats::fct_collapse(extratrain$dow,
                                 Weekend = c('Sun', 'Sat'),
                                 Weekday = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri'))

  sls <- list(sumtrain = sumtrain_dt, sumtest = sumtest_dt, extrain = extratrain, extest = extratest)
  class(sls) = 'sumobj'

  return(sls)
}

#' Predictor and Model Matrix creator
#'
#' @param sumobj Output of clustersum function
#' @param cluster Cluster to Model over
#' @param time  Time point to Model over
#'
#' @return Predictor, Model Matrix, and Column Means. Class 'dpobj'.
#' @export
#'
dataprocess <- function(sumobj, cluster, time){

  sumdata <- sumobj$sumtrain

  extra <- sumobj$extrain

  one <- data.table::data.table(cbind(t(sumdata[cluster,-1]), extra$tod))

  one <- split(one$V1, as.factor(one$V2))

  one <- data.table::as.data.table(one)[-326,]

  ex <- extra[extra$tod == as.integer(time),]

  ex <- ex[-326,]

  dum <- stats::model.matrix(~ ex$dow - 1)[-1,]

  tX <- rbind(matrix(rep(0, ncol(one)), nrow = 1), as.matrix(one))

  yat <- (as.integer(time) +1)

  tY <- c(unlist(as.vector(one[,..yat])), 0)

  X <- cbind(tY, tX)[c(-1, -nrow(tX)),]

  X <- cbind(X, ex$toy[-1], ex$temp[-1])

  means <- colMeans(X)

  X <- scale(X, center = TRUE, scale = FALSE)

  X <- cbind(X, dum)

  Y <- X[,1]

  X <- X[,-1]

  slr <- list(modelM = X, response = Y, colmean = means)

  class(slr) <- 'dpobj'
  return(slr)
}

#' Fits Model using Stan
#'
#' @param dpobj 'dpobj' from dataprocess()
#' @param stancode Stan code used to fit model.
#'
#' @return Model Fit
#' @export
#'
fitmodel <- function(dpobj, stancode){

  # Data for Stan
  stan_data <- list(
    N = length(dpobj$response),
    K = ncol(dpobj$modelM),
    x = dpobj$modelM,
    y = dpobj$response
  )

  # Compile and fit the model
  fit <- rstan::stan(model_code = stancode, data = stan_data, iter = 2000, chains = 4)

  return(fit)
}

#' Create ModelMatrix for Testing Data set
#'
#' @param sumobj 'sumobj' from clustersum()
#' @param cluster cluster which the model is over
#' @param time time point which the model is over
#'
#' @return Model Matrix and True Testing Values
#' @export
#'
modelmTest <- function(sumobj, cluster, time){

  sumtrain <- sumobj$sumtrain
  sumtest <- sumobj$sumtest
  extratrain <- sumobj$extrain
  extratest <- sumobj$extest

  colmeanstrain <- dataprocess(sumobj, cluster, time)$colmean

  cl <- as.character(cluster)

  sr1 <- sumtrain[sumtrain[[1]] == cl]

  sr2 <- sumtest[sumtest[[1]] == cl]

  #Add yday on Training Data set
  train1 <- data.table::data.table(cbind(t(sr1[,-1]), extratrain$tod))

  train1 <- split(train1$V1, as.factor(train1$V2))

  train1 <- data.table::as.data.table(train1)

  datetrain1 <- cbind(train1, lubridate::yday(extratrain$dateTime)[1:length(extratrain$dateTime) %% 48 == 1])

  test1 <- data.table::data.table(cbind(t(sr2[,-1]), extratest$tod))

  test1 <- split(test1$V1, as.factor(test1$V2))

  test1 <- data.table::as.data.table(test1)

  datetest1 <- cbind(test1, data.table::yday(extratest$dateTime)[1:length(extratest$dateTime) %% 48 == 1])

  #Create predictor matrix for testing data set
  modelTest <- matrix(ncol = 49)
  for (i in 1:nrow(datetest1)){
    day <- datetest1$V2[i]
    rw <- datetrain1[datetrain1$V2 == (day-1), ]
    if (nrow(rw) == 0){
      rw <- datetest1[datetest1$V2 == (day-1), ]
    }
    modelTest <- rbind(modelTest, rw, use.names = FALSE)
  }

  modelTest <- modelTest[-1, -49]

  exTest <- extratest[extratest$tod == as.integer(time),]

  dumTest <- stats::model.matrix(~ exTest$dow - 1)

  modelTest <- cbind(modelTest, exTest$toy, exTest$temp)

  modelTest <- t(t(modelTest) - colmeanstrain[-1])

  modelTest <- as.matrix(cbind(modelTest, dumTest))

  slr <- list(predM = modelTest, Testing = test1)

  class(slr) = 'predobj'

  return(slr)
}

#' Mean Estimated Data
#'
#' @param sumobj 'sumobj' from clustersum()
#' @param modelsclusttype list of models from certain clustering method
#' @param cluster cluster to estimate
#'
#' @return Column Means, Estimate (centered), True Value. Class 'estobj'
#' @export
#'
estimate <- function(sumobj, modelsclusttype, cluster){

  dobj <- dataprocess(sumobj, cluster, 0)

  colmeans <- dobj$colmean

  truevalue <- modelmTest(sumobj, cluster, 0)$Testing

  estimate <- matrix(nrow = 24, ncol = 0)

  lowci <- matrix(nrow = 24, ncol = 0)

  upci <- matrix(nrow = 24, ncol = 0)
  
  meanSD <- c(NULL)
  
  for (i in (1:48)){
    post <- rstan::extract(modelsclusttype[[cluster]][[i]])

    tobj <- modelmTest(sumobj, cluster, i - 1)

    modelTest <- tobj$predM

    meanbeta <- as.matrix(colMeans(post$beta), ncol = 1)
    
    meansigma <- mean(post$sigma)
    
    meanSD <- c(meanSD, meansigma)
  
    est <- modelTest %*% meanbeta

    estimate <- cbind(estimate, est)

  }

  slr <- list(colmean = colmeans, est = estimate, trueval = truevalue, MeanSD = meanSD)

  class(slr) = 'estobj'

  return(slr)
  }

#' Plot Predict
#'
#' @param estobj 'estobj' from estimate().
#' @param day day to estimate.
#'
#' @return ggplot of estimate and true values, RMSE, and data frame with estimate for day and true value (not centered). Class 'plotobj'
#' @export
#'
plotpred <- function(estobj, day){
  V1 <- V2 <- V3 <- V4 <- V5 <- NULL

  colmeans <- estobj$colmean

  estimates <- estobj$est

  testing <- estobj$trueval
  
  meanSD <- estobj$MeanSD
  
  dayest <- matrix(estimates[day,], ncol = 1) + matrix(colmeans[-c(1,50,51)], ncol = 1)
  
  calc_ci <- function(mean_i, sd_i) {
    se <- sd_i
    lower <- qnorm(0.025, mean_i, se)
    upper <- qnorm(0.975, mean_i, se)
    return(c(lower, upper))
  }
  
  ci <- mapply(calc_ci, dayest, meanSD)
  
  lowest <- matrix(ci[1,], ncol = 1)
  
  upest <- matrix(ci[2,], ncol = 1)
  
  truday <- t(testing[day,])

  plotdf <- as.data.frame(cbind(1:ncol(testing), truday, dayest, 
                          lowest, upest))

  colnames(plotdf) <- c('Time', 'True', 'Est', 'Lower', 'Upper')

  rmse <- sqrt(sum((plotdf[,2] - plotdf[,3])^2)/length(plotdf[,3]))

  plotted <- ggplot2::ggplot(plotdf) + ggplot2::geom_point(ggplot2::aes(x = Time, y = True, colour = 'True Value')) + 
    ggplot2::geom_line(ggplot2::aes(x = Time, y = Est, colour = 'Estimate')) +
    ggplot2::geom_line(ggplot2::aes(x = Time, y = Lower, colour = 'Lower')) +
    ggplot2::geom_line(ggplot2::aes(x = Time, y = Upper, colour = 'Upper'))
    

  slr <- list(plot = plotted, RMSE = rmse, dayDF = plotdf)

  class(slr) = 'plotobj'

  return(slr)
}

#' Fit Stan Models for each time point of Cluster in Parr
#'
#' @param sumobj 'sumobj' from clustersum()
#' @param stancode .stan file with stancode for our fit
#' @param cluster which cluster to fit
#' @param ncores number of cores for stan fit
#' @return list of models for cluster
#' @export
#'
parfit <- function(sumobj, stancode, cluster, ncores){
  options(mc.cores = ncores)
  rstan::rstan_options(auto_write = TRUE)
  testparr <- list(NULL)
  for (i in 0:47){
    dp <- dataprocess(sumobj, cluster, i)

    fittemp <- fitmodel(dp, stancode)

    testparr[[i+1]] <- fittemp
  }
  return(testparr)
}

test_that("clustersum input validation", {
  # Create a valid input list for testing
  valid_input <- list(
    extra = data.frame(tod = c(1, 2, 1, 2), dow = c("Mon", "Tue", "Mon", "Mon")),
    indCons = data.frame(I1002 = c(1, 10, 20, 30), I1003 = c(40, 50, 60, 70))
  )
  clust <- data.frame(c("I1002", "I1003"), as.factor(c(1, 2)))
  # Test if the function raises an error for an invalid input
  expect_error(clustersum(list(),valid_input,clust,1))
  expect_error(clustersum(valid_input,list(),clust,1))
  expect_error(clustersum(3,valid_input,clust,1))
})

test_that("calc_ci produces correct intervals", {
  # Test case 1: Positive mean and standard deviation
  mean1 <- 10
  sd1 <- 2
  expected1 <- c(6.08, 13.92)
  result1 <- round(calc_ci(mean1, sd1),3)
  expect_equal(result1, expected1)

  # Test case 2: Negative mean and standard deviation
  mean2 <- -5
  sd2 <- 1.5
  expected2 <- c(-7.94, -2.06)
  result2 <- round(calc_ci(mean2, sd2),3)
  expect_equal(result2, expected2)

  # Test case 3: Zero mean and standard deviation
  mean3 <- 0
  sd3 <- 0
  expected3 <- c(0, 0)
  result3 <- round(calc_ci(mean3, sd3),3)
  expect_equal(result3, expected3)
})

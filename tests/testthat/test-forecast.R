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

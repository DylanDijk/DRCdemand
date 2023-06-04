# Unit test for Gower's distance function
# Define a test data set
test_data <- data.frame(
  V1 = c(1, 2, 3),
  V2 = c(4, 5, 6),
  V3 = c(7, 8, 9)
)

test_that("Gower's distance function calculates the distance matrix correctly", {
  # Call the function to calculate the Gower's distance matrix
  output <- gowers_distance(test_data)

  # Define the expected output matrix
  expected_output <- stats::as.dist(
    matrix(c(0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0),
           nrow = 3,
           ncol = 3))

  # Check if the output is a dist object
  expect_s3_class(output, "dist")

  # Check if the output matches the expected output
  expect_identical(attr(output, "method"), attr(expected_output, "method"))
  expect_equal(output, expected_output, ignore_attr = T)
})

# Unit test for weekly profiling
# Define the function to be tested
test_that("weekly_profile input validation and functionality test", {
  # Create a valid input list for testing
  valid_input <- list(
    extra = data.frame(tod = c(1, 2, 1, 2), dow = c("Mon", "Tue", "Mon", "Mon")),
    indCons = data.frame(I1002 = c(1, 10, 20, 30), I1003 = c(40, 50, 60, 70))
  )

  expected_output <- tibble::tibble(ID = c("I1002","I1003"), `1Mon` = c(10.5,50), `2Mon` = c(30,70), `2Tue` = c(10,50))

  # Test if the function returns correct output for a valid input
  expect_equal(DRCdemand::weekly_profile(valid_input), expected_output, ignore_attr = TRUE)

  # Test if the function raises an error for an invalid input
  expect_error(weekly_profile(list()))
})

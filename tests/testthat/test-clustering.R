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
  expect_equivalent(output, expected_output)
})


library(testthat)

# Create sample data for tests
test_data <- data.frame(
  x = c(1, 2, 3, 4, 5),
  y = c(2, 4, 6, 8, 10)
)

test_that("bivqfun creates correct object", {
  result <- bivqfun(test_data, "x", "y")
  
  expect_s4_class(result, "BivQPlot")
  expect_equal(result@tau, 0.5)
  expect_equal(result@var1, "x")
  expect_equal(result@var2, "y")
  expect_true(all(c("y1", "y2", "indicator") %in% names(result@data)))
})

test_that("bivqfun handles invalid inputs correctly", {
  expect_error(bivqfun(test_data, 123, "y"), 
               "Name of the first variable argument must be given as a character")
  expect_error(bivqfun(test_data, "x", 123), 
               "Name of the second variable argument must be given as a character")
  expect_error(bivqfun(as.matrix(test_data), "x", "y"), 
               "Input datasource should be a dataframe")
  expect_error(bivqfun(test_data, "nonexistent", "y"), 
               "object 'nonexistent' not found")
})
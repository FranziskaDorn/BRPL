library(testthat)

# Create sample data for tests
test_data <- data.frame(
  var1 = c(1, 2, 3, 4, 5),
  var2 = c(2, 4, 6, 8, 10)
)

test_that("bivqfun creates correct object", {
  result <- bivqfun(test_data, "var1", "var2")
  
  expect_s4_class(result, "BivQPlot")
  expect_equal(result@tau, 0.5)
  expect_equal(result@var1, "var1")
  expect_equal(result@var2, "var2")
  expect_true(all(c("y1", "y2", "indicator") %in% names(result@data)))
})

test_that("bivqfun handles invalid inputs correctly", {
  expect_error(bivqfun(test_data, 123, "var2"), 
               "Name of the first variable argument must be given as a character")
  expect_error(bivqfun(test_data, "var1", 123), 
               "Name of the second variable argument must be given as a character")
  expect_error(bivqfun(as.matrix(test_data), "var1", "var2"), 
               "Input datasource should be a dataframe")
  expect_error(bivqfun(test_data, "nonexistent", "var2"), 
               "object 'nonexistent' not found")
  expect_error(bivqfun((nrow(test_data) < 2), "var1", "var2"), 
               "Input data needs more than just one observation pair."
  )
})
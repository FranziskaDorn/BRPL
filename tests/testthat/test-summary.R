test_that("summary creates correct object", {
  # Setup test data
  test_data <- data.frame(
    x = c(1, 2, 3, 4, 5),
    y = c(2, 4, 6, 8, 10)
  )
  
  bivq_obj <- bivqfun(test_data, "x", "y")
  summary_obj <- summary(bivq_obj)
  
  # Test class and inheritance
  expect_s4_class(summary_obj, "BivQSummary")
  expect_true(is(summary_obj, "BivQ"))
  
  # Test calculated values
  expect_equal(summary_obj@mean_var1, mean(test_data$x))
  expect_equal(summary_obj@mean_var2, mean(test_data$y))
  expect_equal(summary_obj@sd_var1, sd(test_data$x))
  expect_equal(summary_obj@sd_var2, sd(test_data$y))
  
  # Test that original data is preserved
  expect_equal(summary_obj@tau, bivq_obj@tau)
  expect_equal(summary_obj@var1, bivq_obj@var1)
  expect_equal(summary_obj@var2, bivq_obj@var2)
})

test_that("summary handles NA values correctly", {
  # Setup test data with NAs
  test_data <- data.frame(
    x = c(1, NA, 3, 4, 5),
    y = c(2, 4, NA, 8, 10)
  )
  
  bivq_obj <- bivqfun(test_data, "x", "y")
  summary_obj <- summary(bivq_obj)
  
  # Test that NAs are handled correctly
  expect_equal(summary_obj@mean_var1, mean(test_data$x, na.rm = TRUE))
  expect_equal(summary_obj@mean_var2, mean(test_data$y, na.rm = TRUE))
  expect_equal(summary_obj@sd_var1, sd(test_data$x, na.rm = TRUE))
  expect_equal(summary_obj@sd_var2, sd(test_data$y, na.rm = TRUE))
})
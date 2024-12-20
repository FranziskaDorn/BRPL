test_that("bivqfun handles edge cases", {
  # Test with single row
  single_row <- data.frame(x = 1, y = 2)
  expect_error(bivqfun(single_row, "x", "y"),
               "need at least 2 points to interpolate")
  
  # Test with identical values
  identical_vals <- data.frame(
    x = c(1, 1, 1),
    y = c(2, 2, 2)
  )
  expect_warning(bivqfun(identical_vals, "x", "y"),
                "zero-length arrow is of indeterminate angle and so skipped")
  
  # Test with NA values
  na_data <- test_data
  na_data$x[1] <- NA
  expect_warning(bivqfun(na_data, "x", "y"),
                "missing values removed")
})
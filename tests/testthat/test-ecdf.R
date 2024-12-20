test_that("myecdf functions correctly", {
  result <- myecdf(test_data, "x")
  
  expect_type(result, "list")
  expect_true(all(c("ecdf", "qf") %in% names(result)))
  expect_type(result$ecdf, "closure")
  expect_type(result$qf, "closure")
  
  # Test ECDF values
  expect_equal(result$ecdf(1), 0)
  expect_equal(result$ecdf(5), 1)
  
  # Test quantile function
  expect_equal(result$qf(0), 1)
  expect_equal(result$qf(1), 5)
})

test_that("myecdf handles invalid inputs", {
  expect_error(myecdf(test_data, 123),
               "Variable name argument must be given as a character")
  expect_error(myecdf(as.matrix(test_data), "x"),
               "Input datasource should be a dataframe")
})

test_that("prepquant produces expected output format", {
  # Setup
  data <- data.frame(y1 = c(0.2, 0.4, 0.6),
                     y2 = c(0.3, 0.5, 0.7))
  ecdf1 <- myecdf(test_data, "x")
  ecdf2 <- myecdf(test_data, "y")
  
  result <- prepquant(data, 0.5, ecdf1, ecdf2, 10)
  
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 4)
  expect_equal(nrow(result), 10)
  expect_true(all(c("var1", "var2", "y1", "y2") %in% names(result)))
})

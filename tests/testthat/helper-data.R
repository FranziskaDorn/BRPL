# Helper functions for creating test data
create_test_data <- function(n = 100) {
  data.frame(
    x = rnorm(n),
    y = rnorm(n)
  )
}
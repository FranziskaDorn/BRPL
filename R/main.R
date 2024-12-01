#' Bivariate Quantile Function
#'
#' This function calculates the bivariate quantile function for two variables in a given dataset. It computes the empirical cumulative distribution functions (ECDFs) for each variable, standardizes the data, and derives the bivariate quantile curve. Additionally, it creates an indicator variable to classify points relative to the quantile curve.
#'
#' @param data A `data.frame` containing the input data.
#' @param var1 A character string specifying the name of the first variable in the dataset.
#' @param var2 A character string specifying the name of the second variable in the dataset.
#' @param tau A numeric value (default = 0.5) specifying the quantile level to compute. Must be a double.
#' @param nalpha An integer specifying the number of steps for discretizing the quantile levels (default = 100).
#'
#' @details
#' This function performs the following steps:
#' - Validates the input arguments.
#' - Computes the empirical cumulative distribution functions (ECDFs) for `var1` and `var2` using a helper function `myecdf`.
#' - Standardizes the two variables to the range [0, 1].
#' - Prepares the data and calculates intermediate values using a helper function `prepquant`.
#' - Creates interpolation functions to map between the standardized variables and their original scales.
#' - Generates the bivariate quantile curve, expressed as a sequence of values for `var1` and `var2`.
#' - Constructs an indicator variable in the dataset to classify observations based on their position relative to the quantile curve.
#'
#' @return
#' A list of class `"bivq"` containing the following components:
#' - `data`: The input dataset with an additional `indicator` column, indicating if a observation is above (1) or below (0) the caluclated iso-quant line.
#' - `tau`: The quantile level used.
#' - `plvar2`: The bivariate quantile curve points for `var2`.
#' - `faz`: The extreme values for the quantile curve on the lower end.
#' - `faa`: The extreme values for the quantile curve on the upper end.
#' - `var1`: The name of the first variable.
#' - `var2`: The name of the second variable.
#'
#' @examples
#' # Example dataset
#' data("nutritionpoverty")
#' bivqfun(data = nutritionpoverty, var1="NUval", var2="INval", tau = 0.25)
#'
#' @importFrom stats approxfun
#' @export
bivqfun <- function(data, var1, var2, tau = 0.5, nalpha = 100) {

  # Input checks:
  stopifnot(
    "Name of the first variable argument must be given as a character." = is.character(var1),
    "Name of the second variable argument must be given as a character." = is.character(var2),
    "Tau argument has to be given as a double" = is.double(tau),
    "Input datasource should be a dataframe." = is.data.frame(data)
  )

  # Calculate empirical cumulative distribution functions
  ecdfvar1 <- myecdf(data, var1)
  ecdfvar2 <- myecdf(data, var2)

  # Standardize the variables between 0 and 1
  data$y1 <- ecdfvar1$ecdf(data[[var1]])
  data$y2 <- ecdfvar2$ecdf(data[[var2]])

  # Prepare the data and calculate the values
  res <- prepquant(data, tau, ecdfvar1, ecdfvar2, nalpha)

  # Create interpolation functions
  bivqf <- approxfun(res$y1, res$y2, ties = "max")
  bivqforig <- approxfun(res$var1, res$var2, ties = "max")

  # Generate the bivariate quantile curve
  bivqcurve <- data.frame(
    var1 = seq(min(data[[var1]]), max(data[[var1]]), length = 250),
    y1 = seq(min(data$y1), max(data$y1), length = 250)
  )
  bivqcurve$y2 <- bivqf(bivqcurve$y1)
  bivqcurve$var2 <- bivqforig(bivqcurve$var1)

  # Remove missing values
  bivhhi <- na.omit(data.frame(cbind(bivqcurve$var1, bivqcurve$var2)))

  # Include extreme values
  faz <- c(min(bivhhi[, 1]), max(data[[var2]], na.rm = TRUE))
  faa <- c(max(data[[var1]], na.rm = TRUE) + 1, min(bivhhi[, 2]))
  plvar2 <- rbind(faz, bivhhi, faa)

  # Calculate indicator indicator
  data$indicator <- rep(0, nrow(data))
  for (i in 1:nrow(data)) {
    x <- c(data[[var1]][i], data[[var2]][i])
    if (all(x[1] > plvar2[, 1])) {
      data$indicator[i] <- 0
    } else {
      ind <- min(which(x[1] <= plvar2[, 1]))
      if (x[2] > plvar2[ind, 2]) {
        data$indicator[i] <- 1
      }
    }
  }

  # Return results as an object of class 'bivq'
  output <- list(
    data = data,
    tau = tau,
    plvar2 = plvar2,
    faz = faz,
    faa = faa,
    var1 = var1,
    var2 = var2
  )
  class(output) <- "bivq"
  return(output)
}



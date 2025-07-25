#' Calculate Bivariate Quantiles
#'
#' @importFrom stats approxfun na.omit
#' @importFrom methods new
#' @importFrom utils hasName
#' @param data Input data frame (tibbles and other data.frame variants are automatically converted to data.frame)
#' @param var1 Name of first variable
#' @param var2 Name of second variable
#' @param tau Quantile level (default: 0.5)
#' @param nalpha Number of alpha values (default: 100)
#' @return An object of class brplPlot
#' @examples
#' df_data <- data.frame(x = 1:10, y = 1:10)
#' result <- brpl(df_data, "x", "y")
#' @export
brpl <- function(data, var1, var2, tau = 0.5, nalpha = 100) {
  # Convert tibble or other data.frame variants to pure data.frame if necessary
  # This ensures compatibility with S4 class validation
  if (!identical(class(data), "data.frame")) {
    if (inherits(data, "data.frame")) {
      data <- as.data.frame(data)
    }
  }
  
  # Input validation
  stopifnot(
    "Name of the first variable argument must be given as a character." = is.character(var1),
    "Name of the second variable argument must be given as a character." = is.character(var2),
    "Tau argument has to be given as a numeric" = is.numeric(tau),
    "Input datasource should be a dataframe." = is.data.frame(data),
    "First variable not found in data." = var1 %in% colnames(data),
    "Second variable not found in data." = var2 %in% colnames(data),
    "Input data needs more than just one observation pair." = (nrow(data) > 2)
  )

  # Calculate ECDFs
  ecdfvar1 <- myecdf(data, var1)
  ecdfvar2 <- myecdf(data, var2)

  # Standardize variables
  data$y1 <- ecdfvar1$ecdf(data[[var1]])
  data$y2 <- ecdfvar2$ecdf(data[[var2]])

  # Calculate quantiles
  res <- prepquant(data, tau, ecdfvar1, ecdfvar2, nalpha)

  # Create interpolation functions
  bivqf <- approxfun(res$y1, res$y2, ties = "max")
  bivqforig <- approxfun(res$var1, res$var2, ties = "max")

  # Generate bivariate quantile curve
  bivqcurve <- data.frame(
    var1 = seq(min(data[[var1]]), max(data[[var1]]), length = 250),
    y1 = seq(min(data$y1), max(data$y1), length = 250)
  )
  bivqcurve$y2 <- bivqf(bivqcurve$y1)
  bivqcurve$var2 <- bivqforig(bivqcurve$var1)

  # Remove NAs and include extreme values
  bivhhi <- na.omit(data.frame(cbind(bivqcurve$var1, bivqcurve$var2)))
  faz <- c(min(bivhhi[, 1]), max(data[[var2]], na.rm = TRUE))
  faa <- c(max(data[[var1]], na.rm = TRUE) + 1, min(bivhhi[, 2]))
  plvar2 <- rbind(faz, bivhhi, faa)

  # Calculate indicators
  data$indicator <- vapply(seq_len(nrow(data)), function(i) {
    x <- c(data[[var1]][i], data[[var2]][i])
    if (all(x[1] > plvar2[, 1])) return(0)
    ind <- min(which(x[1] <= plvar2[, 1]))
    ifelse(x[2] > plvar2[ind, 2], 1, 0)
  }, numeric(1))

  # Create and return BivQPlot object
  new("brplPlot",
      data = data,
      tau = tau,
      var1 = var1,
      var2 = var2,
      plvar2 = as.matrix(plvar2),
      faz = faz,
      faa = faa)
}


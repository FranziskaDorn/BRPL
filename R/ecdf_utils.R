#' Calculate Empirical Cumulative Distribution Function
#'
#' @importFrom stats approxfun
#' @param data A data frame containing the variable
#' @param var Character string specifying the variable name
#' @param weight Character string naming the weight column in \code{data}, or
#'   \code{NULL} for uniform weights (default).
#' @param min.var Minimum value for the variable (used as lower bound of quantile function)
#' @return List containing ecdf and quantile functions
#' @export
myecdf <- function(data, var, weight = NULL, min.var = 0) {
  stopifnot(
    "Name of the variable argument must be given as a character." = is.character(var),
    "Input datasource should be a dataframe." = is.data.frame(data),
    "No input given as discriminant variable." = !exists(var),
    "Input data needs more than just one observation pair." = (nrow(data) > 2),
    "weight must be NULL or a column name in data." =
      is.null(weight) || (is.character(weight) && weight %in% colnames(data))
  )

  w <- if (is.null(weight)) rep(1 / nrow(data), nrow(data)) else data[[weight]]
  w <- w / sum(w)

  df <- data.frame(value = data[[var]], weight = w)
  df <- aggregate(weight ~ value, data = df, FUN = sum)
  df <- df[order(df$value), ]
  df$cum_weight <- cumsum(df$weight) / sum(df$weight)

  ecdf <- approxfun(df$value, df$cum_weight, method = "linear", yleft = 0, yright = 1)
  qf   <- approxfun(df$cum_weight, df$value,  method = "linear",
                    yleft = min.var, yright = max(df$value))

  list(ecdf = ecdf, qf = qf)
}

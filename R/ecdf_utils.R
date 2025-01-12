#' Calculate Empirical Cumulative Distribution Function
#'
#' @importFrom stats approxfun
#' @param data A data frame containing the variable
#' @param var Character string specifying the variable name
#' @param min.var Minimum value for the variable
#' @return List containing ecdf and quantile functions
#' @export
myecdf <- function(data, var, min.var = 0) {
  stopifnot(
    "Name of the variable argument must be given as a character." = is.character(var),
    "Input datasource should be a dataframe." = is.data.frame(data),
    #"No input given as discriminant variable." = !exists(var),
    "Input data needs more than just one observation pair." = (nrow(data) > 2)
  )

  n_row <- nrow(data[var])
  target <- data.frame(
    v = rbind(min.var, data[var]),
    ecdf = rep(0, (n_row + 1))
  )

  target <- target[order(target[,var]),]
  target['ecdf'] <- seq(0, 1, length = (n_row + 1))

  indicator <- tapply(seq_along(target[,var]),
                     target[, var],
                     max)

  target <- target[indicator, ]

  ecdf <- approxfun(target[, var],
                    target[,"ecdf"],
                    method = "linear"
                    )

  qf <- approxfun(target[,"ecdf"],
                  target[,var],
                  method = "linear"
                  )

  return(list(ecdf = ecdf, qf = qf))
}

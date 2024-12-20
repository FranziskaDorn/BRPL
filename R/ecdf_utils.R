#' Calculate Empirical Cumulative Distribution Function
#' 
#' @param data A data frame containing the variable
#' @param var Character string specifying the variable name
#' @param min.var Minimum value for the variable
#' @return List containing ecdf and quantile functions
#' @export
myecdf <- function(data, var, min.var = 0) {
  stopifnot(
    "Variable name argument must be given as a character." = is.character(var),
    "Input datasource should be a dataframe." = is.data.frame(data)
  )
  
  n <- nrow(data[var])
  target <- data.frame(
    v = rbind(min.var, data[var]),
    ecdf = rep(0, (n + 1))
  )
  
  target <- target[order(target[,var]),]
  target['ecdf'] <- seq(0, 1, length = (n + 1))
  
  indicator <- tapply(seq_along(target[,var]),
                     target[, var],
                     max)
  
  target <- target[indicator, ]
  
  ecdf <- approxfun(target[, var],
                    target[,"ecdf"],
                    method = "linear",
                    n = 100)
  
  qf <- approxfun(target[,"ecdf"],
                  target[,var],
                  method = "linear",
                  n = 100)
  
  return(list(ecdf = ecdf, qf = qf))
}
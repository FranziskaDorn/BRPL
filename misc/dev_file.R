load("data/total.Rda")
load("data/total1.Rda")

# Libraries
# library(assertthat)

# neue ecdf version
myecdf <- function(data, var, min.var = 0){
  # Input checks:
  stopifnot("Variable name argument must be given as a character." = is_character(var),
            "Input datasource should be a dataframe." = is.data.frame(data))

  # number of observations
  n = nrow(data[var])
  # create temporary object for value collection
  target = data.frame(v=rbind(min.var, data[var]),
                      ecdf=rep(0, (n + 1)) )
  # order the entries in ascending order
  target <- target[order(target[,var]),]
  # create column for ecdf values
  target['ecdf'] <- seq(0, 1, length = (n + 1) )
  # create indicator for row selection
  indicator <- tapply(seq_along(target[,var]),
                      target[, var],
                      max)
  # select values based on indicator
  target <- target[indicator, ]
  # approximate the ecdf and quantile function
  ecdf <- approxfun(target[, var],
                    target[,"ecdf"],
                    method = "linear",
                    n = 100)
  qf <- approxfun(target[,"ecdf"],
                  target[,var],
                  method = "linear",
                  n = 100)
  return(list(ecdf=ecdf, qf=qf))
}

myecdf(data = total1, var = "INval")

# tidy up
rm(list = ls())
gc(reset = TRUE)

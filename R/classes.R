#' @title brpl Class
#' @description Base class for bivariate quantile calculations
#' @exportClass brpl
setClass("brpl",
         slots = list(
           data = "data.frame",
           tau = "numeric",
           var1 = "character",
           var2 = "character",
           plvar2 = "matrix",
           faz = "numeric",
           faa = "numeric"
         ),
         prototype = list(
           tau = 0.5
         )
)

#' @title brplPlot Class
#' @description Class for plotting bivariate quantile results
#' @exportClass brplPlot
setClass("brplPlot",
         contains = "brpl"
)

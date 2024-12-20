#' @title BivQ Class
#' @description Base class for bivariate quantile calculations
#' @exportClass BivQ
setClass("BivQ",
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

#' @title BivQPlot Class
#' @description Class for plotting bivariate quantile results
#' @exportClass BivQPlot
setClass("BivQPlot",
         contains = "BivQ"
)
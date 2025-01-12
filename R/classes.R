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

#' @title BivQSummary Class
#' @description Class for summarizing bivariate quantile results
#' @exportClass BivQSummary
setClass("BivQSummary",
         contains = "BivQ",
         slots = list(
           mean_var1 = "numeric",
           mean_var2 = "numeric",
           sd_var1 = "numeric",
           sd_var2 = "numeric",
           data_group0 = "data.frame",
           data_group1 = "data.frame",
           n_outliers = "numeric"
         )
)
#' @export
setGeneric("plot", function(x, ...) standardGeneric("plot"))

#' @export
setMethod("plot", "BivQPlot",
          function(x, ...) {
            # Plot implementation here
            plot(x@data[[x@var1]], x@data[[x@var2]], 
                 xlab = x@var1, ylab = x@var2,
                 col = ifelse(x@data$indicator == 1, "red", "black"))
            lines(x@plvar2[,1], x@plvar2[,2], col = "blue")
          }
)
#' Plot Method for BivQPlot Class
#'
#' This method defines the plotting behavior for objects of the class `BivQPlot`. It generates a
#' scatter plot visualizing the relationship between two variables (`var1` and `var2`) along with
#' classification indicators and an additional line for further insights.
#'
#' @param x An object of class `BivQPlot`. The object must include the following slots:
#'   - `@data`: A data frame containing the data to be plotted, including `var1`, `var2`,
#'     and a classification indicator `indicator`.
#'   - `@var1`: A string specifying the name of the first variable to be plotted (X-axis).
#'   - `@var2`: A string specifying the name of the second variable to be plotted (Y-axis).
#'   - `@indicator`: A binary variable indicating the classification or grouping of points.
#'   - `@tau`: A threshold value included in the plot's title.
#'   - `@plvar2`: A matrix object defining the data points for the additional line in the plot.
#' @param y Ignored. Included for compatibility with the generic `plot` function.
#' @param ... Additional arguments passed to the base R `plot` function.
#'
#' @importFrom graphics lines
#'
#' @details
#' This method creates:
#' - A scatter plot with points colored based on the value of `indicator`:
#'   - `darkgreen` for `indicator` = 1.
#'   - `blue` for other values.
#' - An additional line, derived from the `plvar2` matrix, drawn in black.
#'
#' The title of the plot includes the threshold value (`tau`) for easier interpretation.
#'
#' @return This function does not return a value. It generates a plot as a side effect.
#'
#' @export
setMethod("plot", "BivQPlot",
          function(x, y, ...) {
            # Plot implementation here
            plot(x@data[[x@var1]], x@data[[x@var2]],
                 xlab = x@var1,
                 ylab = x@var2,
                 col = ifelse(x@data$indicator == 1, 'darkgreen', 'blue'),
                 main = paste("Bivariate relative discriminator with given tau of", x@tau))
            lines(x@plvar2[,1], x@plvar2[,2],
                  lwd = 3,
                  col = 'black')
          }
)


#' Create summary for BivQ object
#' @param object BivQ object
#' @return BivQSummary object
#' @export
setMethod("summary", "BivQ",
          function(object) {
            # Berechne Zusammenfassung
            mean_var1 <- mean(object@data[[object@var1]], na.rm = TRUE)
            mean_var2 <- mean(object@data[[object@var2]], na.rm = TRUE)
            sd_var1 <- sd(object@data[[object@var1]], na.rm = TRUE)
            sd_var2 <- sd(object@data[[object@var2]], na.rm = TRUE)
            data_group0 <- tt@data[tt@data$indicator==0, ]
            data_group1 <- tt@data[tt@data$indicator==1, ]
            n_outliers <- sum(object@data$indicator == 1, na.rm = TRUE)
            
            # Erstelle neues Summary-Objekt
            new("BivQSummary",
                data = object@data,
                tau = object@tau,
                var1 = object@var1,
                var2 = object@var2,
                plvar2 = object@plvar2,
                faz = object@faz,
                faa = object@faa,
                mean_var1 = mean_var1,
                mean_var2 = mean_var2,
                sd_var1 = sd_var1,
                sd_var2 = sd_var2,
                data_group0 = data_group0,
                data_group1 = data_group1,
                n_outliers = n_outliers)
          })

#' @export
setMethod("show", "BivQSummary",
          function(object) {
            cat("Bivariate Quantile Summary\n")
            cat("==========================\n")
            cat(sprintf("Tau: %.2f\n", object@tau))
            cat("\nVariable Statistics:\n")
            cat(sprintf("%s:\n", "Below"))
            cat(sprintf(summary(object@data_group0)))
            cat(sprintf("  SD:   %.2f\n", object@sd_var1))
            cat(sprintf("\n%s:\n", "Above"))
            cat(sprintf("  Mean: %.2f\n", object@mean_var2))
            cat(sprintf("  SD:   %.2f\n", object@sd_var2))
            cat(sprintf("\nNumber of outliers: %d\n", object@n_outliers))
          })
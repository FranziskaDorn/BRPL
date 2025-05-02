#' Prepare Quantile Calculations
#'
#' @importFrom stats quantile
#' @param data Data frame with standardized variables
#' @param tau Quantile level
#' @param ecdfvar1 ECDF for first variable
#' @param ecdfvar2 ECDF for second variable
#' @param nalpha Number of alpha values
#' @return Data frame with quantile calculations
#' @keywords internal
prepquant <- function(data, tau, ecdfvar1, ecdfvar2, nalpha) {
  alphas <- seq(0 * pi/32, 16 * pi/32, by = 0.5 * pi/(nalpha-1))
  res <- data.frame(
    var1 = numeric(nalpha),
    var2 = numeric(nalpha),
    y1 = numeric(nalpha),
    y2 = numeric(nalpha)
  )

  for(i in seq_along(alphas)) {
    alpha <- alphas[i]
    rsample <- pmin(
      (1-data$y1) / (cos(alpha) + .Machine$double.eps),
      (1-data$y2) / (sin(alpha) + .Machine$double.eps)
    )
    r <- quantile(rsample, 1-tau)
    q <- c(1-r*cos(alpha), 1-r*sin(alpha))
    qorig <- c(ecdfvar1$qf(q[1]), ecdfvar2$qf(q[2]))
    res[i,] <- c(qorig, q)
  }
  return(res)
}


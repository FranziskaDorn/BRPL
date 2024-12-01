#' Main File for the wrapper
#'
#' @importFrom rlang is_character
#' @importFrom stats approxfun quantile na.omit
#' @param data data source of interest. Has to be a data.frame.
#' @param tau Minimal possible value for given variable.
#' @param nalpha Minimal possible value for given variable.
#' @param ecdfvar1 ecdf object of variable 1 as constructed by myecdf function.
#' @param ecdfvar2 ecdf object of variable 2 as constructed by myecdf function.


prepquant <- function(data, tau, ecdfvar1, ecdfvar2, nalpha){
  # alter code von franzi
  # TODO Verbessern. Muss effizienter werden.
  alphas <- seq(0*pi/32, 16*pi/32, by = 0.5*pi/(nalpha-1))
  res <- data.frame(var1 = rep(0, nalpha),
                    var2 = rep(0, nalpha),
                    y1 = rep(0, nalpha),
                    y2 = rep(0, nalpha))

  for(i in 1:length(alphas)){
    alpha <- alphas[i]
    rsample <- pmin((1-data$y1) / (cos(alpha) + .Machine$double.eps),
                    (1-data$y2) / (sin(alpha) + .Machine$double.eps) ) # neuer Code: erg√§nzt um ".Machine$double.eps" von oben, um die Division durch Null zu vermeiden, winzige Abweichung von Null verhindert, dass NA entstehen
    # Isoquanten Punkte
    r <- quantile(rsample, 1-tau)
    q <- c(1-r*cos(alpha), 1-r*sin(alpha))
    qorig <- c(ecdfvar1$qf(q[1]), ecdfvar2$qf(q[2]))
    res[i,] <- c(qorig, q)
  }
  return(res)
}

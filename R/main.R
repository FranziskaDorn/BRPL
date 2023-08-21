#' Main File for the wrapper
#'
#' @importFrom rlang is_character
#' @importFrom stats approxfun quantile na.omit
#' @param data data source of interest. Has to be a data.frame.
#' @param var1 Variable of interest. Has to be a character.
#' @param var2 Variable of interest. Has to be a character.
#' @param tau Minimal possible value for given variable.
#' @param nalpha Minimal possible value for given variable.
#' @param plot Logical. When `TRUE` a graphic of the results will be produced.
#'
#' @export

bivqfun = function(data, var1, var2, tau=0.5, nalpha=100, plot = TRUE){

  # Input checks:
  stopifnot("Name of the first variable argument must be given as a character." = is_character(var1),
            "Name of the second variable argument must be given as a character." = is_character(var2),
            "Tau argument has to be given as a double" = is.double(tau),
            "Input datasource should be a dataframe." = is.data.frame(data))

  # hier wenden wir sie an.
  # Einmal für y und x.
  ecdfvar1 <- myecdf(data, var1)
  ecdfvar2 <- myecdf(data, var2)

  # Std. Form die zwischen 0  und 1 liegt. Warum gerade keine Ahung.
  # Möglich zur Sortierung?
  data$y1 <- ecdfvar1$ecdf(data[[var1]])
  data$y2 <- ecdfvar2$ecdf(data[[var2]])

  # Vorbereitung der Daten und Berechnung der Werte
  res <- prepquant(data, tau, ecdfvar1, ecdfvar2, nalpha)

  bivqf <- approxfun(res$y1, res$y2, ties=max)
  bivqforig <- approxfun(res$var1, res$var2, ties=max)

  # NUval : Das ist die Variable von Interesse. Muss Variable sein, Input-Groesse.
  bivqcurve <- data.frame(var1=seq(min(data[[var1]]), max(data[[var1]]), length=250),
                          y1=seq(min(data$y1), max(data$y1), length=250))
  bivqcurve$y2 <- bivqf(bivqcurve$y1)
  bivqcurve$var2 <- bivqforig(bivqcurve$var1)

  bivhhi<-data.frame(cbind(bivqcurve$var1, bivqcurve$var2))
  bivhhi<-na.omit(bivhhi)

  # include etreme values (Randwerte)
  faz <- c(min(bivhhi[,1]), max(data[[var2]], na.rm=TRUE))
  faa <-  c(max(data[[var1]], na.rm=TRUE)+1, min(bivhhi[,2]))
  plvar2 <- rbind(faz, bivhhi, faa)




  data$abovevar2<- c(rep(0))
  n<- nrow(data)

  for (i in 1:n) {
    # erstellen der jeweiligen NUval, Income Paare
    x <- c(data[[var1]][i], data[[var2]][i])
    #liegt der punkte drunter?
    #ind <- min(which(x[1]<=pl[,1]))
    # ist der NUval wert gr??er den pl Werten?
    if (all(x[1]>plvar2[,1])) {
      # Wenn ja, dann ist above = 0
      data$abovevar2[i] <- 0
    } else {
      # wenn nicht gr??er als alle dann
      # indikator (position) auslesen
      ind <- min(which(x[1]<=plvar2[,1]))
      if(x[2] > plvar2[ind,2]){
        # Dann wenn die Werte gr??er gleich
        data$abovevar2[i] <- 1 # above
      }
    }
  }

  if (plot == TRUE) {
    dbvar2<- subset(data, abovevar2==0)
    davar2<- subset(data, abovevar2==1)

    plot(plvar2[,1], plvar2[,2], type="l", col="black", lwd=3,
         xlab="Variable 1", ylab="Variable 2",
         xlim= c(min(dbvar2[[var1]])*0.9,faa[1]*1.1),
         ylim= c(0, faz[2]),
         main=paste("Bivariate relative discrimiator with given tau of", tau))


    points(dbvar2[[var1]], dbvar2[[var2]], col = "blue")
    points(davar2[[var1]], davar2[[var2]], col= "darkgreen")

    abline(v=faz[[1]], col="black", lwd=3)
    abline(h=faa[[2]] , col="black", lwd=3)
    lines(plvar2[,1], plvar2[,2], col = "black",lwd=3)
  }

  print("Funktioniert.")

}

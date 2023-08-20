# Nachbau von Franzis Code für Testing Zwecke

# Code spezifische Packages
library(rlang)    # für variablen name catch

# Daten einelesen
d <- readRDS("~/Git/bivarte-discriminant-package/misc/data_co2_gini_02_21.RDS")
d2 <- read_csv("misc/001_allIndicatorsValuesAndRatios_1992-2015_03oct2021.csv")


# Daten bearbeiten
## Für den ersten Datensatz
d <- tibble::as_tibble(d)
data <- dplyr::rename(d,  country = Country, date=Year)
data$country[data$country=='yemen'] <- 'yemen, rep.'
data$country[data$country=='united states of america'] <- 'united states'
data$country[data$country=='venezuela'] <- 'venezuela, rb'
data$country[data$country=='slovakia'] <- 'slovak republic'
data$country[data$country=='russia'] <- 'russian federation'
data$country[data$country=='laos'] <- 'lao pdr'
data$country[data$country=='kyrgyzstan'] <- 'kyrgyz republic'
data$country[data$country=='korea south'] <- 'korea, rep.'
data$country[data$country=='iran'] <- 'iran, islamic rep.'
data$country[data$country=='gambia'] <- 'gambia, the'
data$country[data$country=='egypt'] <- 'egypt, arab rep.'

## Für den zweiten Datensatz
d2$country<-tolower(d2$country)
d2 <- as.data.frame(d2)

## Variablen erstellen
m<- c("country", "date", "indicator", "value")
d3<- d2[m]
d3<- d3 %>% tidyr::spread(indicator,value)

## Zusammenführen der Daten
total <- merge(data,d3,by=c("date","country"))

#########################
#estimating quantile line
#########################
set.seed(5000)
# Später rausnehmen und explizit forcen.
epsilon <- .Machine$double.eps # hier wird die kleinstmögliche Zahl gespeichert, damit nicht durch 0 geteilt wird

#############################
#Nutrition and income poverty
#############################

# Rausziehen der X und Y Variable, die müssen variable sein
m<- c("INval","NUval")
total1 <- total[m]
total1 <- total1[complete.cases(total1), ]
n <- nrow(total1)
var <- "NUval"


# Die Quantile sind vom Benutzer festzulegen, daher auch variable.
percentile1 <- ecdf(total1$NUval)
percentile1(2700) # 0.43 #threshold from table

# Die Quantile sind vom Benutzer festzulegen, daher auch variable.
percentile1 <- ecdf(total1$INval)
percentile1(95) # 0.74 #threshold from table. Which to use? Is income poverty not related to the food basket that alows the minimum calorie intake, so nutrition.

# Funktion für die CDF
# bauen die Quantilslinie (Isoquante). Das funktionert nicht richtig in bifquan
# bivquant zieht eine Stichprobe, währen dass hier die gesamten Daten anschaut.
# Berechnet die Quadrate, deren Schnittpunkte der X und Y Achse die Threshold der Isoquante bilden.

# Neue Version
myecdf <- function(data, var, min.var=0){
  var_name <- rlang::as_string(var)     # wandelt  variable in string fürs subsetten um
  n <- nrow(data)
  helptotal1 <- data.frame(v=c(min.var,
                               data[,var_name]),
                           ecdf=rep(0, n + 1)) # neuer Code, erstmal nur lauter Nullen
  names(helptotal1)[1] <- var_name
  helptotal1 <- helptotal1[order(helptotal1[,var_name]),]
  helptotal1$ecdf <- seq(0, 1, length = (n + 1)) # neuer Code, als sequence, muss an dieser Stelle stehen
  ind <- tapply(seq_along(helptotal1[,var_name]),
                helptotal1[,var_name],
                max)
  helptotal1 <- helptotal1[ind,]
  ecdf <- approxfun(helptotal1[,var_name],
                    helptotal1[,"ecdf"],
                    method = "linear",
                    n = 100)
  qf <- approxfun(helptotal1[,"ecdf"],
                  helptotal1[,var_name],
                  method = "linear",
                  n = 100)
  return(list(ecdf=ecdf, qf=qf))
}

bivquant <- function(varX, varY, data, tau=0.43, plot=FALSE, ...){
  # anwenden von ecdf funktion für X und Y
  # x = Income Inequality
  # y = Nutrition
  ecdfY <- myecdf(data, paste(varY))    # ist das y
  ecdfX <- myecdf(data, paste(varX))    # ist das x

  # Std. Form die zwischen 0  und 1 liegt. Warum gerade keine Ahung.
  # Möglich zur Sortierung?
  data$y1 <- ecdfY$ecdf(data$varY)
  data$y2 <- ecdfX$ecdf(data$varX)

  # Warum nalpha? Erst einmal egal. Vielleicht fällts später auf.
  nalpha <- 100
  alphas <- seq(0*pi/32, 16*pi/32, by = 0.5*pi/(nalpha-1))
  res <- data.frame(NUval = rep(0, nalpha), INval = rep(0, nalpha), y1 = rep(0, nalpha), y2 = rep(0, nalpha))

  # nachher rausnehmen für ein globales tau
  # Loop für die Quantilsgleichung
  # Intervall von 0 bis 43% und 43% bis 100%
  for(i in 1:length(alphas)){
    alpha <- alphas[i]
    rsample <- pmin((1-data$y1) / (cos(alpha) + epsilon), (1-data$y2) / (sin(alpha) + epsilon) ) # neuer Code: ergänzt um "epsilon" von oben, um die Division durch Null zu vermeiden, winzige Abweichung von Null verhindert, dass NA entstehen
    # Isoquanten Punkte
    r <- quantile(rsample, 1-tau)
    q <- c(1-r*cos(alpha), 1-r*sin(alpha))
    qorig <- c(ecdfY$qf(q[1]), ecdfX$qf(q[2]))
    res[i,] <- c(qorig, q)
  }
  bivqf <- approxfun(res$y1, res$y2, ties=max)
  bivqforig <- approxfun(res$NUval, res$INval, ties=max)

  # NUval : Das ist die Variable von Interesse. Muss Variable sein, Input-Groesse.
  bivqcurve <- data.frame(NUval=seq(min(data$NUval), max(data$NUval), length=250),
                          y1=seq(min(data$y1), max(data$y1), length=250))
  bivqcurve$y2 <- bivqf(bivqcurve$y1)
  bivqcurve$INval <- bivqforig(bivqcurve$NUval)

  bivhhi<-data.frame(cbind(bivqcurve$NUval, bivqcurve$INval))
  bivhhi<-bivhhi %>%na.omit

  plpsid1<-bivhhi

  # include etreme values (Randwerte)
  foo <-  bivhhi
  faz <- c(min(plpsid1[,1]), max(total$INval, na.rm=TRUE))
  faa <-  c(max(total$NUval, na.rm=TRUE)+1, min(plpsid1[,2]))
  plINval <- rbind(faz, foo, faa)

  data$aboveINval<- c(rep(0))
  n<- nrow(data)

  for (i in 1:n) {
    # erstellen der jeweiligen NUval, Income Paare
    x <- c(data$NUval[i], data$INval[i])
    #liegt der punkte drunter?
    #ind <- min(which(x[1]<=pl[,1]))
    # ist der NUval wert gr??er den pl Werten?
    if (all(x[1]>plINval[,1])) {
      # Wenn ja, dann ist above = 0
      data$aboveINval[i] <- 0
    } else {
      # wenn nicht gr??er als alle dann
      # indikator (position) auslesen
      ind <- min(which(x[1]<=plINval[,1]))
      if(x[2] > plINval[ind,2]){
        # Dann wenn die Werte gr??er gleich
        data$aboveINval[i] <- 1 # above
      }
    }
  }
  if (plot == TRUE) {
    plot(plINval[,1], plINval[,2], type="l", col="black", lwd=3, xlab="Nutrition", ylab="Income Poverty", xlim= c(0,4000), ylim= c(0, 100))


    dbINval<- subset(data, aboveINval==0)
    points(dbINval$NUval, dbINval$INval, col = "blue")


    daINval<- subset(data, aboveINval==1)
    points(daINval$NUval, daINval$INval, col= "darkgreen")


    abline(v=2685.90, col="black", lwd=3)

    abline(h=48 , col="black", lwd=3)
    lines(plINval[,1], plINval[,2], col = "black",lwd=3)
  }

  print("Funktioniert.")

  }

bivquant(varX=INval, varY=NUval, data=total1)

# Aufräumen.
rm(list = ls())
gc(reset = TRUE)



testfun = function(var1, var2){
  paste(var1)
}
testfun(var1 = test)

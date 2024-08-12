bivqfun <- function(data, var1, var2, tau = 0.5, nalpha = 100) {

  # Input checks:
  stopifnot(
    "Name of the first variable argument must be given as a character." = is.character(var1),
    "Name of the second variable argument must be given as a character." = is.character(var2),
    "Tau argument has to be given as a double" = is.double(tau),
    "Input datasource should be a dataframe." = is.data.frame(data)
  )

  # Calculate empirical cumulative distribution functions
  ecdfvar1 <- myecdf(data, var1)
  ecdfvar2 <- myecdf(data, var2)

  # Standardize the variables between 0 and 1
  data$y1 <- ecdfvar1$ecdf(data[[var1]])
  data$y2 <- ecdfvar2$ecdf(data[[var2]])

  # Prepare the data and calculate the values
  res <- prepquant(data, tau, ecdfvar1, ecdfvar2, nalpha)

  # Create interpolation functions
  bivqf <- approxfun(res$y1, res$y2, ties = "max")
  bivqforig <- approxfun(res$var1, res$var2, ties = "max")

  # Generate the bivariate quantile curve
  bivqcurve <- data.frame(
    var1 = seq(min(data[[var1]]), max(data[[var1]]), length = 250),
    y1 = seq(min(data$y1), max(data$y1), length = 250)
  )
  bivqcurve$y2 <- bivqf(bivqcurve$y1)
  bivqcurve$var2 <- bivqforig(bivqcurve$var1)

  # Remove missing values
  bivhhi <- na.omit(data.frame(cbind(bivqcurve$var1, bivqcurve$var2)))

  # Include extreme values
  faz <- c(min(bivhhi[, 1]), max(data[[var2]], na.rm = TRUE))
  faa <- c(max(data[[var1]], na.rm = TRUE) + 1, min(bivhhi[, 2]))
  plvar2 <- rbind(faz, bivhhi, faa)
  # print(plvar2[, 1])
  # Calculate abovevar2 indicator
  data$abovevar2 <- rep(0, nrow(data))
  for (i in 1:nrow(data)) {
    x <- c(data[[var1]][i], data[[var2]][i])
    if (all(x[1] > plvar2[, 1])) {
      data$abovevar2[i] <- 0
    } else {
      ind <- min(which(x[1] <= plvar2[, 1]))
      if (x[2] > plvar2[ind, 2]) {
        data$abovevar2[i] <- 1
      }
    }
  }

  output <- as.list(data)
  output <- append(output, list(plvar2 = plvar2))
  output <- append(output, list(faz = faz))
  output <- append(output, list(faa = faa))
  output <- append(output, list(tau = tau))

  print("Funktioniert.")
  class(output) <- "bivq"
  return(output)
}
#
#
plot.bivq <- function(data, ...){
  # Extract data
  plvar2  <- data$plvar2
  var1 <- data[[1]]
  var2 <- data[[2]]
  faz <- data$faz
  faa <- data$faa
  tau <- data$tau

  baz <- as.data.frame(data[1:5])

  dbvar2 <- subset(baz, abovevar2 == 0)
  davar2 <- subset(baz, abovevar2 == 1)

  plot(
    plvar2[, 1], plvar2[, 2], type = "l", col = "black", lwd = 3,
    xlab = "Variable 1", ylab = "Variable 2",
    xlim = c(min(dbvar2[[var1]]) * 0.9, faa[1] * 1.1),
    ylim = c(0, faz[2]),
    main = paste("Bivariate relative discriminator with given tau of", tau)
  )

  points(dbvar2[[var1]], dbvar2[[var2]], col = "blue")  # Problem mit dem Slicing
  points(davar2[[var1]], davar2[[var2]], col = "darkgreen")

  abline(v = faz[[1]], col = "black", lwd = 3)
  abline(h = faa[[2]], col = "black", lwd = 3)
  lines(plvar2[, 1], plvar2[, 2], col = "black", lwd = 3)
}
#
#
#
data("nutritionpoverty")
test <- bivqfun(data = nutritionpoverty, var1="NUval", var2="INval", tau = 0.25)
# test <- bivqfun(data = pov_line_data_wt, var1="leisure", var2="inc_expenses", tau = 0.25)
# # test
plot(test)
# #
# # #------------------------------------
# # #
# # # Debugging
# # #
# # #
# # # -----------------------------------
#
# data = nutritionpoverty
# var1="NUval"
# var2="INval"
# tau = 0.25
# nalpha = 100
#
# #----- main function
#
# # Calculate empirical cumulative distribution functions
# ecdfvar1 <- myecdf(data, var1)
# ecdfvar2 <- myecdf(data, var2)
#
# # Standardize the variables between 0 and 1
# data$y1 <- ecdfvar1$ecdf(data[[var1]])
# data$y2 <- ecdfvar2$ecdf(data[[var2]])
#
# # Prepare the data and calculate the values
# res <- prepquant(data, tau, ecdfvar1, ecdfvar2, nalpha)
#
# # Create interpolation functions
# bivqf <- approxfun(res$y1, res$y2, ties = "max")
# bivqforig <- approxfun(res$var1, res$var2, ties = "max")
#
# # Generate the bivariate quantile curve
# bivqcurve <- data.frame(
#   var1 = seq(min(data[[var1]]), max(data[[var1]]), length = 250),
#   y1 = seq(min(data$y1), max(data$y1), length = 250)
# )
# bivqcurve$y2 <- bivqf(bivqcurve$y1)
# bivqcurve$var2 <- bivqforig(bivqcurve$var1)
#
# # Remove missing values
# bivhhi <- na.omit(data.frame(cbind(bivqcurve$var1, bivqcurve$var2)))
#
# # Include extreme values
# faz <- c(min(bivhhi[, 1]), max(data[[var2]], na.rm = TRUE))
# faa <- c(max(data[[var1]], na.rm = TRUE) + 1, min(bivhhi[, 2]))
# plvar2 <- rbind(faz, bivhhi, faa)
# # print(plvar2[, 1])
# # Calculate abovevar2 indicator
# data$abovevar2 <- rep(0, nrow(data))
# for (i in 1:nrow(data)) {
#   x <- c(data[[var1]][i], data[[var2]][i])
#   if (all(x[1] > plvar2[, 1])) {
#     data$abovevar2[i] <- 0
#   } else {
#     ind <- min(which(x[1] <= plvar2[, 1]))
#     if (x[2] > plvar2[ind, 2]) {
#       data$abovevar2[i] <- 1
#     }
#   }
# }
#
# output <- as.list(data)
# output <- append(output, list(plvar2 = plvar2))
# output <- append(output, list(faz = faz))
# output <- append(output, list(faa = faa))
# output <- append(output, list(tau = tau))
#
# print("Funktioniert.")
# class(output) <- "bivq"
#
#
# data <- output
#
# plvar2  <- data$plvar2
# var1 <- data[1]
# var2 <- data[2]
# faz <- data$faz
# faa <- data$faa
# tau <- data$tau
# abovevar2 <- data$abovevar2
#
#
# dbvar2 <- subset(data, abovevar2 == 0)
# davar2 <- subset(data, abovevar2 == 1)
#
# plot(
#   plvar2[, 1], plvar2[, 2], type = "l", col = "black", lwd = 3,
#   xlab = "Variable 1", ylab = "Variable 2",
#   xlim = c(min(dbvar2$y1) * 0.9, faa[1] * 1.1),
#   ylim = c(0, faz[2]),
#   main = paste("Bivariate relative discriminator with given tau of", tau)
# )
#
# points(dbvar2[[var1]], dbvar2[[var2]], col = "blue")
# points(davar2[[var1]], davar2[[var2]], col = "darkgreen")
#
# abline(v = faz[[1]], col = "black", lwd = 3)
# abline(h = faa[[2]], col = "black", lwd = 3)
# lines(plvar2[, 1], plvar2[, 2], col = "black", lwd = 3)

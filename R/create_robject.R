#' Create a Custom Object of Class 'robject'
#'
#' This function creates a custom object of class \code{"robject"}, which stores a numeric vector of variables.
#'
#' @param variables A numeric vector containing the variables to be stored in the object.
#' @return An object of class \code{"robject"}.
#'
#' @details
#' The \code{"robject"} class is a simple container for numeric data. It includes methods for printing
#' and plotting the stored data as a histogram.
#'
#' @export

create_robject <- function(variables) {
  if (!is.numeric(variables)) {
    stop("The input 'variables' must be a numeric vector.")
  }

  # Create the object as a list with a class
  robject <- list(variables = variables)
  class(robject) <- "robject"
  return(robject)
}

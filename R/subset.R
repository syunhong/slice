# ------------------------------------------------------------------------------
# subset()
#
# Description:
# Function subset_ASP can extract data which user wants. 
# It requires 2 argument, ASpaces and status
# ASpaces is the data to be subsetted(class of ASpaces)
# status is list of expression indicating variables and specific values
# ex) list(sex = 1, yrborn = 1949:1990, income = 3)
#
# Author: Changlock Choi (hihi7100@khu.ac.kr)
#
# Last updated: 2020-03-05
# ------------------------------------------------------------------------------
subset <- function(x, condition, vars, all = TRUE, showProgress = TRUE) {
  
  # ----------------------------------------------------------------------------
  # Is 'x' a valid ASpaces object?
  # ----------------------------------------------------------------------------
  if (!inherits(x, "ASpaces"))
    stop("'x' must be of class ASpaces", call. = FALSE)
  else
    data.list <- slot(x, "data")
  
  # ----------------------------------------------------------------------------
  # If 'condition' is given:
  # ----------------------------------------------------------------------------
  if (!missing(condition)) {
    if (showProgress & require(pbapply)) {
      cat("Finding elements satisfying the given conditions\n")
      INDEX <- pbsapply(data.list, function(z) .select(z, condition, all))
    } else {
      INDEX <- sapply(data.list, function(z) .select(z, condition, all))
    }
      
    data.list <- data.list[INDEX]
  }
  
  # ----------------------------------------------------------------------------
  # If 'vars' is given:
  # ----------------------------------------------------------------------------
  if (!missing(vars)) {
    if (showProgress & require(pbapply)) {
      cat("Dropping the variables not included in 'vars'\n")
      data.list <- pblapply(data.list, function(z) {
        z@info <- z@info[vars]
        return(z)
      })
    } else {
      data.list <- lapply(data.list, function(z) {
        z@info <- z@info[vars]
        return(z)
      })
    }
  }
  
  output <- update(x, data = data.list)
  return(output)
}
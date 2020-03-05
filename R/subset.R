# ------------------------------------------------------------------------------
# subset()
#
# Subsets the Input object of Class ASpaces
#
# Description:
#
#
#
# Usage:
#
#   subset(x, condition, vars, all = TRUE, showProgress = TRUE)
#
# Arguments:
# 
#   x             an object of class ASpacest
#
#   condition     a list.
#
#   vars          an optional character vector.
#
#   all           logical.
#
#   showProgress  logical. If TRUE, a progress bar appears on the R console 
#                 while iterating.
#
# Details:
# 
# 
# 
# Value:
#
#   An object of class ASpaces
#
# Author(s):
#
#   Changlock Choi (hihi7100@khu.ac.kr), Seong-Yun Hong (syhong@khu.ac.kr)
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
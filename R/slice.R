# ------------------------------------------------------------------------------
# slice()
# 
# Description:
# 
# 
# Author: Seong-Yun Hong (syhong@khu.ac.kr)
#
# Last updated: 2020-03-05
# ------------------------------------------------------------------------------
slice <- function(x, at, vars, showProgress = TRUE, na.rm = TRUE, 
                  silent = FALSE) {

  # ----------------------------------------------------------------------------
  # Is 'x' a valid ASpaces object?
  # ----------------------------------------------------------------------------
  if (!inherits(x, "ASpaces"))
    stop("'x' must be of class ASpaces", call. = FALSE)
  else
    df <- slot(x, "data")

  # ----------------------------------------------------------------------------
  # If 'at' is not given, stop the function
  # ----------------------------------------------------------------------------
  if (missing(at))
    stop("'at' must be provided", call. = FALSE)
    
  # ----------------------------------------------------------------------------
  # If 'vars' are not specified, return all variables in the input data
  # ----------------------------------------------------------------------------
  if (missing(vars))
    vars <- names(slot(df[[1]], "info"))

  if (showProgress & require(pbapply))
    output <- pblapply(df, FUN = function(z, vars) {
      info <- as.data.frame(z@info, stringsAsFactors = FALSE)
      trip <- .locate(z@trip, info$id, at, na.rm, silent)
      output <- cbind(info[vars], trip)
      return(output)
      })
  else
    output <- lapply(df, FUN = function(z, vars) {
      info <- as.data.frame(z@info, stringsAsFactors = FALSE)
      trip <- .locate(z@trip, info$id, at, na.rm, silent)
      output <- cbind(info[vars], trip)
      return(output)
    })

  output.unlist <- unlist(output)
  output.length <- length(vars) + 4

  output.df <- data.frame(matrix(output.unlist, ncol = output.length, 
                                 byrow = TRUE), 
                          stringsAsFactors = TRUE)

  names(output.df) <- names(output[[1]])
  
  return(output.df)
}
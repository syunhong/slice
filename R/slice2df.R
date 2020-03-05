# ------------------------------------------------------------------------------
# slice2df()
#
# Summarises the Input Data Frame by Area and an Optional Grouping Variable
#
# Description:
#
#
#
# Usage:
#
#   slice2df(x, var1, var2)
#
# Arguments:
# 
#   x       a data.frame object
#
#   var1    a character vector of length 1, indicating the name of the column 
#           in 'x' that contains area codes or names. This argument must be 
#           provided.
#
#   var2    an optional character vector of length 1, indicating the name of a
#           grouping variable
#
# Details:
# 
# 
# 
# Value:
#
#   A data.frame that is suitable for the functions in the seg package
#
# Author(s):
#
#   Seong-Yun Hong (syhong@khu.ac.kr)
# ------------------------------------------------------------------------------
slice2df <- function(x, var1, var2) {
  area <- x[,var1]
  
  # ----------------------------------------------------------------------------
  # If there are no optional grouping variables:
  # ----------------------------------------------------------------------------
  if (missing(var2)) {
    output <- data.frame(table(area))
    names(output) <- c("area", "count")
  } 
  
  # ----------------------------------------------------------------------------
  # If there is an optional grouping variable:
  # ----------------------------------------------------------------------------
  else {
    by <- x[,var2]
    tmp.tb <- table(area, by)
    tmp.df <- data.frame(rbind(tmp.tb))
    colnames(tmp.df) <- colnames(tmp.tb)
    
    output <- cbind(area = rownames(tmp.df), tmp.df)
    rownames(output) <- 1:nrow(output)
  }
  
  return(output)
}

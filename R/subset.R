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
subset <- function(x, status){
  output <- lapply(slot(x, "data"), function(z) .exclude(z, status))
  na.index <- lapply(output, function(z) typeof(z) == "S4")
  na.index <- do.call(rbind.data.frame, na.index)
  output <- output[na.index[, 1]]
  new.ASP <- update(x, data = output)
  return(new.ASP)
}
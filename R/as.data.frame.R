# ------------------------------------------------------------------------------
# as.data.frame()
#
# Coerces the Input Object of Class ASpaces to a Data Frame Object
#
# Description:
# 
# 
# 
# Usage:
#
#   slice(x, at, vars, showProgress = TRUE, na.rm = TRUE, silent = FALSE)
#
# Arguments:
# 
#   x             an object of class ASpaces
#
#   at            a numeric vector of length 1, specifying the time at which 
#                 the location of an individual needs to be extracted
#
#   vars          an optional character vector. See Details.
#
#   showProgress  logical. If TRUE, a progress bar appears on the R console 
#                 while iterating.
#
#   na.rm         logical. If TRUE, trip records with NA values will be removed
#                 and ignored. If FALSE, an error will be generated.
#
#   silent        logical. If TRUE, if any trip records have NAs in the time
#                 fields, a warning message will be provided.
#
# Details:
#
#   The slice() function attempts to retrieve the locations of people in the 
#   input object 'x' at the given time instant 'at'.
#
#   The function looks into the slot 'trip' in each of the elements in the
#   slot 'data' of the object 'x' (i.e., x@data[[i]]@trip where i = {1 ... n}).
#
#   The 'trip' slot contains a sequence of trips made by each person, and it is
#   assumed that the trips are sorted in ascending order of the departure time
#   (i.e., $o_time). If NOT, this function may not work as intended.
# 
# Value:
#
#   A data.frame
#
# Author(s):
#
#   Seong-Yun Hong (syhong@khu.ac.kr)
# ------------------------------------------------------------------------------
as.data.frame <- function(ASpaces, remove.notrip = TRUE) {
  
  df <- slot(ASpaces, "data")
  
  ID <- which(sapply(df, function(z) nrow(z@trip)) == 0)
  
  if (length(ID) > 0) {
    warning("the following elements have no trips:\n", 
            paste(ID, collapse = " "), "\n", call. = FALSE)    
  }

  if (remove.notrip) {
    df <- df[-ID]
  } else {
    df[ID] <- lapply(df[ID], function(z) { z@trip[1,] <- NA; return(z) })
  }
  
  output <- lapply(df, function(z) cbind(z@info, z@trip))
  output <- do.call(rbind, output)
  
  return(output)
}

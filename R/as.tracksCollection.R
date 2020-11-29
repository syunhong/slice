# ------------------------------------------------------------------------------
# as.tracksCollection()
#
# Coerces the Input Object of Class ASpaces to a tracksCollection Object
#
# Description:
# 
# 
# 
# Usage:
#
#   as.tracksCollection(x, sp, varname)
#
# Arguments:
# 
#   x             an object of class ASpaces
#
#   sp            an object of class sp used in x
#
#   varname       name of the column in sp's data.frame which can be matched 
#                 with o_zone and d_zone in ASpaces
#
# Details:
#
# 
# Value:
#
#   An object of class tracksCollection
#
# Author(s):
#
#   Seong-Yun Hong (syhong@khu.ac.kr)
# ------------------------------------------------------------------------------
as.tracksCollection <- function(x, sp, varname) {

  # ----------------------------------------------------------------------------
  # If 'varname' is not given, stop the function
  # ----------------------------------------------------------------------------
  if (missing(varname))
    stop("'varname' must be provided", call. = FALSE)
  
  # ----------------------------------------------------------------------------
  # If 'sp' is missing, take the sp from given ASpaces "x"
  # ----------------------------------------------------------------------------
  if (missing(sp)){
    sp <- x@sp
  warning("Since sp is not given, the spatial data in x is used", 
          call. = FALSE)
  }
  
  tracks_list <- lapply(x@data, function(z) try(as.Tracks(z, sp, varname), TRUE))
  errorID <- sapply(tracks_list, function(z) class(z) == "try-error")
  
  return(list(tracks_list, errorID))
}


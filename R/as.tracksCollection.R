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
#   sp            
#
#   varname       
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

  tracks_list <- lapply(x@data, function(z) try(as.Tracks(z, sp, varname), TRUE))
  errorID <- sapply(tracks_list, function(z) class(z) == "try-error")
  
  return(list(tracks_list, errorID))
}
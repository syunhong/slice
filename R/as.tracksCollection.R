# ------------------------------------------------------------------------------
# as.tracksCollection()
# 
# Date: 2019-08-22
# Author: Seong-Yun Hong (syhong@khu.ac.kr)
# ------------------------------------------------------------------------------
as.tracksCollection <- function(x, sp, varname) {

  tracks_list <- lapply(x@data, function(z) try(as.Tracks(z, sp, varname), TRUE))
  errorID <- sapply(tracks_list, function(z) class(z) == "try-error")
  
  return(list(tracks_list, errorID))
}
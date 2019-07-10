# ------------------------------------------------------------------------------
# slice()
# 
# Date: 2018-12-01
# Author: Seong-Yun Hong (syhong@khu.ac.kr)
# ------------------------------------------------------------------------------
slice <- function(x, at, vars) {
  
  output <- lapply(x, FUN = function(z, at, vars) {
    info.df <- as.data.frame(z@info, stringsAsFactors = FALSE)
    if (!missing(vars))
      info.df <- info.df[vars]
    
    trip.df <- .locate(z@trip, at, complete.only = FALSE)
    output <- cbind(info.df, trip.df)
    return(output)    
  })
  
  output
}
# ------------------------------------------------------------------------------
# slice()
# 
# Date: 2018-12-01
# Author: Seong-Yun Hong (syhong@khu.ac.kr)
# ------------------------------------------------------------------------------
slice <- function(x, at, vars) {
  if (missing(vars))
    vars <- names(x[[1]]@info)
  
  output <- lapply(x, FUN = function(z) {
    info.df <- as.data.frame(z@info, stringsAsFactors = FALSE)
    info.df <- info.df[vars]
    
    trip.df <- .locate(z@trip, at)
    output <- cbind(info.df, trip.df)
    return(output)    
  })
  
  result.df <- data.frame(matrix(unlist(output), ncol = 19, byrow = TRUE), 
                          stringsAsFactors = FALSE)
  names(result.df) <- names(output[[1]])
  
  return(result.df)
}
# ------------------------------------------------------------------------------
# slice()
# 
# Date: 2018-12-01
# Author: Seong-Yun Hong (syhong@khu.ac.kr)
# ------------------------------------------------------------------------------
slice <- function(data, at) {
  
  output <- lapply(data, function(z) slice.person(z, at))
  return(output)
}
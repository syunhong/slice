# ------------------------------------------------------------------------------
# slice.person()
# 
# Author: Seong-Yun Hong (syhong@khu.ac.kr)
# Date: 2018-12-01
# Description: 
# This function takes a list object as an input and returns a data frame. It
# uses an internal function .extract(), .expand() and .locate().
# ------------------------------------------------------------------------------
slice.person <- function(person, at) {
  
  x <- .extract(person, "individual")
  y <- .extract(person, "household")
  z <- .extract(person, "trips")
  z <- .expand(z)
  z <- .locate(z, at)
  
  cbind(x, y, z)
}
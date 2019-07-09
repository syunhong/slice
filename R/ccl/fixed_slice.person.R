# ------------------------------------------------------------------------------
# slice() fixed
#
# Date : 2019.05.31
# ------------------------------------------------------------------------------

slice.person <- function(person, at) { ### apply with class "AS"
  
  x <- .extract(person, "info")
  y <- .extract(person, "trip")
  y <- .expand(y)
  y <- .locate(y, at)
  
  cbind(x, y)
}



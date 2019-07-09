# ------------------------------------------------------------------------------
# ## subset.ASP
#
# Date : 2019.06.14
# ------------------------------------------------------------------------------

subset.ASP <- function(ASP, status){
  output <- lapply(slot(ASP, "data"), function(z) .exclude(z, status))
  na.index <- lapply(output, function(z) typeof(z) == "S4")
  na.index <- do.call(rbind.data.frame, na.index)
  output <- output[na.index[, 1]]
  new.ASP <- new("ASpaces", output)
  return(new.ASP)
  
}

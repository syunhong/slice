# ------------------------------------------------------------------------------
# ## subset.asl
#
# Date : 2019.06.14
# ------------------------------------------------------------------------------

subset.asl <- function(ASL, status){
  output <- lapply(slot(ASL, "people"), function(z) .exclude(z, status))
  na.index <- lapply(output, function(z) typeof(z) == "S4")
  na.index <- do.call(rbind.data.frame, na.index)
  output <- output[na.index[, 1]]
  new.ASL <- new("ASL", output)
  return(new.ASL)
  
}
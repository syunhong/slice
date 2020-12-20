# ------------------------------------------------------------------------------
# .checkslicemc
##
# an internal function written by:
#
# Changlock Choi (hihi7100@khu.ac.kr)
# ------------------------------------------------------------------------------

.checkslicemc <- function(core){
  
  if((class(core) != "numeric") )
    stop("'core' must be integer value of 1 length",
         call. = FALSE)
  if((core%%1!=0))
    stop("'core' must be integer value of 1 length",
         call. = FALSE)
  if(!length(core) == 1)
    stop("'core' must be integer value of 1 length",
         call. = FALSE)
  if(is.na(core))
    stop("'core' must be integer value fo 1 length",
         call. = FALSE)
  if(core > detectCores()){
    core <- detectCores() - 1
  cat("number of core is higher than the given environment,
core is replaced with other value (maximum core - 1)")
  }
  return(core)
}


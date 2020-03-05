# ------------------------------------------------------------------------------
# .select()
#
# An internal function written by:
#
#   Changlock Choi (hihi7100@khu.ac.kr)
#   Seong-Yun Hong (syhong@khu.ac.kr)
# ------------------------------------------------------------------------------
.select <- function(x, condition, all){
  
  INFO <- slot(x, "info")
  VARS <- names(condition)
  
  set1 <- unlist(INFO[VARS])
  set2 <- unlist(condition)
  
  # This error should never happen!
  if (length(set1) != length(set2))
    stop("'condition' includes variables that do not exist in the slot 'info'",
         call. = FALSE)
  
  if (all) 
    result <- all(set1 == set2)
  else
    result <- any(set1 == set2)
  
  return(result)
}

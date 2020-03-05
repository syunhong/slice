# ------------------------------------------------------------------------------
# .select()
#
# Date: 2019-09-16
# Author: Changlock Choi (hihi7100@khu.ac.kr)
#
# .exclude returns list to figure out which person's data had to be
# extracted.
# 1) if someone's data satisfy the status which user set at the first of using
# subset.ASL, it returns all value.
# 2) but if data didn't satisfy all of the status, it returns NA value
# 
# there is a little different in how it works for info and trip
#
# info has only 1 length -> it just have the one boolean data for one status
# -> check any of it has false -> if there is false return NA, there is no false
# return all value
#
# but trip can have multiple length for one status. -> check row by row ->
# if there is false on some rows -> exclude that rows -> return ramainders
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

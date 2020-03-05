# ------------------------------------------------------------------------------
# .exclude()
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

.exclude <- function(person, status){
  n_person <- person
  infoindex <- NULL
  tripindex <- NULL
  info <- slot(person, "info")
  trip <- slot(person, "trip")
  
  
  # ----------------------------------------------------------------------------
  # check name of status and compare real value with the status
  # ----------------------------------------------------------------------------
  for(i in 1:length(status)){
    if(names(status)[i] %in% names(info)){
      ## combine boolean values for info
      index <- n_person@info[names(status)[i]] %in% status[[i]]
      infoindex <- rbind(infoindex, index)
      
      
      ## combine boolean values for trip
    } else if(names(status)[i] %in% names(trip)){
      index <- with(trip, eval(parse(text = names(status[i]))) %in% status[[i]])
      tripindex <- rbind(tripindex, index)
      
      
    }
  }
  
  if (FALSE %in% infoindex){
    ## if there is false on info, returns NA
    n_person <- NA
    return(n_person)
    break
  }
  
  
  # ----------------------------------------------------------------------------
  # exclude trip data which does not satisfy all the status
  # ----------------------------------------------------------------------------
  f_tripindex <- NULL
  if(length(tripindex) != 0){
    for(i in 1:ncol(tripindex)){
      
      ## if there is FALSE, exclude that trip
      if(FALSE %in% tripindex[, i]){
        f_tripindex[i] <- FALSE
      } else {
        f_tripindex[i] <- TRUE
      }
    }
    slot(n_person, "trip") <- slot(person, "trip")[f_tripindex, ]
  }
  return(n_person)
}
## subset.asl
subset.asl <- function(ASL, status){
  output <- lapply(slot(ASL, "people"), function(z) .exclude(z, status))
  na.index <- lapply(output, function(z) typeof(z) == "S4")
  na.index <- do.call(rbind.data.frame, na.index)
  output <- output[na.index[, 1]]
  new.ASL <- new("ASL", output)
  return(new.ASL)
  
}

.exclude <- function(person, status){
  n_person <- person
  infoindex <- NULL
  tripindex <- NULL
  info <- slot(person, "info")
  trip <- slot(person, "trip")
  
  for(i in 1:length(status)){
    if(names(status)[i] %in% slotNames(info)){
      index <- slot(info, names(status)[i]) == status[[i]]
      infoindex <- rbind(infoindex, index)
    } else if(names(status)[i] %in% names(trip)){
      index <- with(trip, eval(parse(text = names(status[i]))) == status[[i]])
      tripindex <- rbind(tripindex, index)
    }
  }
     
  if (FALSE %in% infoindex){
    n_person <- NA
    return(n_person)
    break
  }
  
  f_tripindex <- NULL
  if(length(tripindex) != 0){
  for(i in 1:ncol(tripindex)){
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



####

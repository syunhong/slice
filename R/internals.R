# ------------------------------------------------------------------------------
# .extract()
# ------------------------------------------------------------------------------
.extract <- function(person, type = "household") {
  
  output <- NULL
  
  if (type == "household") {
    df <- person$household
    output <- data.frame(income = df$income[1],
                         housing = df$houseType[1],
                         size = df$familySize[1],
                         vehicle = df$hasCar[1],
                         subway = df$timeStation[1],
                         bus = df$timeBusStop[1])
  } else if (type == "individual") {
    df <- person$individual
    output <- data.frame(status = df$status[1],
                         birth = df$birth[1], 
                         sex = df$sex[1],
                         occupation = df$occupation[1],
                         employment = df$workingHour[1],
                         workingday = df$workingDays[1],
                         license = df$hasLicense[1])
  } else if (type == "trips") {
    df <- person$trips  
    output <- data.frame(from = df$o.time, 
                         to = df$d.time,
                         oID = df$o.adminID, 
                         oType = df$o.type,
                         dID = df$d.adminID, 
                         dType = df$d.type,
                         purpose = df$purpose,
                         mode = df$mode, 
                         with = df$with)
  }
  
  return(output)
}

# ------------------------------------------------------------------------------
# .expand()
# ------------------------------------------------------------------------------
.expand <- function(trip) {
  
  if (nrow(trip) > 1) {
    trip <- trip[order(trip$from),]
    
    last <- length(trip$from)
    i1 <- 1:(last - 1)
    i2 <- i1 + 1
    
    INDEX <- which(trip$from[i2] != trip$to[i1])
    
    if (length(INDEX) > 0) {
      output <- list(from = trip$to[INDEX],
                     to = trip$from[INDEX + 1],
                     oID = trip$dID[INDEX], oType = trip$dType[INDEX],
                     dID = trip$dID[INDEX], dType = trip$dType[INDEX],
                     purpose = rep(NA, length(INDEX)),
                     mode = rep(NA, length(INDEX)), 
                     with = rep(NA, length(INDEX)))
      
      output <- rbind(trip, as.data.frame(output))
      output <- output[order(output$from),]
      rownames(output) <- 1:nrow(output)
      
      return(output)
    } else {
      return(trip)
    }
  } else {
    return(trip)
  }
}

# ------------------------------------------------------------------------------
# .locate()
# ------------------------------------------------------------------------------
.locate <- function(trip, at) {
  
  last <- nrow(trip)
  print(trip$from[1])
  print(trip$to[last-1])
  print(trip[last,])
  
  if (is.na(trip$from[1])) {
    output <- data.frame(from = 0, to = 2359, 
                         oID = NA, oType = 1, 
                         dID = NA, dType = 1, 
                         purpose = NA, mode = NA, with = NA)
  }
  
  else if (at < trip$from[1] | at >= trip$to[last]) {
    output <- data.frame(from = trip$to[last], to = trip$from[1],
                         oID = trip$dID[last], oType = trip$dType[last],
                         dID = trip$dID[last], dType = trip$dType[last],
                         purpose = NA, mode = NA, with = NA)
  } 
  
  else {
    INDEX <- which((at >= trip$from) & (at < trip$to))
    if (length(INDEX) == 0)
      stop("There is nothing at the given moment")
    else if (length(INDEX) > 1)
      stop("There is more than one trip at the same time")
    else
      output <- trip[INDEX,]
  }
  
  return(output)
}
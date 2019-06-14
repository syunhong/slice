# ------------------------------------------------------------------------------
# slice() fixed
#
# Date : 2019.06.07
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# slice() fixed
#
# Date : 2019.06.07
# ------------------------------------------------------------------------------

##.extract() fixed

.extract <- function(person, type = "info"){
  output <- NULL
  
  if (type == "info") {
    infolist <- slot(person, "info")
    output <- data.frame(hid = slot(infolist, "hid"),
                         pid = slot(infolist, "pid"),
                         famrel = slot(infolist, "famrel"),
                         yrborn = slot(infolist, "yrborn"),
                         sex = slot(infolist, "sex"),
                         area = slot(infolist, "area"),
                         income = slot(infolist, "income"),
                         occ = slot(infolist, "occ"),
                         emp = slot(infolist, "emp"),
                         hhsize = slot(infolist, "hhsize"),
                         dutype = slot(infolist, "dutype"),
                         haslic = slot(infolist, "haslic"),
                         hascar = slot(infolist, "hascar"),
                         tstn = slot(infolist, "tstn"),
                         tbst = slot(infolist, "tbst"))
  } else if (type == "trip") {
    triplist <- slot(person, "trip")
    if(nrow(triplist) == 0){
      triplist <- data.frame(tr_seq = NA, purpose = NA, mode = NA,
                             o_type = NA, o_time = NA, o_zone = NA,
                             d_type = NA, d_time = NA, d_zone = NA, stay = 0)
    }
    output <- data.frame(tr_seq = as.character(triplist$tr_seq),
                         purpose = triplist$purpose,
                         mode = triplist$mode,
                         o_type = triplist$o_type,
                         o_time = as.numeric(as.character(triplist$o_time)),
                         o_zone = triplist$o_zone,
                         d_type = triplist$d_type,
                         d_time = as.numeric(as.character(triplist$d_time)),
                         d_zone = triplist$d_zone,
                         stay = 0,
                         stringsAsFactors = FALSE)
  }
  
  return(output)
}

## .expand() fixedt

.expand <- function(trip) {
  if(nrow(trip) > 1) {
    trip <- trip[order(trip$o_time),]
    
    last <- length(trip$o_time)
    i1 <- 1:(last - 1)
    i2 <- i1 + 1
    
    INDEX <- which(trip$o_time[i2] != trip$d_time[i1])
    
    if (length(INDEX) > 0) {
      output <- data.frame(tr_seq = 1:length(INDEX),
                           purpose = trip$purpose[INDEX],
                           mode = rep(NA, length(INDEX)),
                           o_type = trip$d_type[INDEX],
                           o_time = as.numeric(trip$d_time)[INDEX],
                           o_zone = trip$d_zone[INDEX],
                           d_type = trip$d_type[INDEX],
                           d_time = as.numeric(trip$o_time)[INDEX + 1],
                           d_zone = trip$d_zone[INDEX],
                           stay = 1)
      
      
      output <- rbind(trip, as.data.frame(output))
      output <- output[order(output$o_time),]
      rownames(output) <- 1:nrow(output)
      
      trip <- output
    } 
  }
  if(is.na(trip$d_zone[1]) == FALSE) {
    final <- length(trip$d_time)
    output2 <- data.frame(tr_seq = "last",
                          purpose = trip$purpose[final],
                          mode = rep(NA, length(final)),
                          o_type = trip$d_type[final],
                          o_time = as.numeric(trip$d_time)[final],
                          o_zone = trip$d_zone[final],
                          d_type = trip$d_type[final],
                          d_time = as.numeric(trip$o_time)[1],
                          d_zone = trip$d_zone[final],
                          stay = 1)
    
    trip <- rbind(trip, as.data.frame(output2))
    
    rownames(trip) <- 1:nrow(trip)
    return(trip)
  } else {
    return(trip)
  }
}


## .locate()

.locate <- function(trip, at) {
  
  last <- nrow(trip)
  
  
  if(is.na(trip$o_time[1])) {
    output <- data.frame(tr_seq = 0, purpose = NA, mode = NA, o_type = 1,
                         o_time = 0, o_zone = NA, d_type = 1, d_time = 2359,
                         d_zone = NA, stay = 1)
  }
  
  else if (at < trip$o_time[1] | at >= trip$d_time[last - 1]) {
    output <- trip[last,]
  }
  
  else {
    INDEX <- which((at >= trip$o_time) & (at < trip$d_time))
    if (length(INDEX) == 0)
      stop("There is nothing at the given moment")
    else if (length(INDEX) > 1){
      output <- trip[INDEX, ][1, ]
      print("There is more than one trip at the same time")
    } else
      output <- trip[INDEX,]
  }
  
  return(output)
}

## .exclude()

.exclude <- function(person, status){
  n_person <- person
  infoindex <- NULL
  tripindex <- NULL
  info <- slot(person, "info")
  trip <- slot(person, "trip")
  
  for(i in 1:length(status)){
    if(names(status)[i] %in% slotNames(info)){
      index <- slot(info, names(status)[i]) %in% status[[i]]
      infoindex <- rbind(infoindex, index)
    } else if(names(status)[i] %in% names(trip)){
      index <- with(trip, eval(parse(text = names(status[i]))) %in% status[[i]])
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

## .transh() by myunghoon
.transh <- function(number) {
  number <- gsub("-", "", number)
  n <- nchar(number)
  if(n == 8){
    if(number %in% gatong$분류코드){
      b <- subset(gatong, gatong[, 4] == number)
      c <- b[,6] 
    }else{
      print("잘못 입력하셨습니다1.")
    }
  }else if(n == 10){
    if(number %in% bjd[, 8]){
      b <- subset(bjd, bjd[, 8] == number)
      c <- b[, 6] 
    }else if(number %in% bjd[, 7]){
      b <- subset(bjd, bjd[, 7] == number)
      c <- b[1, 6] 
    }else{
      print("잘못 입력하셨습니다2.")
    }
  }else{
    print("잘못 입력하셨습니다3.")
  }
  return(c)
}
#slice()

slice <- function(data, at, attr = c("hid", "pid")) {
<<<<<<< HEAD
  
  output <- lapply(slot(data, "people"), function(z) slice.person(z, at))
  df.output <- do.call(rbind.data.frame, output)
  attrfixed <- attr
=======
  output <- lapply(slot(data, "people"), function(z) slice.person(z, at))
  df.output <- do.call(rbind.data.frame, output)
>>>>>>> e68c4cdf5aa120166fc566ef532501708151ee6e
  if("trip" %in% attr){
    attrfixed <- c(attr, names(data@people[[1]]@trip), "stay")
    attrfixed <- attrfixed[!attrfixed == "trip"]
  }
  df.output <- df.output[order(as.numeric(rownames(df.output))), 
                         c("area", attrfixed)]
<<<<<<< HEAD
  df.output[, 1] <- .transh(df.output[, 1])
=======
>>>>>>> e68c4cdf5aa120166fc566ef532501708151ee6e
  return(df.output)
}
## slice.person()
slice.person <- function(person, at) {
  
  x <- .extract(person, "info")
  y <- .extract(person, "trip")
  y <- .expand(y)
  y <- .locate(y, at)
  
  cbind(x, y)
}


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

## .expand() fixed
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
    output2 <- data.frame(tr_seq = final,
                          purpose = trip$purpose[final],
                          mode = rep(NA, length(final)),
                          o_type = trip$d_type[final],
                          o_time = as.numeric(trip$d_time)[final],
                          o_zone = trip$d_zone[final],
                          d_type = trip$d_type[final],
                          d_time = as.numeric(trip$o_time)[final],
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
<<<<<<< HEAD
                         o_time = 0, o_zone = NA, d_type = 1, d_time = 2359,
=======
                         o_time = 0, o_zone = NA, d_type = 1, d_time = 0,
>>>>>>> e68c4cdf5aa120166fc566ef532501708151ee6e
                         d_zone = NA, stay = 1)
  }
  
  else if (at < trip$o_time[1] | at >= trip$d_time[last-1]) {
    output <- trip[last-1,]
  }
  
  else {
    INDEX <- which((at >= trip$o_time) & (at < trip$d_time))
    if (length(INDEX) == 0)
      stop("There is nothing at the given moment")
    else if (length(INDEX) > 1)
      stop("There is more than one trip at the same time")
    else
      output <- trip[INDEX,]
  }
  
  return(output)
}

<<<<<<< HEAD

## .transh() by myunghoon

.transh <- function(number) {
  number <- gsub("-", "", number)
  tong <- NULL
  for(i in 1:length(number)){
    a <- as.character(number[i])
    n <- nchar(a)
    if(n == 8){
      if(a %in% gatong$분류코드){
        b <- subset(gatong, gatong[, 4] == a)
        c <- b[,6] 
      }else{
        print("잘못 입력하셨습니다.")
      }
    }else if(n == 10){
      if(a %in% bjd[, 8]){
        b <- subset(bjd, bjd[, 8] == a)
        c <- b[, 6] 
      }else if(a %in% bjd[, 7]){
        b <- subset(bjd, bjd[, 7] == a)
        c <- b[1, 6] 
      }else{
        print("잘못 입력하셨습니다.")
      }
    }else{
      print("잘못 입력하셨습니다.")
    }
    tong <- rbind(tong, c)
  }
  return(tong)
}

=======
>>>>>>> e68c4cdf5aa120166fc566ef532501708151ee6e

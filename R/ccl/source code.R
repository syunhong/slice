##sourcecode

# ------------------------------------------------------------------------------
# Class 'Personal'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------
setClass("Personal", slots = c(hid = "character", pid = "numeric", 
                               famrel = "numeric", 
                               yrborn = "numeric", sex = "numeric", 
                               area = "character", income = "numeric", 
                               occ = "numeric", emp = "numeric", 
                               hhsize = "numeric", dutype = "numeric", 
                               haslic = "logical", hascar = "logical",
                               tstn = "numeric", tbst = "numeric"))

setMethod("initialize", "Personal", 
          function(.Object, hid, pid, famrel, yrborn, sex, area, income, occ, 
                   emp, hhsize, dutype, haslic, hascar, tstn, tbst, ...) {
            
            .Object <- callNextMethod(.Object, ...)
            
            # [[1-1]] Household ID
            if (missing(hid))
              .Object@hid <- character()
            else
              .Object@hid <- hid
            
            # [[1-2]] Personal ID
            if (missing(pid))
              .Object@pid <- numeric()
            else
              .Object@pid <- pid
            
            # [[2]] Relationship to family
            # https://cps.ipums.org/cps-action/variables/FAMREL#codes_section
            if (missing(famrel))
              .Object@famrel <- numeric()
            else
              .Object@famrel <- famrel
            
            # [[3]] Birth year
            if (missing(yrborn))
              .Object@yrborn <- numeric()
            else
              .Object@yrborn <- yrborn
            
            # [[4]] Sex
            if (missing(sex))
              .Object@sex <- numeric()
            else
              .Object@sex <- sex
            
            # [[5]] Area code
            if (missing(area))
              .Object@area <- character()
            else
              .Object@area <- area
            
            # [[6]] Income
            if (missing(income))
              .Object@income <- numeric()
            else
              .Object@income <- income
            
            # [[7]] Occupation
            if (missing(occ))
              .Object@occ <- numeric()
            else
              .Object@occ <- occ
            
            # [[8]] Employment status
            if (missing(emp))
              .Object@emp <- numeric()
            else
              .Object@emp <- emp
            
            # [[9]] Household size
            if (missing(hhsize))
              .Object@hhsize <- numeric()
            else
              .Object@hhsize <- hhsize
            
            # [[10]] Dwelling unit type
            if (missing(dutype))
              .Object@dutype <- numeric()
            else
              .Object@dutype <- dutype
            
            # [[11]] Do you have a driver's license?
            if (missing(haslic))
              .Object@haslic <- logical()
            else
              .Object@haslic <- haslic
            
            # [[12]] Do you have a car?
            if (missing(hascar))
              .Object@hascar <- logical()
            else
              .Object@hascar <- hascar
            
            # [[13]] Time to the nearest subway station
            if (missing(tstn))
              .Object@tstn <- numeric()
            else
              .Object@tstn <- tstn
            
            # [[14]] Time to the nearest bus stop
            if (missing(tbst))
              .Object@tbst <- numeric()
            else
              .Object@tbst <- tbst
            
            .Object
          })

validityPersonal <- function(object) {
  
  if (object@yrborn < 0)
    paste("'yrborn' must be greater than 0")
  else if (object@hhsize < 0)
    paste("'hhsize' must be greater than 0")
  else if (!is.logical(object@haslic))
    paste("'haslic' must be logical")
  else if (!is.logical(object@hascar))
    paste("'hascar' must be logical")
  else if (object@tstn < 0)
    paste("'tstn' must be greater than 0")
  else if (object@tbst < 0)
    paste("'tbst' must be greater than 0")
  else
    TRUE
}

validityPersonal <- function(object) {
  TRUE
}

setValidity("Personal", validityPersonal)


# ------------------------------------------------------------------------------
# Class 'AS'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------
setClass("AS", slots = c(info = "Personal", trip = "data.frame"))

setMethod("initialize", "AS", 
          function(.Object, info, trip, ...) {
            
            .Object <- callNextMethod(.Object, ...)
            
            fn <- c("tr_seq", "purpose", "mode", "o_type", "o_time", "o_zone", 
                    "d_type", "d_time", "d_zone")
            
            if (missing(info))
              .Object@info <- new("Personal")
            else if (class(info) == "Personal" & validObject(info))
              .Object@info <- info
            else
              stop("invalid 'info' object", call. = FALSE)
            
            if (missing(trip))
              .Object@trip <- data.frame(tr_seq = NA, purpose = NA, mode = NA,
                                         o_type = NA, o_time = NA, o_zone = NA,
                                         d_type = NA, d_time = NA, d_zone = NA)
            else if (is.data.frame(trip) & all(names(trip) == fn))
              .Object@trip <- trip
            else
              stop("'trip' is not structured as required", call. = FALSE)
            
            .Object
          })

validityAS <- function(object) {
  
}

setValidity("AS", validityAS)


# ------------------------------------------------------------------------------
# Class 'ASL'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------
setClass("ASL", slots = c(people = "list"))

setMethod("initialize", "ASL", 
          function(.Object, people, ...) {
            
            .Object <- callNextMethod(.Object, ...)
            
            if (missing(people))
              .Object@people <- list(new("AS"))
            else if (is.list(people))
              .Object@people <- people
            else
              stop("'people' must be of class list", call. = FALSE)
            
            .Object
          })

validityASL <- function(object) {
  
  if (all(lapply(x@people, function(z) class(z)) == "AS"))
    TRUE
  else
    paste("all elements in the 'people' slot must be of class AS")
}

setValidity("ASL", validityASL)

# ------------------------------------------------------------------------------
# Methods for class 'activity'
#
# Author: Seong-Yun Hong <hong.seongyun@gmail.com>
# ------------------------------------------------------------------------------
setMethod("[[", 
          signature = "ASL", 
          definition = function(x, i) { 
            x@people[[i]] 
          })

setMethod("[[<-", 
          signature = c(x = "ASL"),
          definition = function(x, i, value) { 
            x@people[[i]] <- value
            x
          })

setMethod("show", 
          signature = "ASL", 
          definition = function(object) {
            cat("Activity space data for measuring segregation")
          })


# ------------------------------------------------------------------------------
# slice() fixed
#
# Date : 2019.05.31
# ------------------------------------------------------------------------------
### apply with class "AS"
slice <- function(data, at, attr = c("hid", "pid")) {
  
  
  output <- lapply(slot(data, "people"), function(z) slice.person(z, at))
  df.output <- do.call(rbind.data.frame, output)
  attrfixed <- attr
  
  
  if("trip" %in% attr){
    attrfixed <- c(attr, names(data@people[[1]]@trip), "stay")
    attrfixed <- attrfixed[!attrfixed == "trip"]
  }
  df.output <- df.output[order(as.numeric(rownames(df.output))), 
                         c("area", attrfixed)]
  
  df.output[, 1] <- sapply(df.output[, 1], function(z) .transh(z))
  
  return(df.output)
}

# ------------------------------------------------------------------------------
# slice() fixed
#
# Date : 2019.05.31
# ------------------------------------------------------------------------------

slice.person <- function(person, at) { ### apply with class "AS"
  
  x <- .extract(person, "info")
  y <- .extract(person, "trip")
  y <- .expand(y)
  y <- .locate(y, at)
  
  cbind(x, y)
}

# ------------------------------------------------------------------------------
# ## subset.asl
#
# Date : 2019.05.31
# ------------------------------------------------------------------------------

subset.asl <- function(ASL, status){
  output <- lapply(slot(ASL, "people"), function(z) .exclude(z, status))
  na.index <- lapply(output, function(z) typeof(z) == "S4")
  na.index <- do.call(rbind.data.frame, na.index)
  output <- output[na.index[, 1]]
  new.ASL <- new("ASL", output)
  return(new.ASL)
  
}


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

#####################################################
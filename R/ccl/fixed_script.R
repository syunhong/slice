#slice()

slice <- function(data, at, purpose, stay = TRUE) {
  if(missing(purpose)){ ## 시간만 주어진 경우
  output <- lapply(data, function(z) slice.person.t(z, at))
  } else if(missing(at)){ ## 목적만 주어진 경우
    output <- lapply(data, function(z) slice.person.p(z, purpose, stay))
    output <- .exclude(output)
    return(output)
  } else { ## 시간 목적 둘다 주어진 경우
    output <- lapply(data, function(z) slice.person.t(z, at))
    output <- filter_t(output, purpose)
  }
  return(output)
}


## slice.person()
slice.person.t <- function(person, at) {
  
  x <- .extract(person, "individual")
  y <- .extract(person, "household")
  z <- .extract(person, "trips")
  z <- .expand(z)
  z <- .locate(z, at)
  
  cbind(x, y, z)
}


##.extract()

.extract <- function(person, type = "household"){
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
                         with = df$with,
                         stay = 0)
  }
  
  return(output)
}

## .expand()

.expand <- function(trip) {
  
  if(nrow(trip) > 1) {
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
                     purpose = trip$purpose[INDEX], ## 지역에 머무는 동안의 목적
                     mode = rep(NA, length(INDEX)),
                     with = rep(NA, length(INDEX)),
                     stay = 1)
      
      output <- rbind(trip, as.data.frame(output))
      output <- output[order(output$from),]
      rownames(output) <- 1:nrow(output)
      
      trip <- output
    } 
  }
  if(is.na(trip$dID[1]) == FALSE) {
  final <- length(trip$to)
  output2 <- list(from = trip$to[final],
                  to = trip$from[1],
                  oID = trip$dID[final], 
                  oType = trip$dType[final],
                  dID = trip$dID[final], 
                  dType = trip$dType[final],
                  purpose = trip$purpose[final],
                  mode = NA,
                  with = NA,
                  stay = 1)
  if(output2$dType != 1){
    output2$stay <- 2
    output2$purpose <- NA
  }
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

  
  if(is.na(trip$from[1])) {
    output <- data.frame(from = 0, to = 2359,
                         oID = NA, oType = 1,
                         dID = NA, dType = 1,
                         purpose = NA, mode = NA, with = NA, stay = NA)
  }
  
  else if (at < trip$from[1] | at >= trip$to[last-1]) {
    output <- trip[last-1,]
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



###################################
####2018/12/03 2010 gagutonghang
##load(file.choose())
##convert <- read.csv(file.choose()) ##첨부된 dongcode table 


##x는 원래의 list입니다
##y는 동코드로써, 1열에는 가통자료에서의 동 코드, 
##2열에는 실제 행정동 및 법정동 코드가 들어갑니다.


ConvertCode <- function(x, y){
  convert <- y
  colnames(convert) <- c("출발동코드", "출발동행정동코드")
  a <- lapply(x, function(f) 
    merge(f[[3]], convert, by = "출발동코드")
  )
  print(paste0('출발동코드 is finished'))
  colnames(convert) <- c("도착동코드", "도착동행정동코드")
  b <- lapply(a, function(f) 
    merge(f, convert, by = "도착동코드")
  )
  print(paste0('도착동코드 is finished'))
  for(i in 1:length(b))
    x[[i]][[3]]<-b[[i]]  
  return(x)
}

##쓸 필요가 있는가?
sudo17 <- readOGR(file.choose(), encoding = "UTF-8")
wow <- slot(sudo17, "data")$EMD_CD
##################################

load(file.choose())

a <- chuchool[1:1000]
a <- lapply(a, function(z) {
  hehe <- z
  names(hehe) <- c("household", "individual", "trips")
  return(hehe)}
)


slice1 <- slice(a, purpose = 4)



amtable <- as.data.frame(table(b[18,]))
wowowow <- b[, which(b[14, ] > b[15, ])]











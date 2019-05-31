#slice()

slice <- function(data, at, purpose, stay = TRUE) {
  if(missing(purpose)){ ## 시간만 주어진 경우
    output <- lapply(data@people, function(z) slice.person.t(z, at, purpose))
    
  } else if(missing(at)){ ## 목적만 주어진 경우
    output <- lapply(data, function(z) slice.person.t(z, purpose, stay))
    output <- .exclude(output)
    return(output)
  } else { ## 시간 목적 둘다 주어진 경우
    output <- lapply(data, function(z) slice.person.t(z, at))
    output <- filter_t(output, purpose)
  }
  return(output)
}

## slice.person()
slice.person.t <- function(person, at, purpose) {
  
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
trip <- y
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
    output <- data.frame(from = 0, to = 2359,
                         oID = NA, oType = 1,
                         dID = NA, dType = 1,
                         purpose = NA, mode = NA, with = NA, stay = NA)
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











# ------------------------------------------------------------------------------
# ASPtoTC()
#
# Date : 2019. 07. 15
# ------------------------------------------------------------------------------
### apply with class "AS"

ASPtoTC <- function(ASpaces, adm){
  if(missing(ASpaces)){
    stop(errorCondition("There is no ASpaces object"))
  } else if(missing(adm)){
    stop(errorCondition("There is no reference code for local connections(adm)"))
  } else{
    a <- lapply(ASpaces@data, function(z) {  ##checking time format in ASpaces
      a <- z@trip[, c("o_time", "d_time")]
      a <- c(as.character(a[, 1]), as.character(a[, 2]))
      return(a)
    })
    
    timecheck <- unlist(a)
    if(all(nchar(timecheck) == 4)){
      timecheck <- timecheck
    } else if(any(nchar(timecheck) != 4)){
      tryCatch(timecheck1 <- as.POSIXct(timecheck), 
               error = function(e){
                 stop(errorCondition("The time value has not appropriate format"))
               })
    }
    if(length(ASpaces@sp@bbox) == 0){
      stop(errorCondition("There is no shp in ASpaces object"))
    }
    shp <- ASpaces@sp
    coordi <- coordinates(ASpaces@sp)
    coordi <- cbind.data.frame(adm, coordi)
    colnames(coordi) <- c("zone", "x", "y")
    date <- substr(ASpaces@attr$date, start = 1, stop = 10)
    
    
    
    trackslist <- lapply(ASpaces@data, function(a) {
      
      index <- unique(a@trip$tr_id)
      triptong <- list()
      for(i in index){
        triptong[[i]] <- a@trip[which(a@trip$tr_id == i), ]
      }
      
      
      tracklist <- lapply(triptong, function(z){
        first <- rbind(as.matrix(z[, c("o_time", "o_zone", "o_type")]), 
                       as.matrix(z[nrow(z), c("d_time", "d_zone", "d_type")]))
        first <- cbind(1:nrow(first), first)
        colnames(first) <- c("id", "time", "zone", "type")
        second <- merge(first, coordi, by = "zone", all.x = T, all.y = F)
        second <- second[order(second$id), ]
        times <- sapply(as.character(second$time), function(f) {
          hour <- substr(f, start = 1, stop = nchar(f) - 2)
          min <- substr(f, start = nchar(f) - 1, stop = nchar(f))
          posixct <- as.POSIXct(as.numeric(hour)*3600 + as.numeric(min)*60,
                                origin = date, tz = "GMT")
          return(posixct)
        })
        second$time <- as.POSIXct("1970-01-01") + times
        sppoint <- SpatialPoints(cbind.data.frame(x = second$x, y = second$y), 
                                 proj4string = wow@sp@proj4string)
        stidf <- STIDF(sppoint, time = second$time, 
                       data = second[, c("zone", "type")])
        tr <- Track(stidf, df = z[, 3:4])
        
        return(tr)
      })
      tryCatch(tracklist <- Tracks(tracklist, tracksData = a@info),
               error = function(e) {}
      )
      
      return(tracklist)
    }
    )
    
    test <- lapply(trackslist, function(z) return(length(z)))
    trackslist <- trackslist[-which(unlist(test) == 0)] ## remove 0 length trip
    
    TC <- TracksCollection(trackslist)
    cat(length(which(unlist(test) == 0)),
        "objects are omitted because of no trip data")
    return(TC)
  }
}
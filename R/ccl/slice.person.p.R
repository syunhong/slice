##slice.person.p()

slice.person.p <- function(person, purpose, stay) {
  
  x <- .extract(person, "individual")
  y <- .extract(person, "household")
  z <- .extract(person, "trips")
  z <- .expand(z)
  z <- .combine(z)
  output <- cbind(x,y,z)
  output <- .filter_p(output, purpose, stay)
}

##머무는 경우와 이동하는 경우 두개를 상정함




##.combine()
.combine <- function(trip) {
  INDEX1 <- which(trip$stay == 1 | trip$stay == 2)
  INDEX2 <- c(1, INDEX1[-length(INDEX1)] + 1)
  INDEX3 <- INDEX1 - 1
  
  if(length(INDEX1) > 1){
  combine1 <- trip[INDEX1, ]
  combine2 <- cbind(trip[INDEX2, c(1,3,4,6,7,8,9,10)],
                    trip[INDEX3, c(2,5)])
  combine2 <- combine2[, colnames(combine1)]
  output <- rbind(combine1, combine2)
  
  output <- output[order(output$from), ]
  } else {
    output <- trip
  }
  t <- output$to - output$from
  t[which(t < 0)] <- t[which(t < 0)] + 2400
  output <- cbind(output, t)
  return(output)
}
  
##.filter_p()
.filter_p <- function(data, purpose, stay){
  if(stay == TRUE){
    output <- data[which(data$purpose %in% purpose),]
    if(length(output$purpose) > 0){
      output <- output[which(output$t == max(output$t)), ]
      if(length(output$purpose) > 1){
        output <- output[1, ]
      }
    } else {
      output <- NA
    }
    return(output)
  } else {
    output <- data[which(data$purpose %in% purpose & data$stay == 0),]
    if(length(output$purpose) > 0){
      output <- output[which(output$t == max(output$t)), ]
      if(length(output$purpose) > 1){
        output <- output[1, ]
      }
    } else {
      output <- NA
    }
    return(output)
  }
}

##.exclude()

.exclude <- function(data){
  m <- as.data.frame(lapply(data, function(z) return(z[1])))
  data <- data[-which(is.na(m))]
  
}

#############
chuchool <- lapply(chuchool, function(z) {
  hehe <- z
  names(hehe) <- c("household", "individual", "trips")
  return(hehe)}
)
haha <- lapply(chuchool[1:5000], function(z) slice.person.p(z, purpose))

test2 <- output[[1]]
wow <- test2$to - test2$from
wow[which(wow < 0)] <- wow[which(wow < 0)] + 2400
wow
sum(wow)



haha <- slice(a, 1300)
haha1 <- slice(a, 1300, 3)
haha2 <- slice(a, purpose = 4)
data <- a
purpose <- 4
stay <- TRUE
output <- lapply(data, function(z) slice.person.p(z, purpose, stay))
output <- .exclude(output)





##another internals/ ccl

## .division()

.division <- function(data, by){
  trans <- matrix(unlist(data), nrow = length(data[[1]]))
  i <- which(colnames(data[[1]]) == by)
  divide <- unique(trans[i, ])
  c <- data.frame(oID = NA, number = NA)
  for(k in 1:length(divide)){
    a <- trans[, trans[i, ] == divide[k]]
    b <- as.data.frame(table(a[16, ]))
    colnames(b) <- c("oID", divide[k])
    c <- merge(c, b, all.x = TRUE, all.y = TRUE)
  }
  c <- c[-nrow(c), -2]
  c <- apply(c, 2, function(z){
    z[which(is.na(z))] <- 0
    z <- as.numeric(z)
    return(z)
  }
  )
  c[, 1] <- as.character(c[, 1])
  return(as.data.frame(c))
}


##.filter()
.filter <- function(result, purpose){
  if(is.na(result$purpose)){
    return(NA)
  } else if(result$purpose %in% purpose){
    return(result$purpose)
  } else {
    return(NA)
  }
}

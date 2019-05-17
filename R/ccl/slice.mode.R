##통행수단 거르기

slice.mode <- function(data, mode){
  output <- lapply(data, function(z) slice.test(z, mode))
  m <- as.data.frame(lapply(output, function(z) return(z[1,1])))
  output <- output[-which(is.na(m))]
}

slice.test <- function(person, mode){
  x <- .extract(person, "individual")
  y <- .extract(person, "household")
  z <- .extract(person, "trips")
  output <- cbind(x,y,z)
  output <- output[which(output$mode == mode), ]
  return(output[1, ])
}


haha <- slice.mode(a, 1)

translice(haha)


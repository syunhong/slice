###인구 통계학적 특징 추가

slice.demography <- function(data, demography){
  output <- lapply(data, function(z) slice.person.d(z, demography))
  output <- .exclude(output)
}

slice.person.d <- function(person, demography){
  
  x <- .extract(person, "individual")
  y <- .extract(person, "household")
  z <- .extract(person, "trips")
  output <- cbind(x,y,z)
  if(with(output, eval(parse(text = demography)))[1] == FALSE)
    output <- data.frame(NA)
  return(output[1, ])
}


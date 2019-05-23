##filter_t()

filter_t <- function(result, purpose) {
  output2 <- lapply(result, function(z) .filter(z, purpose))
  output2 <- as.data.frame(output2)
  INDEX <- which(output2 %in% purpose)
  return(result[INDEX])
}


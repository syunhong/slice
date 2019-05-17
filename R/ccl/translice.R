
### 2019-05-03

## translice()

translice <- function(data, by){
  ## data = slice의 결과물
  ## by = 무슨 변수로 데이터를 나눌지
  if(missing(by)){
    ## 이거 같은 경우 단순히 인구수를 table화 함
    trans <- matrix(unlist(data), nrow = length(data[[1]]))
    adm <- as.data.frame(table(trans[16, ]))
    colnames(adm) <- c("oID", "number")
    return(adm)
  } else { 
    adm <- .division(data, by)
    return(adm)
  }
  
}

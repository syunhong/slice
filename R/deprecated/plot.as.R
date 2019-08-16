# ------------------------------------------------------------------------------
# plot.as()
#
# Date : 2019.07.03
# ------------------------------------------------------------------------------
### apply with class "AS"



plot.as <- function(as, shp, adm_cd, ...){
  a <- .extract(as, type = "trip")
  b <- .expand(a)
  uni <- unique(substr(b[, 1], start = 1, stop = 1))
  new.b <- NULL
  for(i in 1:(length(uni)-1)){
    index <- which(substr(b[, 1], start = 1, stop = 1) == i)
    new.b <- rbind(new.b, b[index[1], ])
    new.b <- rbind(new.b, b[index[length(index)], ])
  }
  new.b[nrow(new.b), ] <- b[nrow(b), ]
  new.b[, 6] <- sapply(new.b[, 6], function(z) .transh(z))
  new.b[, 9] <- sapply(new.b[, 9], function(z) .transh(z))
  original.time <- new.b[, 5]
  new.b[, 5] <- as.numeric(substring(new.b[, 5], first = nchar(new.b[, 5]) - 1, 
                                 last = nchar(new.b[, 5]))) / 60 + 
    as.numeric(substring(new.b[, 5], first = 1,
                         last = nchar(new.b[, 5]) - 2))
  
  par(fig = c(0, 1, 0, 1))
  plot.new()
  plot.window(xlim = shp@bbox[1, ], ylim = shp@bbox[2, ])
  par(mar = c(1, 4.1, 4, 2.1), oma = c(0, 0, 1, 0), fig = c(0.1,0.9,0.25, 1),
      new = TRUE)
  
  col.map <- rep("white", length(shp))
  col.map[which(adm_cd %in% new.b[, 6])] <- "red"
  plot(shp, col = col.map, border = "white", add = TRUE)
  plot(shp, border = "grey70", add = TRUE)
  
  z <- cbind(adm_cd, coordinates(shp))
  shp.index <- NULL
  for(i in 1:nrow(new.b)){
    wow <- which(adm_cd %in% new.b[i, 6])
    wow1 <- which(adm_cd %in% new.b[i, 9])
    shp.index <- rbind(shp.index, wow)
  }
  for(i in 1:(length(shp.index) - 1)){
    lines(x = c(as.numeric(z[, 2][shp.index[i]]), 
                as.numeric(z[, 2][shp.index[i+1]])), 
           y = c(as.numeric(z[, 3][shp.index[i]]), 
                 as.numeric(z[, 3][shp.index[i+1]]))
    )
  }
  
  
  par(fig = c(0, 1, 0.05, 0.25), mar = c(0,0,0,0))
  plot.window(xlim = c(new.b[1, 5] - 2, new.b[nrow(new.b),5] + 2), 
              ylim = c(0, 3),
              xaxs = "i", yaxs = "i")
  
  lines(x = c(new.b[1, 5] - 1, 
              new.b[nrow(new.b),5] + 1), y = c(1.5, 1.5), lwd = 2)
  lines(x = c(new.b[1, 5] - 1, 
              new.b[1, 5] - 1), y = c(1.4, 1.6), lwd = 2)
  lines(x = c(new.b[nrow(new.b),5] + 1, 
              new.b[nrow(new.b),5] + 1), y = c(1.4, 1.6), lwd = 2)
  text(x = new.b[1, 5] - 1, y = 1.3, labels = "0")
  text(x = new.b[nrow(new.b), 5] + 1, y = 1.3, labels = "24")
  for(i in 1:nrow(new.b)){
    lines(x = c(new.b[i, 5], new.b[i, 5]), y = c(1.45, 1.55))
    text(x = new.b[i, 5], y = 1.7, labels = as.character(new.b[i, 2]))
    text(x = new.b[i, 5], y = 1.3, labels = as.character(original.time[i]))
  }
  
}

# ------------------------------------------------------------------------------
# plot.asl()
#
# Date : 2019.07.03
# ------------------------------------------------------------------------------
### apply with class "ASL"

plot.asl <- function(asl, shp, adm, col, n){
  area_list <- lapply(asl@people, function(z){slot(slot(z, "info"), "area")})
  ha <- as.data.frame(area_list)
  a <- sapply(ha, function(z) .transh(z))
  b <- as.data.frame(table(a))
  colnames(b)[1] <- "adm_cd"
  c <- data.frame(adm_cd = adm)
  c <- cbind(1:424, c)
  
  newdf <- merge(c, b, by = "adm_cd", all.x = T, all.y = F, sort = F)
  newdf <- newdf[order(newdf[, 1]), ]
  newdf[is.na(newdf[, 3]), 3] <- 0
  my_colors <- col
  class_of_freq <- cut(newdf$Freq, 
                       breaks = quantile(newdf$Freq, probs = seq(0, 1, 1/n)))
  my_colors <- my_colors[as.numeric(class_of_freq)]
  plot(shp, col = my_colors)

}


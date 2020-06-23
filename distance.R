distance <- function(lat1, long1, lat2, long2, A=6378, B=6357) {
  
  ################## distance #########################
  # 
  # M. Martin
  #
  # Computes point to point distance
  #
  ######################################################
  
  E <- sqrt(1 - B^2/A^2)
  rad <- pi/180.
  lat1 <- lat1 * rad
  lat2 <- lat2 * rad  
  long1 <- long1 * rad
  long2 <- long2 * rad
  lat.dif <- lat2 - lat1
  long.dif <- long2 - long1
  L <- mean(c(lat1, lat2))
  R1 = A * (1 - E^2) / (1 - E^2 * sin(L)^2)^(3/2)
  R2 = A / sqrt(1 - E^2 * sin(L)^2)
  Ravg = A * sqrt(1 - E^2) / (1 - E^2 * (sin(L))^2)
  a <- sin(lat.dif/2) * sin(lat.dif/2) + cos(lat1) * cos(lat2) * sin(long.dif/2) * sin(long.dif/2) 
  c <- 2 * atan2(sqrt(a), sqrt(1-a)) 
  d <- Ravg * c
  d[is.nan(d)] <- 0
  d
  
}

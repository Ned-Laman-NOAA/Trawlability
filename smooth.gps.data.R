smooth.gps.data <- function (gps.data, cross.180 = FALSE){
  
  #######################  smooth.gps.data  ############################
  #
  # N. Laman
  # March 2015
  #
  # This program will create smoothed tow paths for the lines files 
  # necessary for Globe and for old tow paths plotted in ArcGIS
  #
  ######################################################################
  
  smooth.gps.arc <- NA
  
  if(any(sign(gps.data$longitude)!=sign(gps.data$longitude[1]))){
    gps.data$longitude[sign(gps.data$longitude) == -1] <- 180 + (180 - abs(gps.data$longitude[sign(gps.data$longitude) == -1]))
    cat("Tow crosses 180 degrees.\n")
    cross.180 <- TRUE
  }
  
  ## I changed the original kts definition because it was failing because there were too many knots
  # kts = round(length(gps.data$date_time)/5,0)
  if(length(gps.data$date_time) <= 1){
    next
  }else{
    kts <- ifelse(length(gps.data$date_time) >= 2 & length(gps.data$date_time) < 10, 2, 10)
  }
  
  lat.spline <- smooth.spline(as.numeric(difftime(gps.data$date_time, gps.data$date_time[1])), gps.data$latitude, all.knots = FALSE, nknots = kts)
  long.spline <- smooth.spline(as.numeric(difftime(gps.data$date_time, gps.data$date_time[1])), gps.data$longitude, all.knots = FALSE, 
                               nknots = kts)
  smooth.lat <- predict(lat.spline, as.numeric(difftime(gps.data$date_time, gps.data$date_time[1], units = "secs")))$y
  smooth.long <- predict(long.spline, as.numeric(difftime(gps.data$date_time, gps.data$date_time[1], units = "secs")))$y
  smooth.gps <- data.frame(gps.data$date_time, smooth.lat, smooth.long)
  names(smooth.gps) <- c("date_time", "latitude", "longitude")
  smooth.gps.arc <- smooth.gps
  
  if(cross.180){
    smooth.gps.arc$longitude[smooth.gps.arc$longitude > 180] <- smooth.gps.arc$longitude[smooth.gps.arc$longitude > 180] - 360
  }
  
  smooth.gps$latitude <- smooth.gps$latitude * pi/180
  smooth.gps$longitude <- smooth.gps$longitude * pi/180
  list(smooth.gps, smooth.gps.arc)
  
}

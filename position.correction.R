position.correction <- function(hj, start.lon, start.lat, end.lon, end.lat, bearing, depth, wire_out){
  
  # this function accepts start and end positions in decimal degrees and uses them 
  # along with a fixed bearing between on and off bottom (linear towpath), bottom depth, and wire out 
  # to adjust the vessel position to become trawl position behind the vessel
  
  # use Pythagorean theorem to estimate distance of net behind vessel (that's what the -1 is for)
  # wire length and depth are in meters from RACEBase
  dist.behind <- sqrt(wire_out^2 - depth^2) * -1
  
  # convert lat/lons to radians
  start.lon <- start.lon*pi/180
  start.lat <- start.lat*pi/180
  end.lon <- end.lon*pi/180
  end.lat <- end.lat*pi/180
  
  ## estimate trawl net position behind vessel
  c.start.lat <- asin(sin(start.lat) * cos(dist.behind/6367449) + cos(start.lat) * sin(dist.behind/6367449) * cos(bearing * pi/180))
  c.end.lat <- asin(sin(end.lat) * cos(dist.behind/6367449) + cos(end.lat) * sin(dist.behind/6367449) * cos(bearing * pi/180))
  # do lats input into these positions need to be in radians or deceimal degrees?
  c.start.lon <- start.lon + atan2(sin(bearing * (pi/180)) * sin(dist.behind/6367449) * cos(start.lat), cos(dist.behind/6367449) - sin(start.lat) * sin(c.start.lat))
  c.end.lon <- end.lon + atan2(sin(bearing * (pi/180)) * sin(dist.behind/6367449) * cos(end.lat), cos(dist.behind/6367449) - sin(end.lat) * sin(c.end.lat))
  
  # convert back to decimal degrees
  c.start.lon <- c.start.lon*180/pi
  c.start.lat <- c.start.lat*180/pi
  c.end.lon <- c.end.lon*180/pi
  c.end.lat <- c.end.lat*180/pi
  
  out.vec <- c(hj, c.start.lon, c.start.lat, c.end.lon, c.end.lat)
  
}

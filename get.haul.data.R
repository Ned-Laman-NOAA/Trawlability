get.haul.data <- function(channel){
  
  RACEBase.HAUL.qry <- "select cruisejoin, hauljoin, region, vessel, cruise, haul,
  haul_type, performance, start_time, duration, distance_fished, net_width, net_measured, 
  net_height, stratum, start_latitude, end_latitude, start_longitude, end_longitude, stationid,
  gear_depth, bottom_depth, bottom_type, surface_temperature, gear_temperature, wire_length, gear, 
  accessories from racebase.haul where region = 'AI' and gear = 172"
  
  RACEBase.HAUL.data <- sqlQuery(channel = channel, query = RACEBase.HAUL.qry)
  names(RACEBase.HAUL.data) <- casefold(names(RACEBase.HAUL.data))
  
  RACEBase.HAUL.data
  
}
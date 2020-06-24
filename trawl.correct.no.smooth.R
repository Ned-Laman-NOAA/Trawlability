trawl.correct <- function(){
  
  # this function will produce corrected On/Off Bottom positions for straight line towpaths corrected for trawl position behind vessel
  # and will produce corrected positions along the towpath between On/Off Bottom positions, saving both as separate csvs "linepath" and "towpath" named
  
  require(RODBC)
  require(geosphere)
  
  options(digits = 9)
  
  # Supply username and password to Oracle 
  afsc <- odbcConnect(dsn = "AFSC", uid = rstudioapi::showPrompt(title = "Username", message = "Oracle Username", default = "lamane"), 
                      pwd = rstudioapi::askForPassword("Enter Oracle Password"), believeNRows = FALSE)
  
  # get data from Oracle
  RACEBase.HAUL.data <- get.haul.data(afsc)
  haul.dat <- RACEBase.HAUL.data
  
  # head(RACEBase.HAUL.data)
  
  print("Getting towpaths from RACE_DATA and RACE_EDIT...")
  ################## consider changing this to an Oracle data call
  # these data were pre-queried, SQLPlus can be found @F:/Manuscripts/Baker_Trawlability/GettingPositionData.sql
  RACE_DATA.towpaths <- read.csv("C:/Users/ned.laman/Desktop/Baker Products/AI2006_2018_towpaths.csv", header = T)
  # RACE_DATA records have negative haul numbers when they are ported into RACEBase
  RACE_DATA.towpaths$hauljoin <- -RACE_DATA.towpaths$hauljoin
  RACE_EDIT.towpaths <- read.csv("C:/Users/ned.laman/Desktop/Baker Products/AI1991_2004_towpaths.csv", header = T)
  
  # Combine towpath data from RACE_DATA and RACE_EDIT into a single data frame
  path.dat <- rbind(RACE_DATA.towpaths, RACE_EDIT.towpaths)
  
  close(afsc)
  
  ####################### for testing ############################
  # haul.dat <- haul.dat[haul.dat$hauljoin %in% c(-18060, -16079), ]
  # path.dat <- path.dat[path.dat$hauljoin %in% c(-18060, -16079), ]
  
  
  # create null data frames to accept appended rows from from loops below
  corrected.obfb <- data.frame(hauljoin = numeric(), c_start_lon = numeric(), c_start_lat = numeric(), c_end_lon = numeric(), c_end_lat = numeric(), stringsAsFactors = FALSE)
  hj.names <- names(corrected.obfb)
  
  corrected.position <- data.frame(hauljoin = numeric(), c_lon_1 = numeric(), c_lat_1 = numeric(), c_lon_2 = numeric(), c_lat_2 = numeric(), stringsAsFactors = FALSE)
  col.names <- names(corrected.position)
  
  print("Correcting GPS data for position of net behind vessel...")
  
  for(hj in haul.dat$hauljoin){
    
    # this outer loop will correct linear towpath
    
    ## subset by hauljoin
    hj.dat <- haul.dat[haul.dat$hauljoin == hj, ]
    pos.dat <- path.dat[path.dat$hauljoin == hj, ]
    
    # limit to hauls with at least 100 positions
    # this should only eliminate around 20 hauls based on pre-screen
    if(length(pos.dat$hauljoin) < 100){next}
    
    # what if there's position data but no haul?
    if(unique(pos.dat$hauljoin) != hj.dat$hauljoin){next}
    
    # compute bearing between positions, accepts data in decimal degrees
    B <- geosphere::bearing(c(hj.dat$end_longitude, hj.dat$end_latitude), c(hj.dat$start_longitude, hj.dat$start_latitude))
    
    # call position.correction function
    haul.vec <- position.correction(hj = hj.dat$hauljoin, start.lon = hj.dat$start_longitude, start.lat = hj.dat$start_latitude, 
                                    end.lon = hj.dat$end_longitude, end.lat = hj.dat$end_latitude, bearing = B, depth = hj.dat$bottom_depth, wire_out = hj.dat$wire_length)
    ## Haul-specific parameters
    
    corrected.obfb <- rbind(corrected.obfb, haul.vec)
    
    print(paste0("Correcting towpath for hauljoin ", hj, "..."))
    
    i = 2
    
    for(i in 2:(length(pos.dat$hauljoin))){
      
      # index is built to compare the second position to the previous position so direction of travel is in line with vessel travel
      start.lat <- pos.dat$latitude[i]
      start.lon <- pos.dat$longitude[i]
      end.lat <- pos.dat$latitude[i-1]
      end.lon <- pos.dat$longitude[i-1]
      
      ## create vector of corrected position identified by hauljoin
      pos.vec <- position.correction(hj, start.lon = start.lon, start.lat = start.lat, end.lon = end.lon, end.lat = end.lat, bearing = B,
                                     depth = hj.dat$bottom_depth, wire_out = hj.dat$wire_length)
      
      ## append corrected positions to results data frame
      corrected.position <- rbind(corrected.position, pos.vec)
      
    }
  }
  
  # instead of looping this as above, could simply grab data and correct in mass one column vector at a time?
  # if I did this how would I represent the direction of travel from position i+1 to position i?
  
  ## apply field names to results data frame
  names(corrected.position) <- col.names
  names(corrected.obfb) <- hj.names
  
  ## merge original and corrected positions into single data frame
  # print("Merging raw and correct position data...")
  # new.haul.dat <- merge(path.dat, corrected.position)
  
  print("Writing csvs...")
  write.csv(corrected.position, "C:/Users/ned.laman/Desktop/Baker Products/AI1991_2018_towpaths_corrected.csv", row.names = F)
  write.csv(corrected.obfb, "C:/Users/ned.laman/Desktop/Baker Products/AI1991_2018_linepaths_corrected.csv", row.names = F)
  
  # write.csv(corrected.position, "F:/Manuscripts/Baker_Trawlability/Corrected Position Data/AI1991_2018_towpaths_corrected.csv", row.names = F)
  # write.csv(corrected.obfb, "F:/Manuscripts/Baker_Trawlability/Corrected Position Data/AI1991_2018_linepaths_corrected.csv", row.names = F)
  
}

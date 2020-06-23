make.lines <- function () {
  
  ################################  make.globe.lines  ########################################
  # 
  # N. Laman
  # March 2015
  # 
  # This is the main mover in the towpath series of programs.  This program creates collects
  # data, reads and writes shapefiles, calls subroutines to check and smooth GPS data, etc.
  #
  # Dependencies: maptools, shapefiles, rgdal, RODBC, calls smooth.gps.data
  #
  # Products:
  #		shapefiles (towpaths, towmids, towstarts)
  #		csvs (globepaths, globestarts)
  #
  #############################################################################################
  
  output.globe.paths.file <- "C:/Users/ned.laman/Desktop/Baker Products/globepaths.csv"
  output.globe.starts.file <- "C:/Users/ned.laman/Desktop/Baker Products/globestarts.csv"
  output.arcgis.paths.file <- "C:/Users/ned.laman/Desktop/Baker Products/towpaths"
  output.arcgis.starts.file <- "C:/Users/ned.laman/Desktop/Baker Products/arcstarts"
  output.badgps.file <- "C:/Users/ned.laman/Desktop/Baker Products/badgps.csv"
  output.midpoints.file <- "C:/Users/ned.laman/Desktop/Baker Products/midpoints"
  minutes <- 1
  mark.color.list <- c(11,3,9,1,13,5)
  
  channel <- oracle.connect.Rstudio()
  
  survey = "AI"
  
  library(maptools)
  library(shapefiles)
  library(rgdal)
  
  cat("", file = output.globe.paths.file, append = F)
  cat("", file = output.globe.starts.file, append = F)
  
  arcgis.shp <- data.frame(matrix(ncol = 3))
  names(arcgis.shp) <- c("Id","X","Y")
  arcgis.dbf <- vector()
  
  tow.mids <- data.frame(matrix(ncol = 3))
  names(tow.mids) <- c("Id","X","Y")
  
  cat("\nRetrieving haul data from Oracle...\n")
  sqlQuery(query = "drop table hauls", channel = channel)
  sqlQuery(query = paste("create table hauls as select h.cruisejoin, h.hauljoin, h.vessel, h.cruise, h.haul, h.start_time, 
		h.start_latitude, h.start_longitude, h.end_latitude, h.end_longitude, h.performance, h.duration, h.haul_type, h.stratum, 
		h.bottom_depth, gear_temperature from racebase.haul h, goa.biennial_surveys s where h.cruisejoin = s.cruisejoin and 
		gear in (172) and s.survey = '", survey, "' order by cruise, vessel, haul", sep = ""), channel = channel)
  sqlQuery(query = "drop table species_table", channel = channel)
  sqlQuery(query = paste("create table species_table as select hauljoin, species_code from goa.analysis_species, hauls 
		where sizecomp_flag = '", survey, "' or sizecomp_flag = 'BOTH'", sep = ""), channel = channel)
  haul.data <- sqlQuery(query = "select * from hauls where vessel <> 83", channel = channel)
  names(haul.data) <- tolower(names(haul.data))
  end_time <- haul.data$start_time + (haul.data$duration*3600)
  haul.data <- data.frame(haul.data, end_time)
  
  cat("\nRetrieving catch data from Oracle...\n")
  catch.data <- sqlQuery(query = "select sp.hauljoin, sp.species_code, nvl(c.weight,0) weight from racebase.catch c, 
		species_table sp where c.hauljoin(+) = sp.hauljoin and c.species_code(+) = sp.species_code order by hauljoin, species_code", 
                         channel = channel)
  names(catch.data) <- tolower(names(catch.data))
  catch.data <- merge(haul.data, catch.data, by.x = "hauljoin", by.y = "hauljoin", all.x = T)
  catch.data[is.na(catch.data$weight)] <- 0
  total.catch <- tapply(catch.data$weight, catch.data$hauljoin, sum)
  total.catch <- total.catch[match(haul.data$hauljoin, names(total.catch))]
  performance.codes <- sqlQuery(query = "select haul_performance_code, note from race_data.haul_performance_codes c,
		race_data.haul_performance_notes n where c.haul_performance_note_id = n.haul_performance_note_id", channel = channel)
  names(performance.codes) <- tolower(names(performance.codes))
  haul.performance <- performance.codes$note[match(performance.codes$haul_performance_codes, haul.data$performance)]
  vessels <- sort(unique(haul.data$vessel))
  vessel.frame <- data.frame(vessels, 1:length(vessels))
  names(vessel.frame) <- c("vessel","symbol")
  colors <- (haul.data$bottom_depth%/%100)+1
  colors <- mark.color.list[colors]
  
  if(survey == "AI"){
    stratum.depths <- haul.data$stratum%%10
    stratum.colors <- mark.color.list[stratum.depths]
    colors[is.na(colors)] <- stratum.colors[is.na(colors)]
  }
  
  if(survey == "GOA"){
    stratum.depths <- (haul.data$stratum%/%100)+1
    stratum.colors <- mark.color.list[stratum.depths]
    colors[is.na(colors)] <- stratum.colors[is.na(colors)]
  }
  
  marks.data.total.catch <- tapply(catch.data$weight, catch.data$hauljoin, sum)
  marks.data <- data.frame(haul.data[,c("start_latitude","start_longitude")] * pi/180, 
                           vessel.frame$symbol[match(haul.data$vessel, vessel.frame$vessel)], colors, haul.data$start_time, "", 
                           haul.data[,c("bottom_depth", "gear_temperature")], "", "", paste(paste(haul.data$vessel, "/", 
                                                                                                  haul.data$cruise, "/", haul.data$haul, "/", haul.data$bottom_depth, sep = ""), haul.performance, 
                                                                                            performance.codes$note[match(haul.data$performance, performance.codes$haul_performance_code)]),
                           total.catch, haul.data$start_time, haul.data$hauljoin)
  names(marks.data) <- c("Latitude","Longitude","Symbol","Color","DateTime","Tide","Depth","Temperature",
                         "Flags","Comment","Name","Catch","LastModified","hauljoin")
  write.csv(x = marks.data, file = output.globe.starts.file, row.names = F, na = "")
  ##########################
  # Seems like right here if I don't constrain to max(cruise) I should get all cruises from above
  # but is it just for ArcStarts?
  ##########################
  ## excise Mark's ArcGIS shapefiles of towstarts for only the most recent survey
  # last.cruise <- max(haul.data$cruise)
  # cruise.data <- haul.data[haul.data$cruise == last.cruise, ]
  cruise.data <- haul.data
  marks.shp <- data.frame(cruise.data$hauljoin, cruise.data[,c("start_longitude","start_latitude")])
  names(marks.shp) <- c("Id", "X", "Y")
  marks.dbf <- data.frame(cruise.data$hauljoin)
  names(marks.dbf) <- "Id"
  marks.arc <- convert.to.shapefile(marks.shp, marks.dbf, "Id", 1)
  write.shapefile(marks.arc, output.arcgis.starts.file, arcgis = T)
  marks.arc <- readShapePoints(output.arcgis.starts.file, proj4string=CRS("+proj=longlat +datum=NAD83"))
  writeOGR(marks.arc, output.arcgis.starts.file, "towstarts", driver = "ESRI Shapefile", morphToESRI = T, overwrite_layer = TRUE)
  cruises <- unique(haul.data[,c("cruisejoin", "vessel", "cruise")][order(haul.data$cruise, haul.data$vessel),])
  
  ##Put column names into the output files.
  sink(output.globe.paths.file, append = F)
  cat("Latitude,Longitude,DateTime,Color,Width,index,hauljoin\n")
  sink()
  
  for(cr in cruises$cruisejoin){
    cruise <- cruises[cruises$cruisejoin == cr,]
    
    # if(cruise$cruise < 201201) next
    
    cruise.haul <- haul.data[haul.data$cruisejoin == cr,]
    
    cat(paste("\nVessel", cruise$vessel, "Cruise", cruise$cruise, "\n"))
    cat("\nRetrieving gps data from Oracle...\n")
    
    ## Collect data from multiple sources and test using precedence rules as follows:  Final RACE_DATA data
    ## are preferred, next choice is Edit RACE_DATA, followed by RACE_EDIT.RB2 population data, followed by
    ## RACEBase point to point data.
    edit.data <- sqlQuery(channel, query = paste("select c.vessel_id, c.cruise, h.haul, p.edit_date_time, 
			(trunc(p.edit_latitude/100,0) + (mod(p.edit_latitude,100)/60)) latitude, 
			(trunc(p.edit_longitude/100,0) + (mod(p.edit_longitude,100)/60)) longitude
			from race_data.cruises c, hauls rh, race_data.edit_hauls h, race_data.edit_position_headers ph, 
			race_data.edit_positions p, race_data.datum_codes dc where c.vessel_id = rh.vessel and 
			c.cruise = rh.cruise and h.haul = rh.haul and c.cruise_id = h.cruise_id and h.haul_id = ph.haul_id 
			and ph.position_header_id = p.position_header_id and p.datum_code = dc.datum_code
			and dc.use_in_analysis = 'Y' and c.vessel_id = ", cruise$vessel, " and c.cruise = ", cruise$cruise, 
                                                 sep = ""))
    
    final.data <- sqlQuery(channel, query = paste("select c.vessel_id, c.cruise, h.haul, p.edit_date_time, 
			p.latitude, p.longitude from race_data.cruises c, hauls rh, race_data.hauls h, race_data.position_headers ph, 
			race_data.positions p, race_data.datum_codes dc where c.vessel_id = rh.vessel and 
			c.cruise = rh.cruise and h.haul = rh.haul and c.cruise_id = h.cruise_id and h.haul_id = ph.haul_id 
			and ph.position_header_id = p.position_header_id and p.datum_code = dc.datum_code
			and dc.use_in_analysis = 'Y' and c.vessel_id = ", cruise$vessel, " and c.cruise = ", cruise$cruise, 
                                                  sep = ""))
    
    # get data from RACEBASE.HAUL tables
    rb2.data <- sqlQuery(channel = channel, query = paste("select vessel, cruise, haul, date_time, latitude, longitude 
				from race_edit.rb2_gps where vessel = ", cruise$vessel, "and cruise =", cruise$cruise, 
                                                          " and datum_code = 1 order by date_time", sep = ""))
    
    
    # test if data come from the Final, Edit, or RACEBase tables
    if (length(final.data[,1]) == 0){
      if(length(edit.data[,1]) == 0){
        dat <- rb2.data
      }else{
        dat <- edit.data
      }
    }else{
      dat <- final.data
    }
    
    names(dat) <- c("vessel","cruise","haul","date_time","latitude","longitude")
    
    if(length(dat$haul) == 0){
      gps.data <- sqlQuery(channel, query = paste("select a.vessel, a.cruise, a.haul, a.start_time date_time, 
				a.start_latitude latitude, a.start_longitude longitude from racebase.haul a, hauls b where a.vessel = b.vessel 
				and a.cruise = b.cruise and a.cruise = ", cruise$cruise, " and a.haul = b.haul and a.vessel = ", cruise$vessel, 
                                                  " union select a.vessel, a.cruise, a.haul, (a.start_time + a.duration/24) date_time, a.end_latitude latitude, 
				a.end_longitude longitude from racebase.haul a, hauls b where a.vessel = b.vessel and a.cruise = b.cruise 
				and a.cruise = ", cruise$cruise, " and a.vessel = ", cruise$vessel, " and a.haul = b.haul", sep = ""))
    }else{
      gps.data <- dat
    }
    
    names(gps.data) <- c("vessel","cruise","haul","date_time","latitude","longitude")
    
    gps.hauls <- sort(unique(gps.data$haul))
    gps.avail <- cruise.haul$haul[!is.na(match(cruise.haul$haul, gps.hauls))]
    no.gps <- cruise.haul$haul[is.na(match(cruise.haul$haul, gps.hauls))]
    
    for(haul in gps.avail) {
      
      print(haul)
      tow.data <- cruise.haul[cruise.haul$haul == haul,  ]
      
      if(is.na(tow.data$end_time)){
        cat("\nNo end time for haul", haul, " - cannot estimate tow path.\n")
        next
      }
      
      start.time <- tow.data$start_time
      end.time <- tow.data$end_time
      duration <- as.numeric(difftime(end.time, start.time, units = "secs"))
      
      if(tow.data$performance == 0) globe.color <- 65382
      if(tow.data$performance < 0) globe.color <- 6684927
      if(tow.data$performance > 0) globe.color <- 16737894
      
      tow.gps <- gps.data[gps.data$haul == haul,  ]
      tow.gps <- tow.gps[order(tow.gps$date_time),]
      
      if(length(tow.gps$latitude) < 90) {
        no.gps <- sort(c(no.gps, haul))
        cat("\nNot enough gps data in haul", haul, "to use.\n")
        next
      }
      
      tow.gps.list <- smooth.gps.data(tow.gps)
      tow.gps.smooth <- tow.gps.list[[1]]
      tow.gps.smooth.arc <- tow.gps.list[[2]]
      ob.data <- (1:length(tow.gps.smooth$date_time))[tow.gps.smooth$date_time >= start.time & tow.gps.smooth$date_time <= end.time]
      
      if(length(ob.data) ==0){
        no.gps <- sort(c(no.gps, haul))
        cat("\n No overlap between gps data and tow period (> start time & < end time) - cannot estimate tow path.\n")
        next
      }
      
      starts.data <- ob.data[1:(length(ob.data)-1)]
      ends.data <- ob.data[2:length(ob.data)]
      distances <- distance(tow.gps.smooth$latitude[starts.data], tow.gps.smooth$longitude[starts.data], 
                            tow.gps.smooth$latitude[ends.data], tow.gps.smooth$longitude[ends.data])
      cum.dist <- cumsum(distances)
      tow.mid <- tow.gps.smooth.arc[starts.data[nearest(cum.dist,cum.dist[length(cum.dist)]/2)],c("latitude","longitude")]
      tow.mids <- rbind(tow.mids, c(tow.data$hauljoin, tow.mid$longitude, tow.mid$latitude))
      time.diffs <- as.numeric(difftime(tow.gps.smooth$date_time[ends.data], tow.gps.smooth$date_time[starts.data], units = "hours"))
      speeds <- distances/time.diffs
      
      if(is.na(any(speeds > 10))){
        next
      }else{
        if(any(speeds > 10)){
          
          return.gps  <- check.gps.data(tow.gps, tow.gps.smooth, output.badgps.file)
          tow.gps.smooth <- return.gps[[1]]
          tow.gps.smooth.arc <- return.gps[[2]]
        }
      }
      
      near.start <- nearest(start.time, tow.gps.smooth$date_time)
      near.end <- nearest(end.time, tow.gps.smooth.arc$date_time)
      
      if(abs(start.time - tow.gps.smooth$date_time[near.start]) < 10){
        start.globe.pos <- tow.gps.smooth[near.start,]
        start.arcgis.pos <- tow.gps.smooth.arc[near.start,]
      }else{
        start.globe.pos <- tow.data[,c("start_latitude","start_longitude")] * pi/180
        names(start.globe.pos) <- c("latitude","longitude")
        start.arcgis.pos <- tow.data[,c("start_latitude","start_longitude")]
        names(start.arcgis.pos) <- c("latitude","longitude")
      }
      
      if(abs(end.time - tow.gps.smooth$date_time[near.end]) < 10){
        end.globe.pos <- tow.gps.smooth[near.end,]
        end.arcgis.pos <- tow.gps.smooth.arc[near.end,]
      }else{
        end.globe.pos <- tow.data[,c("end_latitude","end_longitude")] * pi/180
        names(end.globe.pos) <- c("latitude","longitude")
        end.arcgis.pos <- tow.data[,c("end_latitude","end_longitude")]
        names(end.arcgis.pos) <- c("latitude","longitude")
      }
      
      globe.start <- paste(start.globe.pos$latitude, start.globe.pos$longitude, start.time, globe.color, "-1","", 
                           tow.data$hauljoin, sep = ",")
      
      if(is.na(end.globe.pos$latitude)){
        globe.end <- NA
      }else{
        globe.end <- paste(end.globe.pos$latitude, end.globe.pos$longitude, end.time, globe.color, "-1","", tow.data$hauljoin, sep = ",")
      }
      
      cat(globe.start, file = output.globe.paths.file, append = T)
      cat("\n", file = output.globe.paths.file, append = T)
      
      this.arcgis.shp <- data.frame(tow.data$hauljoin, round(start.arcgis.pos$longitude,6), round(start.arcgis.pos$latitude,6))
      arcgis.dbf <- c(arcgis.dbf, tow.data$hauljoin)
      
      if(duration > (60*minutes)) {
        periods <- 1.:floor(duration/(minutes * 60.))
        
        for(p in periods) {
          time <- tow.gps.smooth$date_time[nearest(start.time + periods[p] * (minutes * 60.), tow.gps.smooth$date_time)]
          
          if(abs(difftime(time, (start.time + periods[p] * (minutes * 60.)), units = "secs")) > 10){
            cat("No gps data during this time period - skipped\n")
            next
          }
          
          mid.globe.pos <- tow.gps.smooth[tow.gps.smooth$date_time == time,  ][1,]
          mid.arcgis.pos <- tow.gps.smooth.arc[tow.gps.smooth.arc$date_time == time,  ][1,]
          
          cat(paste(mid.globe.pos$latitude, mid.globe.pos$longitude, time, globe.color, "-1","", tow.data$hauljoin, 
                    sep = ","), file = output.globe.paths.file, append = T)
          cat("\n", file = output.globe.paths.file, append = T)
          
          this.arcgis.shp <- rbind(this.arcgis.shp, c(tow.data$hauljoin, round(mid.arcgis.pos$longitude,6), 
                                                      round(mid.arcgis.pos$latitude,6)))
          last.p <- p
        }
      }
      
      if(!is.na(globe.end)){
        cat(globe.end, file = output.globe.paths.file, append = T)
        this.arcgis.shp <- rbind(this.arcgis.shp, c(tow.data$hauljoin, round(end.arcgis.pos$longitude,6), 
                                                    round(end.arcgis.pos$latitude,6)))
      }else{
        cat("No suitable end position available - tow path is truncated\n")
      }
      
      cat("\n\n", file = output.globe.paths.file, append = T)
      
      names(this.arcgis.shp) <- names(arcgis.shp)
      
      if(any(sign(this.arcgis.shp$X)!=sign(this.arcgis.shp$X[1]))){
        this.arcgis.shp$X[sign(this.arcgis.shp$X) == -1] <- 180 + (180 - abs(this.arcgis.shp$X[sign(this.arcgis.shp$X) == -1]))
        cat("Tow crosses 180 degrees.\n")
      }
      
      arcgis.shp <- rbind(arcgis.shp, this.arcgis.shp)
    }
    
    for(haul in no.gps) {
      print(haul)
      tow.data <- cruise.haul[cruise.haul$haul == haul,  ]
      
      if(is.na(tow.data$end_time)){
        cat("\nNo end time for haul", haul, " - cannot estimate tow path.\n")
        next
      }
      
      if(is.na(tow.data$end_latitude)){
        cat("\nNo end position for haul", haul, " - cannot estimate tow path.\n")
        next
      }
      
      if(tow.data$performance == 0) globe.color <- 65382
      if(tow.data$performance < 0) globe.color <- 6684927
      if(tow.data$performance > 0) globe.color <- 16737894
      
      globe.start <- paste(tow.data$start_latitude * (pi/180.), tow.data$start_longitude * (pi/180.), tow.data$start_time, 
                           globe.color, "-1", "", tow.data$hauljoin, sep = ",")
      tow.data$start_time <- as.character(tow.data$start_time)
      tow.data$end_time <- as.character(tow.data$end_time)
      
      cat(globe.start, file = output.globe.paths.file, append = T)
      cat("\n", file = output.globe.paths.file, append = T)
      
      globe.end <- paste(tow.data$end_latitude * (pi/180), tow.data$end_longitude * (pi/180.), tow.data$end_time, globe.color, 
                         "-1", "", tow.data$hauljoin, sep = ",")
      cat(globe.end, file = output.globe.paths.file, append = T)
      cat("\n\n", file = output.globe.paths.file, append = T)
      this.arcgis.shp <- data.frame(tow.data$hauljoin, tow.data$start_longitude, tow.data$start_latitude)
      this.arcgis.shp <- rbind(this.arcgis.shp, c(tow.data$hauljoin, tow.data$end_longitude, tow.data$end_latitude))
      names(this.arcgis.shp) <- names(arcgis.shp)
      
      if(any(sign(this.arcgis.shp$X)!=sign(this.arcgis.shp$X[1]))){
        this.arcgis.shp$X[sign(this.arcgis.shp$X) == -1] <- 180 + (180 - abs(this.arcgis.shp$X[sign(this.arcgis.shp$X) == -1]))
        cat("Tow crosses 180 degrees.\n")
      }
      
      arcgis.shp <- rbind(arcgis.shp, this.arcgis.shp)
      arcgis.dbf <- c(arcgis.dbf, tow.data$hauljoin)
      tow.mids <- rbind(tow.mids, c(tow.data$hauljoin, (tow.data$start_longitude + tow.data$end_longitude)/2, (tow.data$start_latitude + tow.data$end_latitude)/2))
      
    }
  }
  
  # x <- merge(arcgis.shp, data.frame("hauljoin" = cruise.data$hauljoin), by.x = "Id", by.y = "hauljoin")
  
  arcgis.shp <- arcgis.shp[!is.na(arcgis.shp$Id),]
  arcgis.dbf <- arcgis.dbf[!is.na(arcgis.dbf)]
  arcgis.dbf <- data.frame(matrix(arcgis.dbf, ncol = 1))
  names(arcgis.dbf) <- c("Id")
  tow.mids <- tow.mids[!is.na(tow.mids$Id),]
  arcgis.shapefile <- convert.to.shapefile(arcgis.shp, arcgis.dbf, "Id", 3)
  write.shapefile(arcgis.shapefile, output.arcgis.paths.file, arcgis = T)
  arcgis.shapefile <- readShapeLines(output.arcgis.paths.file, proj4string=CRS("+proj=longlat +datum=NAD83"))
  writeOGR(arcgis.shapefile, output.arcgis.paths.file, "towpaths", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  towmids.shapefile <- convert.to.shapefile(tow.mids, arcgis.dbf, "Id", 1)
  write.shapefile(towmids.shapefile, output.midpoints.file, arcgis = T)
  towmids.shapefile <- readShapePoints(output.midpoints.file, proj4string=CRS("+proj=longlat +datum=NAD83"))
  writeOGR(towmids.shapefile, output.midpoints.file, "towmids", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  
  close(channel)
  
}

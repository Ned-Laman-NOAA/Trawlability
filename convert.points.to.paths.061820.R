# convert points into lines for ArcMap
require(shapefiles)
require(maptools)
require(rgdal)
require(geosphere)

options(stringsAsFactors = FALSE)
options(digits = 9)


# get data
path.dat <- read.csv("C:/Users/ned.laman/Desktop/Baker Products/AI1991_2018_towpaths_corrected.csv", header = T)
haul.dat <- read.csv("C:/Users/ned.laman/Desktop/Baker Products/AI1991_2018_linepaths_corrected.csv", header = T)

# eliminate NAs in incoming data ### these mostly originate from allowing hauls with missing parameters (e.g., wire length or
# bottom depth) necessary for the correction computation to make it into the data set being passed
path.dat <- path.dat[!is.na(path.dat$c_lon_1) | !is.na(path.dat$c_lat_1) | !is.na(path.dat$c_lon_2) | !is.na(path.dat$c_lat_2), ] # 7975 records eliminated 6/18/20
haul.dat <- haul.dat[!is.na(haul.dat$c_start_lon) | !is.na(haul.dat$c_start_lat) | !is.na(haul.dat$c_end_lon) | !is.na(haul.dat$c_end_lat), ] # 16 records elminated 6/18/20

# identify destination
output.arcgis.paths.file <- "C:/Users/ned.laman/Desktop/Baker Products/towpaths"
output.arcmap.lines.file <- "C:/Users/ned.laman/Desktop/Baker Products/towlines"

################################# frame data for towpaths ############################### 
# the data coming in from the CSV contain both adjacent points are pairs, I'm using just the first position of each pair
arcgis.shp <- data.frame(Id = path.dat$hauljoin, X = path.dat$c_lon_1, Y = path.dat$c_lat_1)
# eliminate NAs
arcgis.shp <- arcgis.shp[!is.na(arcgis.shp$Id),] # a data frame with 3 variables (Id, X, Y)
# just need column of unique hauljoins here 
arcgis.dbf <- data.frame(Id = unique(path.dat$hauljoin))

# old school method for creating the towpaths
# maptools::readShapeLines is deprecated
# first convert points to shapefile	where type 3 means polyLine
arcgis.shapefile <- shapefiles::convert.to.shapefile(shpTable = arcgis.shp, attTable = arcgis.dbf, field = "Id", type = 3)
# write shapefile which, give it the filename in out.name, and arcgis = T replaces "." with "\_" in column names for ArcGIS
shapefiles::write.shapefile(shapefile = arcgis.shapefile, out.name = paste0(output.arcgis.paths.file, "/towpath"), arcgis = T)
# DEPRECATED writes shapefile at end of path, but really doesn't...I think it makes a memory object accessible in the next step
# arcgis.shapefile <- maptools::readShapeLines(output.arcgis.paths.file, proj4string=CRS("+proj=longlat +datum=NAD83"))

# reads shapefile named above and assigns projection (P4)
rgdal.shp <- rgdal::readOGR(dsn = output.arcgis.paths.file, layer = "towpath", p4s = "+proj=longlat +datum=NAD83")
# re-writes shapefile using ESRI driver, includes projection, overwrites anything same named at the end of the path
rgdal::writeOGR(rgdal.shp, output.arcgis.paths.file, "c_towpaths", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# note that when charting the corrected positions the corrected towpath looks like it is in front of the vessel positions
# the phenomenon is actually that the vessel starts at a point but has to travel the distance the net is set behind the vessel 
# before arriving at the corrected position where the net touched down at on bottom



################################# frame data for towlines ###############################
# the data coming in from the CSV contain start and end points of a linear towpath
pos.dat <- data.frame()
col.names <- c("Id", "X", "Y", "OB_FB")
for(hj in sort(unique(haul.dat$hauljoin))){
  
  p1 <- c(hj, haul.dat$c_end_lon[haul.dat$hauljoin == hj], haul.dat$c_end_lat[haul.dat$hauljoin == hj], "off bottom")
  p2 <- c(hj, haul.dat$c_start_lon[haul.dat$hauljoin == hj], haul.dat$c_start_lat[haul.dat$hauljoin == hj], "on bottom")
  
  pos.dat <- rbind(pos.dat, p2, p1)
}
names(pos.dat) <- col.names
# head(pos.dat); length(unique(pos.dat$Id)); length(pos.dat$Id)

i <- c(1:3) # columns to convert to numeric from character
pos.dat[ ,i] <- apply(pos.dat[ ,i], 2, function(x) as.numeric(as.character(x)))
# sapply(pos.dat, class); head(pos.dat)
# eliminate any NAs
pos.dat <- pos.dat[!is.na(pos.dat$Id), ]

# arcmap.shp <- data.frame(Id = pos.dat$Id, X = pos.dat$X, Y = pos.dat$Y)
# eliminate NAs
# arcmap.shp <- arcmap.shp[!is.na(arcmap.shp$Id),] # a data frame with 3 variables (Id, X, Y)
# just need column of unique hauljoins here 
arcmap.dbf <- data.frame(Id = unique(pos.dat$Id))

# old school method for creating the towpaths
# maptools::readShapeLines is deprecated
# first convert points to shapefile	where type 3 means polyLine
arcmap.shapefile <- shapefiles::convert.to.shapefile(shpTable = pos.dat, attTable = arcmap.dbf, field = "Id", type = 3)
# write shapefile which, give it the filename in out.name, and arcgis = T replaces "." with "\_" in column names for ArcGIS
shapefiles::write.shapefile(shapefile = arcmap.shapefile, out.name = paste0(output.arcmap.lines.file, "/linepath"), arcgis = T)
# DEPRECATED writes shapefile at end of path, but really doesn't...I think it makes a memory object accessible in the next step
# arcgis.shapefile <- maptools::readShapeLines(output.arcgis.paths.file, proj4string=CRS("+proj=longlat +datum=NAD83"))

# reads shapefile named above and assigns projection (P4)
rgdal.shp <- rgdal::readOGR(dsn = output.arcmap.lines.file, layer = "linepath", p4s = "+proj=longlat +datum=NAD83")
# re-writes shapefile using ESRI driver, includes projection, overwrites anything same named at the end of the path
rgdal::writeOGR(rgdal.shp, output.arcmap.lines.file, "c_linepaths", driver = "ESRI Shapefile", overwrite_layer = TRUE)	


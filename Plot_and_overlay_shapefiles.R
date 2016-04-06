
#  To demonstrate how the data objects are moving across different packages, I denote an object name with:
#  name.pbs for PBSmapping object (polySet)
#  name.sp  for sp object (there are many types - spatialPolygons, spatialLines, spatialPolygonDataFrame, spatialLineDataFrame, etc)
#  name.gg  for ggplot (which is actually just a normal old data.frame)

#  Normally, I would overwrite the intermediate objects as I go, but I'm keeping the files from each data type for comparison as we go.

###############################################
library(maps)
library(mapdata)
library(ggplot2)
library(ggmap)
library(maptools)
library(sp)
library(PBSmapping)
library(rgdal)
library(plyr)
library(mapproj)

###############################################

#Add location of cities in PWS
cities <- data.frame(lon=c(-146.359505, -145.758158), lat=c(61.14, 60.543566))

#Location of various salmon stocks to analyze
stock_locs <- data.frame(lon=c(-147.814347, -144.908947, -148.128593, -147.448752),
                         lat=c(61.094084, 60.491447, 60.451407, 61.079911))

#Location where Exxon hit Bligh Reef….oops!
EVOS <- data.frame(lon=-146.8, lat=60.93)

####################################################

#Pull up NE Pacific high resolution data from the PBSmapping library
data(nepacLLhigh)##nepacLLhigh -> high resolution. nepacLL -> low resolution
tail(nepacLLhigh)
length(unique(nepacLLhigh$PID)) ##determines number of unique polygon groups

#----------------------------------------
#  This first step isn't totally necessary but the nepacLLhigh shapefile is really big. 
#  So we can create some boundaries and clip it to those boundaries.
lon.lim <- c(-160,-140)
lat.lim <- c(55,65)
# Clip 'PolySet' by given extent - this is still just a PBSmapping object
myNepac.pbs <- clipPolys(nepacLLhigh, xlim = lon.lim, ylim = lat.lim)
rm(lon.lim,lat.lim)
#----------------------------------------

# Look at what kind of data object it is
class(myNepac.pbs)

# Currently, nepacLLhigh is a 'PolySet' object. So look what happens if you just try to plot nepacLLhigh
# It's gonna take a few minutes.
plot(nepacLLhigh) # It plots each of the attributes (ie., the data frame part) of the shapefile

# If you just wanted to make a map of nepacLLhigh you could use the following (pay attention to how long it takes to plot)
plotPolys(nepacLLhigh)

# Now compare to our new, clipped version (which is also a polySet)
plotPolys(myNepac.pbs) # Much faster!
#----------------------------------------

# Now, in order to make a polySet object like nepacLLhigh play well with objects from the other spatial packages we have to convert it.
# We could just as easily have chosen to convert a SpatialPolygons object into a polySet instead of the polySet into a SpatialPolygons object.

# Convert clipped 'PolySet' back to SpatialPolygons from the sp package
Nepac.sp <- PolySet2SpatialPolygons(myNepac.pbs, close_polys=TRUE)

# Now what kind of data object is it?
class(Nepac.sp)

# Now you can just use the simple plot command on Nepac because it's no longer in PBSmapping world
plot(Nepac.sp)

# If you try to look at the attributes of the polySet object now, it's kind of a mess because it's an S4 object (that dealie with the slots)
head(Nepac.sp)

# Based on the interwebs, I think the nepacLLhigh dataset was an unprojected WGS84 datum. If something is unprojected, you don't really need
# to dick around with this next line most of the time I don't think. But I'm inluding it just to be thorough. 
# You can look up epsg numbers online - they are easier than typing out all the details. Also, see accompanying NCEAS worksheet.
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf
Nepac.sp <- spTransform(Nepac.sp,CRS("+init=epsg:4326"))

# Now transform for ggplot world
Nepac.gg <- fortify(Nepac.sp)

# look at what the field names are now (ie., is "x" called "X" or "lon" or "long"?), because you'll need to know what to call them in the ggplot 
head(Nepac.gg)

# Now what kind of data object is it?
class(Nepac.gg)
# Data.frame - great, since that's what ggplot likes!
####################################################

#Add Copper River Shapefiles
#Copper_Shape.sp <- readOGR(dsn="Shapefiles", layer = "copper_2m_lin")
Copper_Shape.sp <- readOGR(dsn="Copper River_KML.kml",layer="Copper River_KML.kml")

# What kind of data object is it?
class(Copper_Shape.sp)

# We can use the following to figure out what the current project/datum is:
# The @ sign here is used because the projection is a "slot." It's fucking complicated and this is about as far as I get with S4 objects.
Copper_Shape.sp@proj4string # Notice there is nothing in here that says 'datum'

# We can use the following line to figure out the project
# As we saw above, we can plot a Spatial_____DataFrame object using plot
plot(Copper_Shape.sp)
# Not very helpful without axes
axis(1);axis(2)

# Shit! It's in UTM (I hate UTM). So we use the next line to convert to lat-long. 
# I searched on-line to figure out that the nepacLLhigh dataset has datum WGS84. But since there wasn't a datum to begin with, I think it gets screwy. 
# The following two lines have the same result (one is less typing). They convert to an unprojected lat-long map with datum WGS84
# myCopper.sp <- spTransform(Copper_Shape.sp, CRS("+proj=longlat +datum=WGS84"))
myCopper.sp <- spTransform(Copper_Shape.sp, CRS("+init=epsg:4326"))

# Now look at the projection information
myCopper.sp@proj4string

# How does that compare with the Nepac.sp?
Nepac.sp@proj4string

# We can plot them on top of each other using sp structure.
plot(Nepac.sp,col="grey")
plot(myCopper.sp,add=TRUE,col="red")

# Convert to ggplot world
myCopper.gg <- (fortify(myCopper.sp))

head(myCopper.gg)

head(Cop) ####check out map proj????
###Convert <- mapproject(long,lat, proj="albers", orientation = c(90, 0, 225))

####################################################

#Map of PWS using nepacLLhigh
PWSMAP1 <- ggplot()+
  geom_polygon(data=Nepac.gg,aes(x=long,y=lat,group=factor(group)), fill=8, color='black', linetype = "solid", size = 0.15) + # look at the names in head(myCopper.gg) to figure out assignments here
  geom_path(data=myCopper.gg,aes(long,lat,group=factor(group)),color="red")+ # Experiment here. See what happens when you use geom_polygon vs. geom_line vs. geom_path
  theme(panel.background=element_rect(fill='white'))+
  xlab(expression(paste(Longitude^o,~'W')))+
  ylab(expression(paste(Latitude^o,~'N')))+theme_bw()+
  coord_map(xlim=c(-148.8,-144.5), ylim=c(59.5,61.5))+
  geom_point(data=EVOS, aes(lon, lat), size = 2, pch=8)+
  geom_point(data=cities, aes(lon, lat), size = 2)+
  geom_point(data=stock_locs, aes(lon,lat), size = 2, pch=17) +
  theme(axis.title.y=element_text(size=16),axis.title.x=element_text(size=16),
        axis.text.x=element_text(size=14),axis.text.y=element_text(size=14))+
  annotate("text", x = -146.3, y = 60.92, label = "EVOS") +
  annotate("text", x = -146, y = 61.2, label = "Valdez") +
  annotate("text", x = -145.4, y = 60.55, label = "Cordova") +
  annotate("text", x = -148.2, y = 61.19, label = "Coghill") +
  annotate("text", x = -144.908947, y = 60.7, label = "Copper R.") +
  annotate("text", x = -148.5, y = 60.44, label = "Eshamy") +
  annotate("text", x = -147.2, y = 61.2, label = "Unakwik")
PWSMAP1
ggsave("C:/Users/rebrenner/Desktop/PWSMAP1.png", dpi=500)


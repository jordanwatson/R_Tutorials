## Convert Lat-Lon dataset to a PBSmapping object and plot the points on a area gridded map of AK

PlotPointsFun <- function(junk){
	## Create an event ID for PBSMapping PolySet.	
	junk$EID = seq(length(junk$LONGITUDE))
	## Coordinates will be converted to UTM
	junk$X = junk$LONGITUDE
	junk$Y = junk$LATITUDE
	
	## Convert and project data in space
	junk=as.EventData(junk)
	attr(junk,"zone") = 3
	attr(junk, "projection") = "LL"
	junk = convUL(junk,km=F)
	return(junk)
}

## This is an example of how you would then make a plot.
junk <- PlotPointsFun(new.test)
plotPolys(ST6,projection="UTM",xlim=c(-0.2e+06,1e+06),ylim=c(5.9e+06,6.5e+06))
addPoints(junk,col="red")

## Now if you wanted to add labels to each of the STATAREA grids
## You have to read in the shapefile as an object within maptools (requires libary(maptools)).
myareas <- readShapePoly("Data/ST6")
text(myareas,labels=as.character(myareas$STAT_AREA),cex=0.75)




## This file creates a polygon that encompasses the Dutch Harbor to Akutan corridor.
## The polygon is then merged with VMS data to omit observations that only occurred within this corridor.
## Those observations can then be omitted or flagged as non-fishing.
## 4/22/2013 Jordan Watson


library(rgdal)
library(sp)

nearshore <- subset(AA03,PORTDIST<=32 & FISHING=="N",)
nearshore <- nearshore[,-c(1:3)]
nearshorepoints <- PlotPointsFun(nearshore)
plotPolys(ST6,projection="UTM",xlim=c(3.8e+05,4.7e+05),ylim=c(5.94e+06,6.03e+06))
addPoints(nearshorepoints,col="#00000020")

###############################################################################################################################
## Creating a custom polygon from XY data using the sp package.
## Skip to below commented section to read in polygon shapefile and to perform point-in-polygon operation using 
## a custom corridor polygon between Dutch Harbor and Akutan
###############################################################################################################################

## Use the locator function to create a polygon of points that encompass those vessels just transiting between Dutch and Akutan
x=locator()

## Put the xy values from the locator polygon "x" into a spatially referenced system
## First create a dataframe of xy values
DutchAK <- data.frame(cbind(X=x$x,Y=x$y))

## Need to close the polygon by making the first and last points the same location
DutchAK[22,] <- DutchAK[1,]

## Now for the crazy shit. 
## Convert the set of XY data into a single polygon
DutchAK <- Polygon(DutchAK,hole=T)

## If you had multiple polygons, you would then create a list with each of the above polygons. In this case, there is just one.
DutchAK2 <- Polygons(list(DutchAK),1)

## Convert this polygon(s) to a projected spatial polygon data frame
SpTest <- SpatialPolygons(list(DutchAK2),proj4string=CRS("+proj=utm +zone=3 +datum=NAD83 +units=m"))

## Add attribute data to this polygon. I have assigned the polygon an arbitrary name of "999999" though I don't think I'll ever use this.
my.dat = data.frame(STATAREA=999999)

## Join the polygon data to the attribute data
spdf = SpatialPolygonsDataFrame(SpTest,my.dat)

## Save this polygon as a shapefile. This step is not necessary for functionality but in order to be able to reuse this polygon in later
## R sessions, we will need to read it back in so we don't have to recreate the polygon again.
writePolyShape(spdf,"Data/DutchAkutanPoly")

###############################################################################################################################
## Identify the vessel observations that occurred within the custom corridor polygon between Dutch and Akutan
###############################################################################################################################

## Read in the polygon shapefile that is the corridor
spdf <- readShapePoly("Data/DutchAkutanPoly",proj4string=CRS("+proj=utm +zone=3 +datum=NAD83 +units=m"))

## Convert VMS data to a SpatialPointsDataFrame in order to perform point-in-polygon operation
## Extract just the spatial data from the VMS dataset so that sp package knows how to join your fields
coords=cbind(AA03$X,AA03$Y)

## Combine your spatial coordinates fields with you data and include the projection. The projection must match
## the projection of the polygon data. proj4string=CRS("+proj=utm +zone=3 +datum=NAD83 +units=m")
AA03Spatial <- SpatialPointsDataFrame(coords,AA03,proj4string=CRS("+proj=utm +zone=3 +datum=NAD83 +units=m"))


## Perform the point-in-polygon operation. This is a version of the "over" command in sp. However, instead of extracting
## the polygon information, which is what "over" does, we extract the point data instead. The output data frame will
## include all rows where the point data match the polygon.  
junk <- AA03Spatial[spdf,]

## Normal subsetting does not seem to work here, as I really only want INDEX value from AA03Spatial. So extract INDEX via this second step
DutchAKTransit <- junk$INDEX

## If you wanted to see which polygons in the SpatialPolygon layer overlap with our data, you could use this command
# junk <- AA03Spatial %over% spdf



## This file creates a polygon that encompasses the eastern Bering Sea.
## The polygon is then merged with VMS data to omit observations that only occurred within this corridor.
## Those observations can then be omitted or flagged as non-fishing.
## 4/22/2013 Jordan Watson

## If we want just the convex hull of the fishing observations, we can use this first section. Otherwise, go through the process to create
## your own polygon, which allows for a buffer.

x <- PlotPointsFun(AA03[AA03$HAUL_JOIN>0,])
all <- PlotPointsFun(AA03)
#plotPolys(ST6,projection="UTM",xlim=c(0,1.3e+06),ylim=c(58e+05,6.5e+06),bg="gray")
plotPolys(ST6,projection="UTM",xlim=c(-5e+05,1.8e+06),ylim=c(55e+05,7.5e+06),bg="gray")
addPoints(x,col="#FF000050")
addPoints(all,col="#FF0000")
addPoints(x[x$STAT_AREA>600000,],col="#FFFFFF50")

AA03[AA03$HAUL_JOIN>0 & AA03$STAT_AREA==685530,]

convexHullPoly = calcConvexHull(x[x$NMFS>500 & x$NMFS<530,])
addPolys(convexHullPoly,col="#FFFFFF")


library(rgdal)
library(sp)

EBS <- subset(AA03,FISHING=="Y",)
#EBS <- EBS[,-c(1:3)]
EBSpoints <- PlotPointsFun(EBS)
plotPolys(ST6,projection="UTM",xlim=c(-4e+05,1e+06),ylim=c(5.75e+06,7e+06))
#plotPolys(ST6,projection="UTM")
addPoints(EBSpoints,col="#00000030")

###############################################################################################################################
## Creating a custom polygon from XY data using the sp package.
## Skip to below commented section to read in polygon shapefile and to perform point-in-polygon operation using 
## a custom corridor polygon between Dutch Harbor and Akutan
###############################################################################################################################

## Use the locator function to create a polygon of points that encompass those vessels just transiting between Dutch and Akutan
x=locator()

## Put the xy values from the locator polygon "x" into a spatially referenced system
## First create a dataframe of xy values
EBSpoly <- data.frame(cbind(X=x$x,Y=x$y))
EBSpoly <- EBSpoly[-10,]

## Need to close the polygon by making the first and last points the same location
EBSpoly[12,] <- EBSpoly[1,]

## Now for the crazy shit. 
## Convert the set of XY data into a single polygon
EBSpoly <- Polygon(EBSpoly,hole=T)

## If you had multiple polygons, you would then create a list with each of the above polygons. In this case, there is just one.
EBSpoly2 <- Polygons(list(EBSpoly),1)

## Convert this polygon(s) to a projected spatial polygon data frame
SpTest <- SpatialPolygons(list(EBSpoly2),proj4string=CRS("+proj=utm +zone=3 +datum=NAD83 +units=m"))

## Add attribute data to this polygon. I have assigned the polygon an arbitrary name of "999999" though I don't think I'll ever use this.
my.dat = data.frame(STATAREA=999999)

## Join the polygon data to the attribute data
spdf = SpatialPolygonsDataFrame(SpTest,my.dat)

## Save this polygon as a shapefile. This step is not necessary for functionality but in order to be able to reuse this polygon in later
## R sessions, we will need to read it back in so we don't have to recreate the polygon again.
writePolyShape(spdf,"Data/EBSPoly")

###############################################################################################################################
## 
###############################################################################################################################

## Read in the polygon shapefile that is the corridor
spdf <- readShapePoly("Data/EBSPoly",proj4string=CRS("+proj=utm +zone=3 +datum=NAD83 +units=m"))

## Convert VMS data to a SpatialPointsDataFrame in order to perform point-in-polygon operation
## Extract just the spatial data from the VMS dataset so that sp package knows how to join your fields
coords=cbind(AA03$X,AA03$Y)

## Combine your spatial coordinates fields with your data and include the projection. The projection must match
## the projection of the polygon data. proj4string=CRS("+proj=utm +zone=3 +datum=NAD83 +units=m")
AA03Spatial <- SpatialPointsDataFrame(coords,AA03,proj4string=CRS("+proj=utm +zone=3 +datum=NAD83 +units=m"))


## Perform the point-in-polygon operation. This is a version of the "over" command in sp. However, instead of extracting
## the polygon information, which is what "over" does, we extract the point data instead. The output data frame will
## include all rows where the point data match the polygon.  
junk <- AA03Spatial[spdf,]

## Normal subsetting does not seem to work here, as I really only want INDEX value from AA03Spatial. So extract INDEX via this second step
EBStrips <- junk$INDEX

## If you wanted to see which polygons in the SpatialPolygon layer overlap with our data, you could use this command
# junk <- AA03Spatial %over% spdf



###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################

## Plot the mean squared prediction error using the model prediction output "my.prob"


## Create a custom function for mean squared prediction error
## based on 1/n * Sum over all (observed-prediction)^2
## where x in the function is the data.frame name (probably AA100.test or AA30.test)

mse.fun<-function(x){
  return((sum((x$Fishing.Yes-x$my.prob)^2))/length(x$STAT_AREA))
  }


## Pat's mapping code turned into a function. Use the mtext command with your call to the function to add a title.
map.fun <- function(){
  PID.key = tapply(OA03$PID,OA03$STAT_AREA,mean)
  PID.key = data.frame(STATAREA=names(PID.key),PID=PID.key)
  PID.ave.fishing = merge(PID.key,average.fishing,by=c("STATAREA","STATAREA"),all=T)
  PID.ave.fishing = PID.ave.fishing[order(PID.ave.fishing$PID),]
  PID.ave.fishing$AVE.FISHING <- PID.ave.fishing$mse
  my.labels = cut(PID.ave.fishing$AVE.FISHING*100,breaks=seq(0,16))
  PID.ave.fishing$colors = as.numeric(cut(PID.ave.fishing$AVE.FISHING,breaks=seq(0,0.16,by=0.01),labels=seq(1,16)))+2
  PID.ave.fishing$colors[is.na(PID.ave.fishing$colors)]=2
  jet.colors <-colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
  palette(c("black","white",jet.colors(16)))
  plotPolys(ST6,border=1,col=PID.ave.fishing$colors)
  legend(x="topleft",bty="n",fill=c("white",jet.colors(16)),legend=c("Missing",levels(my.labels)),cex=.75)
}

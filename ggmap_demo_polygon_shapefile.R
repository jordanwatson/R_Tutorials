library(rgdal)
library(ggmap)
setwd("//nmfs.local//AKC-ABL//Users2//jordan.watson/Desktop/AFSC/Haynie_Sullivan_ms")
#Only thing you should have to change is the "Data/Shapefiles", which is the folder in which your shapefile lives.
stat6 <- readOGR("Data/Shapefiles","AK_Stat")
#  Reproject the shapefile to match Google Earth's projection
stat6 <- spTransform(stat6, CRS("+proj=longlat +datum=WGS84"))
#  Extract the ID/names for each stat_area
idList <- stat6@data$STAT_AREA
#  Extract the centroid coordinates for each stat area, convert it to a spatial object and then to a data.frame
centroids.df <- as.data.frame(coordinates(stat6))
names(centroids.df) <- c("Longitude", "Latitude")  #more sensible column names
label.df <- data.frame(id = idList, centroids.df)
#  Convert the shapefile polygons to ggplot polygons
stat6 <- fortify(stat6,region='STAT_AREA')

#  Make up some location data for shits and grins
my.coords <- data.frame(Hatchery = c("Main Bay","Cannery Creek","AFK","Noeremberg"),
				LONGITUDE = c(-148.09355, -147.51962, -148.06817, -148.08639),
				LATITUDE = c(60.51922,61.01586,60.04995,60.79770))

x11();qmap("Prince William Sound, Alaska",maptype="satellite",zoom=8) + geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.2,color='green', data=stat6, alpha=0.0) + 
	geom_point(data=my.coords,aes(x=LONGITUDE,y=LATITUDE),size=5,colour="red") + geom_text(data=label.df,aes(label = id, x = Longitude, y = Latitude))




#  The following reads-in the "afa" data frame from Trip_Characteristics.R and then plots speeds by stat area.

library(rgdal)
library(ggmap)
setwd("//nmfs.local//AKC-ABL//Users2//jordan.watson/Desktop/AFSC/Haynie_Sullivan_ms")
#Only thing you should have to change is the "Data/Shapefiles", which is the folder in which your shapefile lives.
stat6 <- readOGR("Data/Shapefiles","AK_Stat")
#  Reproject the shapefile to match Google Earth's projection
stat6 <- spTransform(stat6, CRS("+proj=longlat +datum=WGS84"))
#  Extract the ID/names for each stat_area
idList <- stat6@data$STAT_AREA
#  Extract the centroid coordinates for each stat area, convert it to a spatial object and then to a data.frame
centroids.df <- as.data.frame(coordinates(stat6))
names(centroids.df) <- c("Longitude", "Latitude")  #more sensible column names
label.df <- data.frame(id = idList, centroids.df)
#  Convert the shapefile polygons to ggplot polygons
stat6 <- fortify(stat6,region='STAT_AREA')
stat6 <- filter(stat6,!id %in% unique(stat6$id)[1:600])

fishdat <- filter(afa,Fishing.Yes==1 & SEASON=="A" & vessel==2 & YEAR==2007 & MYAVG<7)
fishstat <- group_by(fishdat,STAT_AREA) %>% summarise(Speed=median(MYAVG))
statdat <- right_join(fishstat,stat6,by=c("STAT_AREA"="id"))

#ggp <- ggplot(data=statdat[!is.na(statdat$Speed),], aes(x=long, y=lat, group=group)) 
ggp <- ggplot(data=statdat, aes(x=long, y=lat, group=group)) + ylim(52,59) + xlim(-172,-160)
ggp <- ggp + geom_polygon(aes(fill=Speed))         # draw polygons
ggp <- ggp + geom_path(color="grey", linestyle=2)  # draw boundaries
ggp <- ggp + coord_equal() 
ggp <- ggp + scale_fill_gradient(low = "#ffffcc", high = "#ff4444", 
                                 space = "Lab", na.value = "grey50",
                                 guide = "colourbar")
ggp <- ggp + labs(title="Speed (knots) - vessel 2, A season 2007")
# render the map
x11();ggp


fastboats <- c(109,325,647,661,703,872)
years <- 2003:2013
for(i in 1:length(years)){
  fishdat <- filter(afa,Fishing.Yes==1 & SEASON=="B" & vessel==3 & YEAR==years[i] & MYAVG<7 & (RANDOM_ID %in% fastboats))
  fishstat <- group_by(fishdat,STAT_AREA) %>% summarise(Speed=median(MYAVG))
  statdat <- right_join(fishstat,stat6,by=c("STAT_AREA"="id"))
  
  #ggp <- ggplot(data=statdat[!is.na(statdat$Speed),], aes(x=long, y=lat, group=group)) 
  ggp <- ggplot(data=statdat, aes(x=long, y=lat, group=group)) + ylim(52,59) + xlim(-179,-160) +
    geom_polygon(aes(fill=Speed)) +
    geom_path(color="grey", linestyle=2) +
    coord_equal() +
    scale_fill_gradient(low = "#ffffcc", high = "#ff4444", 
                        space = "Lab", na.value = "grey50",
                        guide = "colourbar",limits=c(1,7)) +
    labs(title=paste("Speed (knots) - vessel 2, B season", years[i]),sep="") +
    theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))
  # render the map
  assign(paste("p",years[i],sep=""),ggp)
}

pdf("FishingSpeedLocation_fastboats.pdf",width=20,height=20);grid.arrange(p2003,p2004,p2005,p2006,p2007,p2008,p2009,p2010,
                                                                          p2011,p2012,p2013);dev.off()


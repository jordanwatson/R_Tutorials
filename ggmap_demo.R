install.packages("ggmap")
library(ggmap)

#  Examples of different zoom levels and how you can enter locations. Think you can also just center it on a lat-lon
qmap('Cordova, Alaska',zoom=14,maptype='satellite')
qmap('Sheridan Glacier',zoom=10,maptype='satellite')
qmap('300 Breakwater Ave., Cordova, AK',zoom=15,maptype='satellite')
qmap('300 Breakwater Ave., Cordova, AK',zoom=17,maptype='satellite') #it has the address a tad off
qmap('300 Breakwater Ave., Cordova, AK',zoom=17,maptype='satellite') #it has the address a tad off

# Now let's do something useful
# Create some coordinates (these happen to be hatcheries in PWS)
my.coords <- data.frame(Hatchery = c("Main Bay","Cannery Creek","AFK","Noeremberg"),
				LONGITUDE = c(-148.09355, -147.51962, -148.06817, -148.08639),
				LATITUDE = c(60.51922,61.01586,60.04995,60.79770))

# Plot the map with points
qmap('Prince William Sound',zoom=8,maptype='satellite') + geom_point(data=my.coords, aes(x=LONGITUDE,y=LATITUDE),colour="red")

# Plot the map with larger points
qmap('Prince William Sound',zoom=8,maptype='satellite') + geom_point(data=my.coords, aes(x=LONGITUDE,y=LATITUDE),colour="red",size=5)

# Add text labels
qmap('Prince William Sound',zoom=8,maptype='satellite') + 
				geom_point(data=my.coords, aes(x=LONGITUDE,y=LATITUDE),colour="red",size=5) + 
				geom_text(data=my.coords, aes(x=LONGITUDE,y=LATITUDE, label = Hatchery),colour="red", vjust=1.5)

# Use different colors for each factor level
my.colors <- c("red","green","yellow","white")
qmap('Prince William Sound',zoom=8,maptype='satellite') + 
				geom_point(data=my.coords, aes(x=LONGITUDE,y=LATITUDE,group = factor(Hatchery),colour=factor(Hatchery)),size=5) + 
				geom_text(data=my.coords, aes(x=LONGITUDE,y=LATITUDE, label = Hatchery,colour=factor(Hatchery)), vjust=1.5) + 
				scale_colour_manual(values=my.colors)

qmap('Prince William Sound',zoom=8,maptype='terrain') + 
				geom_point(data=my.coords, aes(x=LONGITUDE,y=LATITUDE,group = factor(Hatchery),colour=factor(Hatchery)),size=5) + 
				geom_text(data=my.coords, aes(x=LONGITUDE,y=LATITUDE, label = Hatchery,colour=factor(Hatchery)), vjust=1.5) + 
				scale_colour_manual(values=my.colors)

# Different map style
qmap('Cordova, Alaska',zoom=15,maptype='terrain')

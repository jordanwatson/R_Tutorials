





























http://stackoverflow.com/questions/38853917/avoiding-hoizontal-lines-and-crazy-shapes-when-plotting-maps-in-ggplot2




#  Before we dive into the code, a quick warning...err, I mean, uh, introduction...to a piece of syntax that is different when
#  working with shapefiles in R. When you load a shapefile or other spatial object (like a google .kml file) there is a lot of info.
#  Remember that when Arc reads "a" shapefile there are often about a half dozen files associated with that file 
#  (e.g., .xml, .shp, .dbf, .prj). Each of these files stores different types of information with different types of attributes. R 
#  needs to retain that information if you want to be able to use it. So when you read in a shapefile, it is turned into a spatial 
#  object that you could imagine as a really fancy list. It includes the coordinates of each of the vertices of say, an Alaska map.
#  It also needs to include your projection, the attributes and attribute table (your data), and well other stuff that I just don't 
#  really understand. 

#  What I do understand is that these information can not be simply accessed the same way as information in a list. Different pieces of 
#  data are kept in different "slots." A slot can be accessed using the "@" character. You will see this only a few times throughout this
#  file. Particularly, we will access the projection via a slot called "proj4string" via: myshapefile@proj4string.
#  If we wanted to read in a shapefile and then look at the attribute table we would use myshapefile@data. If that yields many results, 
#  try head(myshapefile@data).

#  Here is one of a gazillion sites that explains it better than I do: https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf



library(tidyverse)
library(rgdal)
library(broom)

setwd("//nmfs.local/AKC-ABL/Users2/jordan.watson/Desktop/Other_people/RWorkshop/Maps")

ogrInfo(dsn="//nmfs.local/AKC-ABL/Users2/jordan.watson/Desktop/Other_people/RWorkshop/Maps/AKbasemap.shp",layer="AKbasemap")

akmap <- readOGR(dsn="//nmfs.local/AKC-ABL/Users2/jordan.watson/Desktop/Other_people/RWorkshop/Maps/AKbasemap.shp",layer="AKbasemap")

#  What does this shapefile look like? 
plot(akmap)

#  By default, the axes are not plotted but you can fix that quickly
axis(1);axis(2)

#  What object class is this magical file?
class(akmap)

#  What is the projection of the shapefile? We'll need to make sure that each of the files have the same projection.
akmap@proj4string

#  This is much of the same information that you'd see if you looked at the different files that make up the shapefile 
#  (e.g., AKbasemap.shp, AKbasemap.dbf, AKbasemap.prj, AKbasemap.xml...) and you opened the .prj file. Or similarly if, in your GIS
#  program like Arc or qGIS you looked at the properties of the layer.

#  Convert the object to a ggmap object
ak <- tidy(akmap)

#  Now what is the class of the object?
class(ak)

#  What are the names of our fields in the data.frame?
head(ak)

#  When we plot, we need to know that our coordinates are called "long" and "lat" and that polygons are called "group" for this shapefile.
map1 <- ggplot() + 
  geom_polygon(data=ak,aes(x=long,y=lat,group=factor(group))) + theme_bw()

xmin <- -170
xmax <- -130
ymin <- 50
ymax <- 70

map1 + coord_map(xlim=c(xmin,xmax),ylim=c(ymin,ymax))

#  Okay, it looks a little silly with Canada missing. Let's pull in one of the Canada shapefiles that are built in the R maps packages.
library(maps)
library(mapdata)
#  This will make sure the high res data set is loaded
data("world2HiresMapEnv")

#  Convert the dataset to a data frame
canada=map_data("world2Hires",region="canada")

#  I'm not going to worry about the projection because this part is just background filler.

#  Let's make sure we know what the fields are called.
head(canada)

#  Let's make sure it looks like Canada
ggplot() + geom_polygon(data=canada,aes(long,lat,group=factor(group)))

#library(PBSmapping)
#canada <- map_data("world")
#names(canada) <- c("X","Y","PID","POS","region","subregion")
#canada = clipPolys(canada, xlim=xlim,ylim=ylim, keepExtra=TRUE)
#ggplot() + geom_polygon(data = mapWorld, aes(X,Y,group=PID))


#  Now let's add this to our Alaska map, putting in a "color" argument which changes the outline color and provides us with a border
map1 + 
  geom_polygon(data=canada,aes(long,lat,group=factor(group)),color="grey30") + 
  coord_map(xlim=c(xmin,xmax),ylim=c(ymin,ymax)) 


#  Let's add a layer that shows the extent of EVOS (Exxon Valdez Oil Spill)
evos <- readOGR(dsn="//nmfs.local/AKC-ABL/Users2/jordan.watson/Desktop/Other_people/RWorkshop/Maps/EVOS.shp",layer="EVOS")

#  What does this layer look like?
plot(evos)

#  We need to make sure that the projection of this shapefile matches that of our basemap. 
evos@proj4string

#  Hmm, that's a lot to compare so let's just ask R to see if they are identical
identical(evos@proj4string,akmap@proj4string)

#  We could transform the projection by using the spTranform function and manually spelling out the new CRS 
#  (coordinate reference system) that we want (see ?spTransform). But let's be smart. We know we just want to match
#  that of our basemap projection. So let's just tell it our source shapefile (evos) and our new projection 
#  (which should match akmap@proj4string)
evos2 <- spTransform(evos,akmap@proj4string)

#  Now let's see if evos2@proj4string is the same as akmap@proj4string
identical(evos2@proj4string,akmap@proj4string)

#  Neat!!! 
evoslayer <- tidy(evos2)

head(evoslayer)

map1 + 
  geom_polygon(data=canada,aes(long,lat,group=factor(group)),color="grey30") + 
  geom_polygon(data=evoslayer,aes(x=long,y=lat,group=factor(group)),fill="gray10",alpha=0.5) + 
  coord_map(xlim=c(xmin,xmax),ylim=c(ymin,ymax))

#  If we just add the new polygon it adds it on top of the map but we want it to slide underneath the map for better continuity
#  because we are fisheries people and mostly we're interested in the marine parts of the EVOS effects. 

ggplot() + 
  geom_polygon(data=canada,aes(long,lat,group=factor(group)),color="grey30") + 
  geom_polygon(data=evoslayer,aes(x=long,y=lat,group=factor(group)),fill="gray10",alpha=0.5) + 
  geom_polygon(data=ak,aes(x=long,y=lat,group=factor(group))) + 
  coord_map(xlim=c(xmin,xmax),ylim=c(ymin,ymax)) + 
  theme_bw()


#  Let's add one more layer that includes polygons for several different study regions.
nceas <- readOGR(dsn="//nmfs.local/AKC-ABL/Users2/jordan.watson/Desktop/Other_people/RWorkshop/Maps/NCEASRegions.shp",layer="NCEASRegions")

plot(nceas);axis(1)

#  Before we transform the shapefile, let's first look at the data (the .dbf part) that you'd find in the attribute field of a shapefile.

nceas@data

#  Okay, we see here that there are 15 different regions or polygons. Let's select only certain ones that we want to plot.
nceas <- nceas[nceas@data$NCEAS_Area %in% c("Alaska Peninsula","Bristol Bay","Cook Inlet","Kodiak","PWS","SEAK Inside"),]

#  Also, ggplot
mylabs <- data.frame(nceas@data,id=(as.character(rownames(nceas@data))))

plot(nceas)

#  Let's go through the same projection business
nceas@proj4string

identical(nceas@proj4string,akmap@proj4string)

nceas2 <- spTransform(nceas,akmap@proj4string)

identical(nceas2@proj4string,akmap@proj4string)

nceas2@proj4string

#  Convert to a data.frame
nceas2 <- tidy(nceas2) %>% left_join(mylabs)

#  Make sure the fields are called the same thing.
head(nceas2)

#  It'll take a few tries to get the order of the polygons right, but with some tweaking we can get...
ggplot() + 
  geom_polygon(data=canada,aes(long,lat,group=factor(group)),fill="gray30",color="black") + 
  geom_polygon(data=evoslayer,aes(x=long,y=lat,group=factor(group)),fill="gray50",alpha=0.5) + 
  geom_polygon(data=nceas2,aes(x=long,y=lat,group=factor(group)),fill="gray20",color="black",linetype=3,alpha=0.5) +
  geom_polygon(data=ak,aes(x=long,y=lat,group=factor(group)),fill="gray30",color="black") +
  coord_map(xlim=c(xmin,xmax),ylim=c(ymin,ymax)) + 
  theme_bw() + 
  annotate("text", x = -150, y = 53.5, label = "Gulf of Alaska") +
  annotate("text", x = -166, y = 56, label = "Bering\nSea") +
  annotate("text", x = -138, y = 57, label = "SEAK") +
  annotate("text", x = -146, y = 59.5, label = "PWS",color="white")

bw.map <- ggplot() + 
  geom_polygon(data=canada,aes(long,lat,group=factor(group)),fill="gray30",color="black") + 
  geom_polygon(data=nceas2,aes(x=long,y=lat,group=factor(group)),fill="gray20",color="black",linetype=3,alpha=0.75) +
  geom_polygon(data=evoslayer,aes(x=long,y=lat,group=factor(group)),fill="gray50",alpha=0.75) + 
  geom_polygon(data=ak,aes(x=long,y=lat,group=factor(group)),fill="gray30",color="black") +
  coord_map(xlim=c(xmin,xmax),ylim=c(ymin,ymax)) + 
  theme_bw() + 
  annotate("text", x = -150, y = 53.5, label = "Gulf of Alaska") +
  annotate("text", x = -166, y = 56, label = "Bering\nSea") +
  annotate("text", x = -138, y = 57, label = "SEAK") +
  annotate("text", x = -146, y = 59.5, label = "PWS",color="white")

bw.map

my.map <- ggplot() + 
  geom_polygon(data=canada,aes(long,lat,group=factor(group)),fill="gray30",color="black") + 
  geom_polygon(data=evoslayer,aes(x=long,y=lat,group=factor(group)),fill="gray50",alpha=0.9) + 
  geom_polygon(data=nceas2,aes(x=long,y=lat,group=factor(group),fill=factor(NCEAS_Area)),color="black",linetype=3,alpha=0.5) +
  geom_polygon(data=ak,aes(x=long,y=lat,group=factor(group)),fill="gray30",color="black") +
  annotate("text", x = -155, y = 53, label = "Gulf of Alaska") +
  annotate("text", x = -166, y = 56, label = "Bering\nSea") + 
  coord_map(xlim=c(xmin,xmax),ylim=c(ymin,ymax)) + 
  theme_bw() + 
  theme(legend.position = c(0.5, 0.7),legend.background=element_blank()) +
  labs(fill="Fishery areas")

my.map


#--------------------------------------------------------------------------------------------

library(PBSmapping)
data(nepacLLhigh)

class(nepacLLhigh)

head(nepacLLhigh)

ggplot() + geom_polygon(data=nepacLLhigh,aes(x=X,y=Y,group=PID))

insetxmin <- -180
insetxmax <- -120
insetymin <- 40
insetymax <- 71

ggplot() + 
  geom_polygon(data=nepacLLhigh,aes(x=X,y=Y,group=PID)) + 
  coord_map(xlim=c(insetxmin,insetxmax),ylim=c(insetymin,insetymax)) + 
  theme_bw()


#  The key to an inset of course is some sort of bounding box that identifies our study region within the broader map so
#  we can draw a rectangle that is bounded by the coordinates that we used to scale the study area map 
#  (previously called, xmin, xmax, ymin, ymax)

my.extent<-data.frame(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax)

my.extent

my.inset <- ggplot() + 
  geom_polygon(data=nepacLLhigh,aes(x=X,y=Y,group=PID),fill="grey30") + 
  coord_map(xlim=c(insetxmin,insetxmax),ylim=c(insetymin,insetymax)) + 
  geom_rect(data = my.extent, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            alpha=0, 
            colour="red", 
            size = 1, 
            linetype=1) + 
  theme_bw() + 
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm")) +
  labs(x=NULL,y=NULL)
  
  
my.inset

#  An important note in the above settings. For the inset we don't want any margin around the map. 
#  We have to set the x and y labels to NULL instead of just making them blank (via ""). If we set them to be blank
#  then they are still there but you just can't see them. If you use NULL instead, they no longer occupy that space and
#  the margins can be brought in. 
  
library(grid)
library(gridExtra)

#  To say that I have a poor understanding of how to customize and manipulate "viewports" is an understatement. But in so far
#  as adding an inset to your map, I can describe simply. 

#  The grid and gridExtra packages divide the plotting space into a grid. Within the grid, we can define the plotting region by
#  these different viewports. First we can create a new grid:

grid.newpage()

#  A page can create multiple different viewports. If we wanted just a single plot to occupy the entire space, it would have a 
#  width and height of 1, centered in the middle (x and y = 0.5)

v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
print(my.map,vp=v1) 

#  In this case we want our main map to take up the whole plot, and then we want a second viewport to overlay atop the first.
#  So we create one viewport with width/height = 1 and centered on 0.5. Then we create a second viewport where the plot is only, 
#  arbitrarily, let's say 20% of the size of the first plot (width=0.2,height=0.2). And we want to stick in the unoccupied portion of the Gulf of Alaska
#  in the bottom right corner. For viewports, think standard Cartesian reference system where 0,0 is the bottom left and 1,1 is the 
#  top right corner. After a bunch of fiddling, I came up with the following:

grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.2, height = 0.2, x = 0.65, y = 0.2) #plot area for the inset map
print(my.map,vp=v1) 
print(my.inset,vp=v2)

#  We could save this directly to a file.

png(file="test.png",w=1800,h=1800, res=300)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.2, height = 0.2, x = 0.65, y = 0.2) #plot area for the inset map
print(my.map,vp=v1) 
print(my.inset,vp=v2)
dev.off()

#  May you'd prefer to have the inset up in the land portion of the figure? Just change the x and y settings of v2
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.2, height = 0.2, x = 0.7, y = 0.7) #plot area for the inset map
print(my.map,vp=v1) 
print(my.inset,vp=v2)


#  So this isn't really the best inset because it's not a whole lot broader of a region than the original map. That's because
#  the shapefile I used was fairly limiting. I used this one because the coordinates were on the same scale and I was being lazy. 
#  One of the annoying things with mapping Alaska is that whole dateline thing. Let's look at two of the other built-in world data sets
#  from the maps package.

library(maps)

#  The built-in maps package has a "world" and a "world2" dataset. 
#  "World" is centered on the Prime Meridian
map("world",fill=TRUE,col="grey");axis(1);axis(2)

#  World2 is centered on the date line
map("world2",fill=TRUE,col="grey");axis(1);axis(2)

#  Note that the x-axis on one goes from -180 to 180 and the other is 0 to 360. Our Alaska base layer map also goes from -180 to 180.
range(ak$long)

#  However, being on the same scale doesn't really help us that much because if you look at the world map above that goes from -180 to 180,
#  it cuts the Aleutian chain so that parts of it are on opposite sides of the world. We could try to do some transformations or just
#  use the map that is already scaled for the Pacific. 

#  Convert this maps object into a ggplot data frame.
world2 <- map_data("world2")

#  Did it work?
class(world2)

#  What are the names of the fields?
head(world2)

#  What's it look like?
ggplot() + geom_polygon(data=world2,aes(x=long,y=lat,group=factor(group)))


#  Ideally we could just reuse some of the code from our previous inset experience. First let's look at the boundaries we'd set.

insetxmin <- -180
insetxmax <- -120

#  We see pretty quickly that we've already got a probably since our new map doesn't have negative longitudes. No problem. 
#  We wanted new boundaries anyway, right? We could just add 360 to the old boundaries to get the same extent as before

insetxmin <- -180 + 360
insetxmax <- -120 + 360

#  Or we could look at the map and just pick new bounds altogether.

insetxmin <- 170
insetxmax <- 250

#  Maybe let's expand our souther x boundary a bit, too

insetymin <- 30
insetymax <- 71

ggplot() + 
  geom_polygon(data=world2,aes(x=long,y=lat,group=factor(group))) + 
  coord_map(xlim=c(insetxmin,insetxmax),ylim=c(insetymin,insetymax)) + 
  theme_bw()

#  Whoa! What happened? Well this is actually a common problem when you try to zoom in with the world and world2 (and some other) 
#  spatial datasets. The way I understand it is basically that we have tried to zoom in too far within a given polygon or set of 
#  polygons such that some of them are inside the plotting region and some are outside. R gets confused on how to draw the polygons 
#  when this happens. A work around is to clip the polygons to the region that we want to plot, essentially creating new polygons.
#  We can use the clipPolys function from PBSmapping to do this (it should already be loaded because that's where we got the nepacLLhigh
#  dataset earlier).

#  First, clipPolys is expecting certain column names so we should change the names of our columns to match those. Instead of 
#  "long" and "lat", it wants "X" and "Y", repectively. Instead of "group" it wants "PID" and instead of "order" it uses "POS." We can 
#  then keep the extra column names (that's what the keepExtra=TRUE will do in clipPolys)
names(world2) <- c("X","Y","PID","POS","region","subregion")

#  You can learn more about clipPolys via ?clipPolys. We need to specify our x and y boundaries so that it can clip the dataset.
world2 <- clipPolys(world2, xlim=c(insetxmin,insetxmax),ylim=c(insetymin,insetymax), keepExtra=TRUE)

#  Now let's try plotting again, but remember we changed the names so we need to change our geom_polygon
ggplot() + 
  geom_polygon(data=world2,aes(x=X,y=Y,group=factor(PID))) + 
  coord_map(xlim=c(insetxmin,insetxmax),ylim=c(insetymin,insetymax)) + 
  theme_bw()

#  So now our inset has a bit more of Russia and the Aleutian chain and certainly enough to indicate the region and scale that we're 
#  looking at I think.

#  We want the extent rectangle to show the same area as the focus map but remember they're on different scales now.
xmin
xmax

#  We can fix this by simply adding 360 to our longitudinal values.
my.extent<-data.frame(xmin=(xmin+360),xmax=(xmax+360),ymin=ymin,ymax=ymax)

my.extent

my.inset <- ggplot() + 
  geom_polygon(data=world2,aes(x=X,y=Y,group=PID),fill="grey30") + 
  coord_map(xlim=c(insetxmin,insetxmax),ylim=c(insetymin,insetymax)) + 
  geom_rect(data = my.extent, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            alpha=0, 
            colour="red", 
            size = 1, 
            linetype=1) + 
  theme_bw() + 
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm")) +
  labs(x=NULL,y=NULL)


my.inset

#  This spatial file is actually a little less detailed which looks better for the inset as well. 


#  I'll show a variation on the previous inset approach now, where we have multiple main figures and either a single or multiple insets.

#  Let's use a viewport grid layout to display our color and bw maps side by side.

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2))) # Side by side layout (1 row x 2 columns)
#  Create a function that specifies the layout of the grid (you don't have to understand this - just be able to copy and paste it)
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(my.map, vp = vplayout(1, 1))  # Row 1, column 1
print(bw.map, vp = vplayout(1, 2))  # Row 2, column 2

#  Now let's add an inset to the color map.

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2))) # Side by side layout
v2<-viewport(width = 0.18, height = 0.18, x = 0.35, y = 0.28) #plot area for the inset map
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(my.map, vp = vplayout(1, 1))  # Row 1, column 1
print(bw.map, vp = vplayout(1, 2))  # Row 2, column 2
print(my.inset, vp = v2)

#  To add an inset to the second plot, we just need to create a new viewport. 

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2))) # Side by side layout
v2 <-viewport(width = 0.18, height = 0.18, x = 0.35, y = 0.28) #plot area for the inset map
v3 <- viewport(width = 0.18, height = 0.18, x = 0.75, y = 0.68) 
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(my.map, vp = vplayout(1, 1))  # Row 1, column 1
print(bw.map, vp = vplayout(1, 2))  # Row 2, column 2
print(my.inset, vp = v2)
print(my.inset, vp = v3)

#  Of course, viewports are not for maps alone. They are just another way to grid your graphics objects.
#  We can add any old figure to the plotting region. Let's create a dummy plot.

p1 <- ggplot() + geom_point(aes(x=1:25,y=1:25),shape=1:25,size=4)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2))) # Side by side layout
v2 <-viewport(width = 0.18, height = 0.18, x = 0.35, y = 0.28) #plot area for the inset map
v3 <- viewport(width = 0.18, height = 0.18, x = 0.75, y = 0.68) 
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(p1, vp = vplayout(1, 1))  # Row 1, column 1
print(bw.map, vp = vplayout(1, 2))  # Row 2, column 2
print(my.inset, vp = v3)

#  We can just futz with the figure margins in either of the plots to get them to line up but I'm not going to worry about that here. 







#----------------------------------------------
#  Alaska Basemap
#----------------------------------------------


library(PBSmapping)
library(tidyverse)
library(broom)


xmin <- -180
xmax <- -150
ymin <- 50
ymax <- 68

ggplot() + 
  geom_polygon(data=nepacLLhigh,aes(x=X,y=Y,group=PID)) + 
  coord_map(xlim=c(xmin,xmax),ylim=c(ymin,ymax)) + 
  theme_bw()


library(marmap)
bsbath <- getNOAA.bathy(lon1=xmin,lon2=xmax,lat1=ymin,lat2=ymax, resolution=1)


bs.df <- fortify(bsbath)


ggplot() +
  geom_polygon(data=nepacLLhigh,aes(x=X,y=Y,group=PID), fill=8, color='black') + 
  geom_contour(data=bs.df,aes(x=x,y=y,z=z),colour="black", size=0.1,breaks=c(-100, -200)) + 
  coord_map(xlim=c(xmin,xmax),ylim=c(ymin,ymax)) + 
  theme_bw()















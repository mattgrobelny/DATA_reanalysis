library(sp)
library(maps)
library(maptools)
library(ggplot2)
library(ggthemes)
library(polyclip)
library(mapproj)

Fish_catch_gps <- read.csv("~/Documents/OneDrive/Antarctica Files/Data/Gradient Project/Hobodata/Large Tank/DATA_reanalysis/Fish_catch_gps.csv")

Fish_catch_gps<-Fish_catch_gps[1:7,]

world <- map("world", fill=TRUE, col="transparent", plot=FALSE)
worldSpP <- map2SpatialPolygons(world, world$names, CRS("-proj=longlat +ellps=WGS84"))
worldSpP <- worldSpP[grep("Antarctica", row.names(worldSpP)),]
ldSpPnr <- nowrapRecenter(worldSpP)

world_map <- fortify(worldSpPnr)

gg <- ggplot(data=Fish_catch_gps,aes(label=Fish_catch_gps$Catch.Location))

gg <- gg + geom_map(data=world_map, map=world_map,
                    aes(x=long, y=lat, map_id=id),
                    color="black", fill="white", size=0.5)



gg <- gg +geom_point(data=Fish_catch_gps,aes(x=as.numeric(Fish_catch_gps$Longitude),
                                             y=Fish_catch_gps$Latitude,
                                             #shape=Fish_catch_gps$Species,
                                             color=Fish_catch_gps$Catch.Location), size=3, alpha=0.5)+
geom_text(vjust =1 , hjust=.3,size =3, fontface = "bold", color='black',check_overlap = TRUE)+
  + theme(legend.position = "bottom")
  
gg <- gg + coord_map(xlim =c(-70,-50), ylim=c(c(-70,-55)))+theme_map()

gg

##########################









 

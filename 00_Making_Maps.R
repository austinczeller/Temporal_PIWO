#-----------Making Maps-----------#
library(ggplot2)
library(sf)
library(elevatr)
library(ggspatial)
library(vapoRwave)


col_order<- c("longitude","latitude","location","sampling")

locations<-read.csv("data/MapData/locations.csv")
locations<-locations[col_order]
elepoint<-get_elev_point(locations = locations,prj="EPSG:4326",src="aws")
albertashp<-st_read("W:/BayneLabWorkSpace/Woodpeckers/alberta.shp")
points <- st_as_sf(elepoint)
points<-st_transform(points,crs=st_crs(albertashp))


map<-ggplot()+geom_sf(data=albertashp,fill='#FFE2D1')+geom_sf(data=points,aes(color=sampling))+
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location ="br", which_north = "true",
                         style = north_arrow_fancy_orienteering)+
  labs(x=NULL,y=NULL)+theme_void()+labs(color=NULL)+
  scale_color_manual(labels=c("Hourly sampling","Daily sampling"),
                     values=c("#00B19D","#EB4762"))
plot(map)
ggsave("figures/map.png",map)  
 
#Get the ecoregion names-----

ecoregions<-st_read("data/MapData/Ecoregions/ecoregions.shp")
locations_sf <- st_as_sf(locations, coords = c("longitude", "latitude"), crs = st_crs(ecoregions))
locations_sf$ecoregion_name <- apply(st_within(locations_sf, ecoregions, sparse = FALSE), 1, function(x)
  { ecoregions$REGION_NAM [x]})
locations_sf
list(unique(locations_sf$ecoregion_name))


                                                                            
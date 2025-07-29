#-----------Making Maps-----------#
library(ggplot2)
library(sf)
library(ggspatial)
library(dplyr)
library(scales)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)

okabe_ito<-c("#009E73","#E69F00","#D55E00","#F0E442","#56B4E9","#0072B2")


ecosystems<-read_sf("data/MapData/Natural_Regions_Subregions_of_Alberta/Natural_Regions_Subregions_of_Alberta.shp")
ecosystems<-ecosystems%>%mutate("Ecosystem"=NRNAME)

canada <- ne_states(country = "canada", returnclass = "sf")
alberta <- canada %>% filter(name == "Alberta")

locations<-read.csv("C:/Users/austi/Documents/Transfer/Ch2Plots/ch1locations.csv")
locations<-locations%>%filter(latitude<60)%>%filter(longitude>-120)#filter out locations outside AB

locations<-locations%>%mutate("Sampling"=sampling)
locations<-st_as_sf(locations, coords = c("longitude", "latitude"),crs=4326)

cities<- tibble(name=c("Calgary","Edmonton"),long=c(-114.0719,-113.4937),lat=c(51.0447,53.5461))

cities_sf<-cities%>%st_as_sf(coords=c("long","lat"),crs=4326)



map<-ggplot()+
  geom_sf(data = ecosystems, aes(fill = Ecosystem),color=0,linewidth=0)+
  scale_fill_manual(values = okabe_ito) +
  geom_sf(data=locations,aes(color=Sampling))+
  scale_color_manual(labels=c("Hourly sampling","Daily sampling"),values=c("black","gray77"))+
  geom_sf(data=cities_sf,color="black",shape=17,size=3)+
  geom_sf_text(data=cities_sf,aes(label=name),hjust=-.2)+
  theme_void()+
  annotation_scale(location = "br", width_hint = 0.4) +
  annotation_north_arrow(location ="bl", which_north = "true",
                         style = north_arrow_fancy_orienteering)

map
ggsave("map.png",map)


#Now add canada insert
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)

# Get North America boundaries
canada <- ne_states(country = "canada", returnclass = "sf")
alberta <- canada %>% filter(name == "Alberta")

inset_map <- ggplot() +
  geom_sf(data = canada, fill = "white", color = "black") +
  geom_sf(data = alberta, fill = "#CC79A7", color = "black", size = 1) +
  theme_void()
inset_map

combined_map <- ggdraw() +
  draw_plot(map) +
  draw_plot(inset_map, x = 0.65, y = 0.70, width = 0.3, height = 0.3)
combined_map


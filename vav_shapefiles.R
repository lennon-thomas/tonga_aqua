library(tidyr)
library(tmap)
library(raster)
library(rgdal)
library(sp)
library(Hmisc)
library(ncdf4)
library(broom)


boxdir<-'/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/Vav_Shapefiles'

island<-readOGR(dsn=boxdir,layer="Island")


repro<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "

island_reporj<-spTransform(island,repro)

ext<-c( -174.2542, -173.79, -19.2, -17.5) 

vav_islands<-crop(island_reporj,ext)

tidy_islands<-tidy(vav_islands)

temp_df<-data.frame(vav_islands@data)

temp_df$id<-row.names(temp_df)

islands_df<-merge(tidy_islands,temp_df,by="id")

base<-ggplot() +
  geom_polygon(data = islands_df, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.5) + 
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

anchor<-readOGR(dsn=boxdir,layer="Anchorages") #wrong coordinates

bioram<-readOGR(dsn=boxdir,layer="BIORAP Important Marine Areas")
vav_bioram<-crop(bioram,ext)
tidy_bioram<-tidy(vav_bioram)
temp_df<-vav_bioram@data
temp_df$id<-row.names(temp_df)

bioram_df<-merge(tidy_bioram,temp_df,by="id")

base +
  geom_polygon(data=bioram_df,aes(x=long,y=lat,group=group),fill="red")




fishing<-readOGR(dsn=boxdir,layer="Community Fishing Areas")
tidy_fishing<-tidy(fishing)
base +
 # geom_polygon(data=fishing,aes(long,y=lat,group=group),fill="yellow",alpha=0.5) +
  geom_point(data=cot_df,aes(x=coords.x1,y=coords.x2),fill="red",col="red")



nests<-readOGR(dsn=boxdir,layer="Confirmed Nests")
nests_df<-tidy(nests)
temp_df<-nests@data
nests_df<-merge(nests_df,temp_df)


cot<-readOGR(dsn=boxdir,layer="CoT")
cot_df<-tidy(cot)
temp_df<-cot@data
cot_df<-merge(cot_df,temp_df)



dsm<-readOGR(dsn=boxdir,layer="DSM_tenaments")
dsm_df<-tidy(dsm)
temp_df<-dsm@data
dsm_df<-merge(dsm_df,temp_df,by.x="id",by.y="Id")
base+
  geom_line(data=dsm_df,aes(x=long,y=lat,group=factor(group)),col="red")



erosion<-readOGR(dsn=boxdir,layer="Erosion zones")

erosion_df<-tidy(erosion)
temp_df<-erosion@data
erosion_df<-merge(erosion_df,temp_df,by.x="id",by.y="Id")

base+
  geom_line(data=erosion_df,aes(x=long,y=lat,group=factor(group)),col="green")






#mangrove_use<-readOGR(dsn=boxdir,layer="Mangrove Use")
whale<-readOGR(dsn=paste0(boxdir,"/data/VAV_Shapefiles"),layer="Mother_Calf_Habitat")
whale_df<-tidy(whale)
temp_df<-whale@data
whale_df<-merge(whale_df,temp_df,by.x="id",by.y="Id")

p<-ggplot()+
  geom_polygon(data=water,aes(x=long,y=lat,group=group),fill="lightblue",alpha=0.5) +
  geom_raster(data=suit_df,aes(x=x,y=y,fill = as.factor(index_1)),show.legend = TRUE,na.rm=TRUE,alpha =0.8)+
  geom_polygon(data=whale_df,aes(x=long,y=lat,group=group,fill=id),alpha=0.5,show.legend=TRUE) +
  scale_fill_manual(values=c("0"="purple","4"="red"),name="",label=c("Mother/calf humpback habitat","Suitable giant clam mariculture area"))+
 # scale_fill_continuous("Suitable Giant Clam Area",low="blue",high="red") +
  
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.5) + 
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed(1.03) 
p+  guides(fill = guide_legend(override.aes = list(alpha = c(0.5,0.8))))
  
 

ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/whale.png")


ovaka<-readOGR(dsn=paste0(boxdir,"/data/VAV_Shapefiles"),layer="Ovaka SMA Boundary")
ovaka_df<-tidy(ovaka) %>%
  filter(id!=0 & id!=1)

pending_sma<-readOGR(dsn=paste0(boxdir,"/data/VAV_Shapefiles"),layer="Pending SMA")
pending_sma_df<-as_data_frame(pending_sma)


cr_base+
  geom_polygon(data=ovaka_df,aes(x=long,y=lat,group=group),color="red",fill=NA) +
  geom_point(data=pending_sma_df,aes(x=coords.x1,y=coords.x2),color="red",size=3)

ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/SMA.png")




dive<-readOGR(dsn=paste0(boxdir,"/data/VAV_Shapefiles"),layer="Popular Dive Sites")
dive_df<-as_data_frame(dive) %>%
  mutate(Activity="Popular dive site") %>%
  select(coords.x1,coords.x2,Activity)

hotel<-readOGR(dsn=paste0(boxdir,"/data/VAV_Shapefiles"),layer="Resorts_and_hotels_Vavau")

repro<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "

hotel<-spTransform(hotel,repro)

hotel_df<-as_data_frame(hotel)%>%
  mutate(Activity="Hotel") %>%
  select(coords.x1,coords.x2,Activity)


rec<-readOGR(dsn=paste0(boxdir,"/data/VAV_Shapefiles"),layer="Recreation")

rec_df<-as_data_frame(rec) %>%
  mutate(Activity="Recreation") %>%
  select(coords.x1,coords.x2,Activity) %>%
  

ggsave()
activity<-rbind(dive_df,hotel_df,rec_df) %>%
  filter(Activity!="Hotel")

g<-ggplot()+
geom_polygon(data=water,aes(x=long,y=lat,group=group),fill="lightblue",alpha=0.5) +
  geom_raster(data=suit_df,aes(x=x,y=y,fill = as.factor(index_1)),show.legend = TRUE,na.rm=TRUE,alpha=0.5)+

  geom_polygon(data=tidy_fishing,aes(x=long,y=lat,group=group,fill=piece),show.legend=TRUE,alpha=0.5)+
 scale_fill_manual(values=c("1"="navy","4"="red"),name="",label=c("Community fishing areas","Suitable giant clam mariculture area"))+
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.5) + 
  geom_point(data=activity,aes(x=coords.x1,y=coords.x2,shape=Activity,color=Activity),size=3)+
  scale_shape_manual(name="",labels=c("Popular dive sites","Recreation areas"),values=c(17,20)) +
  scale_color_manual(name="",labels=c("Popular dive sites","Recreation areas"),values=c("red","green")) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed(1.03) 
g+  guides(fill = guide_legend(override.aes = list(alpha = c(0.5,0.5))))
  
ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/giant_clam/marine_activities.png")



fishing<-readOGR(dsn=paste0(boxdir,"/data/VAV_Shapefiles"),layer="Community Fishing Areas")

tidy_fishing<-tidy(fishing)

cr_base +
  geom_polygon(data=tidy_fishing,aes(x=long,y=lat,group=group),col="red",fill="red",alpha="0.5")
  ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/fishing areas.png")



res<-readOGR(dsn=boxdir,layer="residential area")
hotel<-readOGR(dsn=boxdir,layer="Resorts_and_hotels_Vavau")
seacuc<-readOGR(dsn=boxdir,layer="Sea cucumber")
spawn<-readOGR(dsn=boxdir,layer="Spawning Grounds")
aqua<-readOGR(dsn=boxdir,layer="TTA Aquaculture")

mangrove<-readOGR(dsn=boxdir,layer="Vavau magrove subset")
mangrove_df<-tidy(mangrove)
temp_df<-mangrove@data
mangrove_df<-merge(mangrove_df,temp_df)
base+
  geom_polygon(data=mangrove_df,aes(x=long,y=lat,group=group),fill="green",alpha=0.5)





water<-readOGR(dsn=boxdir,layer="Inland Water")

labels<-readOGR(dsn=boxdir,layer="labels")

reef<-readOGR(dsn=boxdir,layer="Polygons")

reef_reporj<-spTransform(reef,repro)

vav_reef<-crop(reef_reporj,ext)

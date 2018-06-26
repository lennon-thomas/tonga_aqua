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
whale<-readOGR(dsn=boxdir,layer="Mother_Calf_Habitat")
whale_df<-tidy(whale)
temp_df<-whale@data
whale_df<-merge(whale_df,temp_df,by.x="id",by.y="Id")

base+
  geom_polygon(data=whale_df,aes(x=long,y=lat,group=group),fill="purple",alpha=0.5)



ovaka<-readOGR(dsn=boxdir,layer="Ovaka SMA Boundary")
pending_sma<-readOGR(dsn=boxdir,layer="Pending SMA")
dive<-readOGR(dsn=boxdir,layer="Popular Dive Sites")
rec<-readOGR(dsn=boxdir,layer="Recreation")
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

#10/17/18
# Rabbitfish suitability for aquaculture in Vavau
#Lennon Thomas

library(tidyr)
library(tmap)
library(raster)
library(rgdal)
library(sp)
library(Hmisc)
library(ncdf4)
library(broom)


boxdir<-'/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture'

# Define rabbitfish suitability thresholds --------------------------------


min_depth<--3
max_depth<--30
min_salinity<-15
max_salinity<-27
oxygen_min<-5
min_temp<-22
max_temp<-30

# Read in data files

# create basemap ----------------------------------------------------------


vavau_eez<-readOGR(dsn=paste0(boxdir,"/data/tmp"), layer ="vavau_eez_shape")

ext<-extent(vavau_eez)

tidy_eez<-tidy(vavau_eez)

temp_df<-data.frame(vavau_eez@data)

temp_df$id<-row.names(temp_df)

EEZ_df<-merge(tidy_eez,temp_df,by="id")

land<-EEZ_df %>%
  dplyr::filter(hole==TRUE)

water<-EEZ_df %>%
  dplyr::filter(hole==FALSE)

base<-ggplot() +
  geom_polygon(data = water, aes(x=long, y=lat, group=group),fill =  "lightblue", alpha = 0.5, size = 0.5) + 
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.5) + 
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

# Depth -------------------------------------------------------------------


depth<-raster(paste0(boxdir,"/data/tmp/vav_depth.tif"))


depth[depth>min_depth]<-NA
depth[depth<max_depth]<-NA
depth_area<-area(depth,na.rm=TRUE)

depth_df<-as.data.frame(rasterToPoints(depth))
depth_area_df<-as.data.frame(rasterToPoints(depth_area))
t_depth_area<-sum(depth_area_df$layer,na.rm=TRUE)

ggplot()+
  geom_polygon(data=water,aes(x=long,y=lat,group=group),fill="lightblue",alpha=0.5) +
  geom_raster(data=depth_df,aes(x=x,y=y,fill = vav_depth),title="Depth (m)",na.rm=TRUE)+
  scale_fill_continuous("Depth (m)",low="navy",high="red") +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.5) + 
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(paste0("Suitable Depth for rabbitifsh mariculture: ",round(t_depth_area,2)," km^2"))
ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/rabbitfish/depth.png")

depth<-calc(depth,fun=function(x){x*-1})
# Oxygen Minimum units in 	mol.m-3----------------------------------------------------------------

min_ox<-raster(paste0(boxdir,"/data/raw/Present.Surface.Dissolved.oxygen.Min.tif"))
min_ox<-crop(min_ox,ext)


# SST ---------------------------------------------------------------------

min_sst<-raster(paste0(boxdir,"/data/tmp/min_sst.tif"))
max_sst<-raster(paste0(boxdir,"/data/tmp/max_sst.tif"))


min_sst[min_sst < min_temp]<-NA
max_sst[max_sst>max_temp]<-NA
sst_area<-area(max_sst,na.rm = TRUE)


sst_df<-as.data.frame(rasterToPoints(max_sst))
sst_area_df<-as.data.frame(rasterToPoints(sst_area))
t_sst_area<-sum(sst_area_df$layer,na.rm=TRUE)

ggplot()+
  geom_polygon(data=water,aes(x=long,y=lat,group=group),fill="lightblue",alpha=0.5) +
  geom_raster(data=sst_df,aes(x=x,y=y,fill = max_sst),title="Sea Surface Temperature",na.rm=TRUE)+
  scale_fill_continuous("Depth (m)",low="blue",high="red") +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.5) + 
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(paste0("Suitable SST for rabbitifsh mariculture: ",round(t_sst_area,2)," km^2"))
ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/rabbitfish/sst.png")

# Mangroves ---------------------------------------------------------------

mangrove<-readOGR(dsn=paste0(boxdir,"/data/tmp"),layer='mangrove_shp')

m_raster<-rasterize(mangrove,depth, field=0)

m_raster[is.na(m_raster)]<-1

m_df<-as.data.frame(rasterToPoints(m_raster))

# Coral -------------------------------------------------------------------

coral<-raster(paste0(boxdir,"/data/tmp/vav_coral_raster.tif"))
              #buffer 100 m around corals as per Price 2014
coral[coral==2]<-0
coral[is.na(coral)]<-1   

# Turbidity ---------------------------------------------------------------


max_kd<-raster(paste0(boxdir,"/data/raw/pk/max_kd.tif"))
max_kd<-crop(max_kd,ext)
max_kd[max_kd>=0.101]<-NA
kd_area<-area(max_kd,na.rm=TRUE)
max_kd_df<-as.data.frame(rasterToPoints(max_kd)) 
area_kd_df<-as.data.frame(rasterToPoints(kd_area))
t_kd_area<-sum(area_kd_df$layer)

ggplot()+
  geom_polygon(data=water,aes(x=long,y=lat,group=group),fill="lightblue",alpha=0.5) +
  geom_raster(data=max_kd_df,aes(x=x,y=y,fill = max_kd),na.rm=TRUE)+
  scale_fill_continuous("SST (m)",low="yellow",high="darkred") +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.5) + 
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(paste0("Suitable turbidity areas for giant clam mariculture: ",round(t_kd_area,2)," km^2"))
ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/giant_clam/turbidity.png")


max_kd[max_kd>=0.101]<-0
max_kd[max_kd<0.101]<-1


# Suitability assessment --------------------------------------------------

s_stack<-stack(depth,max_kd,coral,m_raster)

suit<-stackApply(s_stack,indices=c(1,1,1,1), fun=sum,na.rm=TRUE)

proj4string(suit)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

suit[suit<4]<-NA
suit<-calc(suit,fun=function(x){x-3})
suit_area<-area(suit,na.rm = TRUE)
suit_df<-as.data.frame(rasterToPoints(suit))
suit_area_df<-as.data.frame(rasterToPoints(suit_area))
total_suit<-sum(suit_area_df$layer)

max_sst[is.na(max_sst)]<--1
max_sst[max_sst>0]<-NA
sst_df<-as.data.frame(rasterToPoints(max_sst))



ggplot()+
  geom_polygon(data=water,aes(x=long,y=lat,group=group),fill="lightblue",alpha=0.5) +
  geom_raster(data=suit_df,aes(x=x,y=y,fill = index_1),show.legend = TRUE,na.rm=TRUE)+
  #  geom_raster(data=m_df,aes(x=x,y=y,layer=layer))+
  #  geom_polygon(data=aqua_df,aes(x=long,y=lat,group=group),fill="navy",alpha=0.5) +
  geom_raster(data=sst_df,aes(x=x,y=y,fill=max_sst),fill="black",alpha=0.5) +
  scale_fill_continuous("Depth",low="blue",high="red",trans="reverse") +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.5) + 
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed(1.03) +
    ggtitle(paste0("Potentially suitable areas for floating cage rabbitfish aquaculture ", round(total_suit,2)," km^2"))
  ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/rabbitfish/final_suitable.png")


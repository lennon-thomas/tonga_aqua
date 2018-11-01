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
max_depth<--20
#min_salinity<-15
#max_salinity<-27
#oxygen_min<-5
#min_temp<-22
#max_temp<-30

# Read in data files

# create basemap ----------------------------------------------------------


vavau_eez<-readOGR(dsn=paste0(boxdir,"/data/tmp"), layer ="vavau_eez_shape")

#ext<-extent(vavau_eez)

ext<-c(-174.2,-173.85,-18.88,-18.55)

vavau_eez<-crop(vavau_eez,ext)

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
depth<-crop(depth,ext)

depth[depth>min_depth]<-NA
depth[depth<max_depth]<-NA
depth_area<-area(depth,na.rm=TRUE)


depth<-calc(depth,fun=function(x){x*-1})
depth[depth>1]<-1

depth_df<-as.data.frame(rasterToPoints(depth))
depth_area_df<-as.data.frame(rasterToPoints(depth_area))
t_depth_area<-sum(depth_area_df$layer,na.rm=TRUE)

d<-ggplot()+
  geom_polygon(data=water,aes(x=long,y=lat,group=group),fill="lightblue",alpha=0.5) +
  geom_raster(data=depth_df,aes(x=x,y=y,fill = layer),title="Depth (m)",na.rm=TRUE)+
 # scale_fill_continuous("Depth (m)",low="navy",high="red") +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.5) + 
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(paste0("Suitable Depth for rabbitifsh mariculture: ",round(t_depth_area,2)," km^2"))
ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/rabbitfish/depth.png")

# Oxygen Minimum units in 	mol.m-3----------------------------------------------------------------

#didn't end up including this
min_ox<-raster(paste0(boxdir,"/data/raw/Present.Surface.Dissolved.oxygen.Min.tif"))
min_ox<-crop(min_ox,ext)


# SST ---------------------------------------------------------------------
#not included in suitability analysis
# min_sst<-raster(paste0(boxdir,"/data/tmp/min_sst.tif"))
# max_sst<-raster(paste0(boxdir,"/data/tmp/max_sst.tif"))
# max_sst<-crop(max_sst,ext)
# 
# min_sst[min_sst < min_temp]<-NA
# max_sst[max_sst>max_temp]<-NA
# sst_area<-area(max_sst,na.rm = TRUE)
# 
# 
# sst_df<-as.data.frame(rasterToPoints(max_sst))
# sst_area_df<-as.data.frame(rasterToPoints(sst_area))
# t_sst_area<-sum(sst_area_df$layer,na.rm=TRUE)
# 
# ggplot()+
#   geom_polygon(data=water,aes(x=long,y=lat,group=group),fill="lightblue",alpha=0.5) +
#   geom_raster(data=sst_df,aes(x=x,y=y,fill = max_sst),title="Sea Surface Temperature",na.rm=TRUE)+
#   scale_fill_continuous("Depth (m)",low="blue",high="red") +
#   geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.5) + 
#   xlab("Longitude") +
#   ylab("Latitude") +
#   theme_bw() +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   ggtitle(paste0("Suitable SST for rabbitifsh mariculture: ",round(t_sst_area,2)," km^2"))
# ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/rabbitfish/sst.png")
# 
# Mangroves ---------------------------------------------------------------

mangrove<-readOGR(dsn=paste0(boxdir,"/data/tmp"),layer='mangrove_shp')

mangrove<-crop(mangrove,ext)

m_raster<-rasterize(mangrove,depth, field=0)

m_raster[is.na(m_raster)]<-1

m_raster[m_raster==0]<-NA
m_df<-as.data.frame(rasterToPoints(m_raster))

ggplot()+
  geom_polygon(data=water,aes(x=long,y=lat,group=group),fill="lightblue",alpha=0.5) +
  geom_raster(data=m_df,aes(x=x,y=y,fill=layer),na.rm=TRUE) 

# Coral -------------------------------------------------------------------

coral<-raster(paste0(boxdir,"/data/tmp/vav_coral_raster.tif"))
              #buffer 100 m around corals as per Price 2014
coral<-crop(coral,ext)
coral[coral==2]<-0
coral[is.na(coral)]<-1 
coral[coral==0]<-NA
coral_df<-as.data.frame(rasterToPoints(coral))

ggplot()+
  geom_polygon(data=water,aes(x=long,y=lat,group=group),fill="lightblue",alpha=0.5) +
  geom_raster(data=coral_df,aes(x=x,y=y,fill=vav_coral_raster),na.rm=TRUE) 
  

coral_area<-area(coral,na.rm=TRUE)
coral_area_df<-as.data.frame(coral_area)
t_coral_area<-sum(coral_area_df)
# Turbidity ---------------------------------------------------------------


max_kd<-raster(paste0(boxdir,"/data/raw/pk/max_kd.tif"))
max_kd<-crop(max_kd,ext)
max_kd[max_kd>=0.3]<-NA
max_kd[max_kd>0]<-1
kd_area<-area(max_kd,na.rm=TRUE)
max_kd_df<-as.data.frame(rasterToPoints(max_kd)) 
area_kd_df<-as.data.frame(rasterToPoints(kd_area))
t_kd_area<-sum(area_kd_df$layer)

ggplot()+
  geom_polygon(data=water,aes(x=long,y=lat,group=group),fill="lightblue",alpha=0.5) +
  geom_raster(data=max_kd_df,aes(x=x,y=y,fill = max_kd),na.rm=TRUE)+
  scale_fill_continuous("Kd",low="yellow",high="darkred") +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.5) + 
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(paste0("Suitable turbidity areas for giant clam mariculture: ",round(t_kd_area,2)," km^2"))
ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/rabbitfish/turbidity.png")



# Current -----------------------------------------------------------------

#max current rec is <1 m/s and min current rec is >0.1 m/x
min_current<-raster(paste0(boxdir,"/data/raw/current/min.tif"))
min_current<-crop(min_current,ext)
min_current[min_current<0.1]<-NA



max_current<-raster(paste0(boxdir,"/data/raw/current/max.tif"))
max_current<-crop(max_current,ext)


# Suitability assessment --------------------------------------------------

s_stack<-stack(depth,max_kd,coral,m_raster)

suit<-stackApply(s_stack,indices=c(1,1,1,1), fun=sum,na.rm=TRUE)

#proj4string(suit)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

suit[suit<4]<-NA
suit<-calc(suit,fun=function(x){x-3})
suit_area<-area(suit,na.rm = TRUE)
#suit[suit>0]<-1
suit_df<-as.data.frame(rasterToPoints(suit))
suit_area_df<-as.data.frame(rasterToPoints(suit_area))
total_suit<-sum(suit_area_df$layer)
suit_df$layer<-as.factor(suit_df$layer)


max_sst[is.na(max_sst)]<--1
max_sst[max_sst>0]<-NA
sst_df<-as.data.frame(rasterToPoints(max_sst))


# Aqua areas --------------------------------------------------------------

#not sure what TTA aqua areas are so taken out for now
tta_aqua<-readOGR(dsn=paste0(boxdir,"/data/VAV_Shapefiles"),layer = "TTA Aquaculture")
tta_aqua<-crop(tta_aqua,suit)
tidy_tta<-as_data_frame(tta_aqua)


aqua<-readOGR(dsn=paste0(boxdir,"/data/tmp"),layer = "aqua_areas")
aqua_raster<-rasterize(aqua,depth)
aqua_raster[aqua_raster>0]<-1

over<-stack(aqua_raster,suit)
over2<-stackApply(over,indices=c(1,1),fun="sum",na.rm=TRUE)
aqua_overlap<-intersect(suit,aqua_raster)
tidy_aqua<-tidy(aqua)
temp_df<-data.frame(aqua@data)
temp_df$id<-row.names(temp_df)
aqua_df<-merge(tidy_aqua,temp_df,by="id") 
names(aqua_df)<-c("id","long","lat","order","hole","piece","group","Name")
aqua_df<-aqua_df%>%
  mutate(fill_value=as.factor(5),
         line_value=as.factor(6))
aqua_labels<-aqua_df %>%
  dplyr:: select(id,long,lat) %>%
  dplyr:: group_by(id) %>%
  dplyr::top_n(1,wt=long)

# SMA areas ---------------------------------------------------------------

sma1<-readOGR(dsn=paste0(boxdir,"/data/SMAs"),layer="SMA_2016")  
sma1<-crop(sma1,suit)
tidy_sma1<-tidy(sma1)
sma1_temp<-as.data.frame(sma1) 
sma1_temp$id<-row.names(sma1_temp)  
sma1_df<-merge(sma1_temp,tidy_sma1,by="id")


sma_vav<-readOGR(dsn=paste0(boxdir,"/data"),layer="Protected_Areas_all_types")
sma_vav<-crop(sma_vav,suit)
tidy_sma1<-tidy(sma_vav)
sma1_temp<-as.data.frame(sma_vav) 
sma1_temp$id<-row.names(sma1_temp)  
sma1_df<-merge(sma1_temp,tidy_sma1,by="id")


# Plots -------------------------------------------------------------------


to<-ggplot()+
  geom_polygon(data=water,aes(x=long,y=lat,group=group),fill="lightblue",alpha=0.5) +
  geom_raster(data=suit_df,aes(x=x,y=y,fill = layer),show.legend = TRUE,na.rm=TRUE, alpha = 0.5)+
  #  geom_raster(data=m_df,aes(x=x,y=y,layer=layer))+
#  geom_raster(data=sst_df,aes(x=x,y=y,fill=max_sst),fill="black",alpha=0.5) +
 # geom_point(data=tidy_tta,aes(x=coords.x1,y=coords.x2),color="green") +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.5) + 
  geom_polygon(data=sma1_df,aes(x=long,y=lat,group=group,color=Status,fill=Status),show.legend = TRUE,size=0.95,alpha=0) +
  scale_color_manual("",labels=c("",""),values=c("Existing"="darkgreen","Proposed"="yellow"),guide = FALSE) +
  scale_fill_manual("",labels=c("Suitable rabbitfish mariculture area","Designated Aquaculture Areas","Existing  SMA","Proposed SMA"),values=c("Existing"="darkgreen","Proposed"="yellow","1"="red","5"="navy")) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed(1.03) +
  geom_polygon(data=aqua_df,aes(x=long,y=lat,group=group,fill=fill_value),col="navy",show.legend=TRUE,alpha=0.5) +
 # geom_text(data=aqua_df,aes(x=long,y=lat,group=group,label=Name),size=1.5)+
    ggtitle(paste0("Potentially suitable areas for floating cage rabbitfish aquaculture ", round(total_suit,2)," km^2"))
  ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/rabbitfish/final_suitable.png")

 t<-distinct(sma1_df,Location, .keep_all=TRUE)
  
to+
  geom_text(data=t,aes(x=long,y=lat,label=Location))

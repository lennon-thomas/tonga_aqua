library(tidyr)
library(tmap)
library(raster)
library(rgdal)
library(sp)
library(Hmisc)
library(ncdf4)
library(broom)


boxdir<-'/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture'


# create basemap ----------------------------------------------------------


vavau_eez<-readOGR(dsn=paste0(boxdir,"/data/tmp"), layer ="vavau_eez_shape")

ext2<-c(-174.25,-173.79,-18.97,-18.5)

vavau_eez<-crop(vavau_eez,ext2)

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
depth<-crop(depth,ext2)
depth[depth>0]<-NA
depth[depth<=-6]<-NA
depth[depth>=-5]<-1
depth_area<-area(depth,na.rm=TRUE)

depth_df<-as_data_frame(rasterToPoints(depth))
depth_area_df<-as_data_frame(rasterToPoints(depth_area))
t_depth_area<-sum(depth_area_df$layer,na.rm=TRUE)

ggplot()+
  geom_polygon(data=water,aes(x=long,y=lat,group=group),fill="lightblue",alpha=0.5) +
  geom_raster(data=depth_df,aes(x=x,y=y,fill = vav_depth),title="Depth (m)",na.rm=TRUE)+
  scale_fill_continuous("Depth (m)",low="navy",high="lightblue") +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.5) + 
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(paste0("Suitable Depth for giant clam mariculture: ",round(t_depth_area,2)," km^2"))
ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/giant_clam/depth.png")



# Turbidity ---------------------------------------------------------------


max_kd<-raster(paste0(boxdir,"/data/raw/pk/max_kd.tif"))
max_kd<-crop(max_kd,ext2)
max_kd[max_kd>=0.101]<-NA
kd_area<-area(max_kd,na.rm=TRUE)
max_kd_df<-as_data_frame(rasterToPoints(max_kd)) 
area_kd_df<-as_data_frame(rasterToPoints(kd_area))
t_kd_area<-sum(area_kd_df$layer)

ggplot()+
  geom_polygon(data=water,aes(x=long,y=lat,group=group),fill="lightblue",alpha=0.5) +
  geom_raster(data=max_kd_df,aes(x=x,y=y,fill = max_kd),title="Depth (m)",na.rm=TRUE)+
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
# habitat -----------------------------------------------------------------

h_type<-c("subtidal reef flat","shallow terrace with constructions","shallow terrace","reef flat","drowned bank","enclosed lagoon","forereef","deep drowned reef flat","deep lagoon","diffuse fringing","land on reef")

habitat<-readOGR(dsn = paste0(boxdir,"/data/tmp"),layer = "vav_habitat") 
habitat<-crop(habitat,ext2)
vav_habitat<-habitat[habitat$L4_ATTRIB %in% h_type,]
vav_habitat<-vav_habitat[vav_habitat$L3_ATTRIB !="Ocean exposed fringing",]



tidy_habitat<- broom::tidy(vav_habitat) #, region = "L4_ATTRIB")
tidy_area<-broom::tidy(habitat_area)

temp_df<-data.frame(vav_habitat@data)

temp_df$id<-row.names(temp_df)

vav_habitat_df<-merge(tidy_habitat,temp_df,by="id")

habitat_raster<-rasterize(vav_habitat,depth, field=1)
area_habitat<-area(habitat_raster,na.rm = TRUE)

habitat_raster_df<-as_data_frame(rasterToPoints(habitat_raster))
area_habitat_df<-as_data_frame(rasterToPoints(area_habitat))

t_habitat_area<-sum(area_habitat_df$layer)
ggplot() +
  geom_polygon(data = water, aes(x=long, y=lat, group=group),fill =  "lightblue", alpha =0.5, size = 0.8) +
  geom_polygon(data=vav_habitat_df,aes(x=long, y=lat, group=group, fill=L4_ATTRIB),show.legend = TRUE)+
  scale_fill_viridis("Benthic habitat",discrete=TRUE) +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(paste0("Suitable benthic areas for giant clam mariculture: ",round(t_habitat_area,2)," km^2")) +


ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/giant_clam/benthic_habitat.png")


ggplot() +
  geom_polygon(data = water, aes(x=long, y=lat, group=group),fill =  "lightblue", alpha =0.5, size = 0.8) +
  geom_raster(data=habitat_raster_df,aes(x=x, y=y, fill=layer),show.legend = TRUE)+
 # scale_fill_viridis("Benthic habitat",discrete=TRUE) +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

# Mangroves ---------------------------------------------------------------



mangrove<-readOGR(dsn=paste0(boxdir,"/data/tmp"),layer='mangrove_shp')

m_raster<-rasterize(mangrove,depth, field=0)

m_raster[is.na(m_raster)]<-1

m_df<-as_data_frame(rasterToPoints(m_raster))

# Suitability -------------------------------------------------------------

s_stack<-stack(depth,max_kd,habitat_raster,m_raster)

suit<-stackApply(s_stack,indices=c(1,1,1,1), fun=sum,na.rm=TRUE)



suit[suit<4]<-NA
suit_area<-area(suit,na.rm = TRUE)
suit_df<-as_data_frame(rasterToPoints(suit))
suit_area_df<-as_data_frame(rasterToPoints(suit_area))
total_suit<-sum(suit_area_df$layer)
  ggplot()+
    geom_polygon(data=water,aes(x=long,y=lat,group=group),fill="lightblue",alpha=0.5) +
    geom_raster(data=suit_df,aes(x=x,y=y,fill = index_1),show.legend = FALSE,na.rm=TRUE)+
  #  geom_raster(data=m_df,aes(x=x,y=y,layer=layer))+
  #  geom_polygon(data=aqua_df,aes(x=long,y=lat,group=group),fill="navy",alpha=0.5) +
    scale_fill_continuous(low="blue",high="red") +
    geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.5) + 
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    coord_fixed(1.03) +
#  ggtitle(paste0("Potentially suitable areas for giant clam aquaculture ", round(total_suit,2)," km^2"))
  ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/giant_clam/final_suitable.png")
  
  aqua_df<-aqua_df%>%
    mutate(fill_value=as.factor(5),
           line_value=as.factor(6))
  
t<-  ggplot() +
    geom_polygon(data=water,aes(x=long,y=lat,group=group),fill="lightblue",alpha=0.5) +
    geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
    geom_raster(data=suit_df,aes(x=x,y=y,fill = as.factor(index_1)),show.legend = TRUE,na.rm=TRUE,alpha=0.5)+
  geom_polygon(data=aqua_df,aes(x=long,y=lat,group=group,fill=fill_value),col="navy",show.legend=TRUE,alpha=0.5) +
  scale_fill_manual ("",values=c("4"="red","5"="navy"), labels=c("Suitable giant clam mariculture area","Designated Aquaculture Areas")) + 
   
 geom_polygon(data=ovaka_df,aes(x=long,y=lat,group=group,color=piece),show.legend = TRUE,size=2,fill=NA) +
  scale_color_manual("",labels="Current SMAs",values=c("1"="darkgreen")) +
   geom_point(data=pending_sma_df,aes(x=coords.x1,y=coords.x2,shape=Id),size=3,col="yellow",fill="white") +
   
   # 
     #theme(legend.key = element_rect(fill = "white", colour = NA, size = 0.25))
    scale_shape_manual("",labels="Pending SMAs",values=19) +
   
    # geom_text(data=aqua_labels,aes(x=long,y=lat,label=id),hjust=-1,vjust=-.5)+
    ylab("Latitude") +
    xlab("Longitude") +
    theme_bw() +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
  #  ggtitle("Vava'u Aquaculture areas") +
    coord_fixed(1.03) 
# t+ theme(legend.key=element_rect(fill="lightblue" ,color="white"))
  

over<-gintersect(suit_df,aqua_df)
plot(suit)
plot(over,add=TRUE,col="red")
plot(all_aqua,add=TRUE,col="blue")

ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/giant_clam/aqua_areas.png")
temp_df<-data.frame(aqua_area_df@data)
temp_df$id<-row.names(temp_df)

#temp_df$id<-row.names(temp_df)

all_aqua<-crop(all_aqua,ext)
aqua_df<-merge(tidy_aqua,temp_df,by="id") 
names(aqua_df)<-c("id","long","lat","order","hole","piece","group","Name")






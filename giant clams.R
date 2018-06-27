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
depth[depth>0]<-NA
depth[depth<=-5]<-NA
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

depth<-raster(paste0(boxdir,"/data/tmp/vav_depth.tif"))
depth[depth>0]<-NA
depth[depth<=-5]<-NA
# Turbidity ---------------------------------------------------------------


max_kd<-raster(paste0(boxdir,"/data/raw/pk/max_kd.tif"))
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


max_kd<-raster(paste0(boxdir,"/data/raw/pk/max_kd.tif"))
max_kd[max_kd>=0.101]<-0

# habitat -----------------------------------------------------------------

h_type<-c("subtidal reef flat","shallow terrace with constructions","shallow terrace","reef flat","drowned bank","enclosed lagoon","forereef")

habitat<-readOGR(dsn = paste0(boxdir,"/data/tmp"),layer = "vav_habitat") 

vav_habitat<-habitat[habitat$L4_ATTRIB %in% h_type,]

tidy_habitat<- broom::tidy(vav_habitat) #, region = "L4_ATTRIB")

temp_df<-data.frame(vav_habitat@data)

temp_df$id<-row.names(temp_df)

vav_habitat_df<-merge(tidy_habitat,temp_df,by="id")


ggplot() +
  geom_polygon(data = water, aes(x=long, y=lat, group=group),fill =  "lightblue", alpha =0.5, size = 0.8) +
  geom_polygon(data=vav_habitat_df,aes(x=long, y=lat, group=group, fill=L4_ATTRIB),show.legend = TRUE)+
  scale_fill_viridis("Benthic habitat",discrete=TRUE) +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/giant_clam/benthic_habitat.png")

habitat_raster<-rasterize(vav_habitat,vav_depth, field=1)
habitat_raster_df<-as_data_frame(rasterToPoints(habitat_raster))

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

mangrove<-readOGR(dsn=paste0(boxdir,"/data/VAV_Shapefiles"),layer="Vavau magrove subset")
repro<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
mangrove<-spTransform(mangrove,repro)

mangrove_ext<-c(-174.2, -173.9, -18.9700,  -18.50)
vav_mangrove<-crop(mangrove,mangrove_ext)

mangrove_df<-tidy(vav_mangrove)
temp_df<-vav_mangrove@data
temp_df$id<-row.names(temp_df)

mangrove_df<-merge(mangrove_df,temp_df)

vavau_eez<-readOGR(dsn=paste0(boxdir,"/data/tmp"), layer ="vavau_eez_shape")

cr_vavu_eez<-crop(vavau_eez,mangrove_ext)

tidy_eez<-tidy(cr_vavu_eez)

temp_df<-data.frame(cr_vavu_eez@data)

temp_df$id<-row.names(temp_df)

EEZ_df<-merge(tidy_eez,temp_df,by="id")

cr_land<-EEZ_df %>%
  dplyr::filter(hole==TRUE)

cr_water<-EEZ_df %>%
  dplyr::filter(hole==FALSE)

cr_base<-ggplot() +
  geom_polygon(data = cr_water, aes(x=long, y=lat, group=group),fill =  "lightblue", alpha = 0.5, size = 0.5) + 
  geom_polygon(data = cr_land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.5) + 
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

cr_base +
  geom_polygon(data=mangrove_df,aes(x=long,y=lat,group=group),fill="red")
ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/mangroves.png")

vav_mangrove_raster<-rasterize(vav_mangrove,vav_depth,field=1)

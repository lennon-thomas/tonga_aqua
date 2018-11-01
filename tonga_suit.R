### Tonga aquaculture Suitability
###################################
##Date: May 31 2018
##Author: Lennon Thomas

library(tidyr)
library(tmap)
library(raster)
library(rgdal)
library(sp)
library(Hmisc)
library(ncdf4)
library(broom)


boxdir<-'/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture'

prep_data<- FALSE

# data from https://data.humdata.org/dataset/tonga-administrative-level-0-1-2-and-3-population-statistics#
tonga<-readOGR(paste0(boxdir,"/data/raw/ton_polbnda_adm1_division"), layer="ton_polbnda_adm1_division")

vav<-"Vava'u"

vavau<- tonga[tonga$ADM1_NAME %in% vav,]

repro<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "

vavau<-spTransform(vavau,repro)

writeOGR(vavau, dsn = paste0(boxdir,"/data/tmp"), driver="ESRI Shapefile",layer ="vavau", overwrite = TRUE)

# Crop global datasets from carib_aqua project to Tonga and save ----------

#if (prep_data == TRUE){

eez<-readOGR(dsn = paste0(boxdir,"/data/raw/World_EEZ_v10_20180221"),layer="eez_v10",stringsAsFactors=FALSE)


#depth <- raster(paste0("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/Suitability/raw/rotated_topo30.tif"))

#eez<-crop(eez,depth)

eez_name<-"Tongan Exclusive Economic Zone"

tonga_eez<- eez[eez$GeoName %in% eez_name,]


ext<-c( -174.2542, -173.79, -18.97, -18.5)#-18.15) #-18.5
    
vavau_eez<-crop(tonga_eez,ext)

writeOGR(vavau_eez, dsn=paste0(boxdir,"/data/tmp"),driver="ESRI Shapefile", layer="vavau_eez_shape",overwrite = TRUE)

rm(eez)
#start here
vavau_eez<-readOGR(dsn=paste0(boxdir,"/data/tmp"), layer ="vavau_eez_shape")

tidy_eez<-tidy(vavau_eez)

temp_df<-data.frame(vavau_eez@data)

temp_df$id<-row.names(temp_df)

EEZ_df<-merge(tidy_eez,temp_df,by="id")

land<-EEZ_df %>%
  dplyr::filter(hole==TRUE)

water<-EEZ_df %>%
  dplyr::filter(hole==FALSE)

#data from https://dusk.geo.orst.edu/tonga/mgr/
#depth2<-raster(paste0(boxdir,"/data/raw/map2mgr.grd"))

depth<-raster(paste0(boxdir,"/data/raw/gebco_depth.tif"))

vav_depth<-crop(depth,ext)

#vav_depth[vav_depth>=0]<-NA

#vav_depth<-raster::mask(vav_depth,vavau_eez)

#zero_value<-Which(vav_depth>0,cells=TRUE)

#vav_depth[zero_value]<-0

#writeRaster(vav_depth,paste0(boxdir,"/data/tmp/vav_depth.tif"),overwrite = TRUE)

vav_depth<-raster(paste0(boxdir,"/data/tmp/vav_depth.tif"))
#Plot

vav_depth[vav_depth<=-6]<-NA
vav_depth[vav_depth>=1]<-NA

depth_df<-dplyr::as_data_frame(rasterToPoints(vav_depth)) 

ggplot() +
  geom_raster(data=depth_df,aes(x=x,y=y,fill = vav_depth),title="Depth (m)")+
  scale_fill_continuous("Depth (m)")+#,low="lightblue",high="navy") +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.5) + 
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))+
  coord_fixed(1.03) 
 
ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/depth.png")

rm(depth)
# data from http://datadownload.unep-wcmc.org


coral<- readOGR(dsn = paste0(boxdir, "/data/raw/14_001_WCMC008_CoralReefs2010_v3/01_Data"),layer = "WCMC008_CoralReef2010_Py_v3")

repro2<-"+proj=tmerc +lat_0=0 +lon_0=-177 +k=0.9996 +x_0=1500000 +y_0=5000000 +ellps=GRS80 +units=m +no_defs"


tonga_coral<-crop(coral,ext)

coral_sp<-spTransform(tonga_coral,repro2)

coral_sp_buff<-gBuffer(coral_sp,width=100,byid=TRUE)

tonga_coral<-spTransform(coral_sp_buff,repro)

tonga_coral_raster<-rasterize(tonga_coral,vav_depth,field=2,progress='text')

#tonga_coral_raster[is.na(tonga_coral_raster)]<-0

coral_df<-as_data_frame(rasterToPoints(tonga_coral_raster))

writeRaster(tonga_coral_raster,paste0(boxdir,"/data/tmp/vav_coral_raster.tif"),overwrite = TRUE)

ggplot() +
  geom_polygon(data = water, aes(x=long, y=lat, group=group),fill =  "lightblue",alpha = 0.5, size = 0.8) +
  geom_raster(data=coral_df,aes(x=x,y=y,fill = layer),show.legend = FALSE) +
  scale_fill_continuous("Coral habitat", high="orange",low=NULL) +
 # geom_polygon(data = water,aes(x=long,y=lat,group=group),fill = "lightblue") +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
 
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed(1.03) 

ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/coral.png")
rm(coral)
#tonga_coral_raster<-raster::mask(tonga_coral_raster,vav_depth,maskvalue=NA,inverse=FALSE,filename=paste(boxdir,"/data/tmp/tonga_coral_raster.tif",sep=""),overwrite=TRUE)


mangrove<-readOGR(dsn = paste0(boxdir,"/data/raw/mangroves/01_Data"),layer = "14_001_WCMC010_MangroveUSGS2011_v1_3")

tonga_mangrove<-crop(mangrove,ext)

writeOGR(tonga_mangrove,dsn=paste0(boxdir,"/data/tmp"),driver="ESRI Shapefile", layer="tonga_mangrove",overwrite = TRUE)
tonga_mangrove<-readOGR(dsn =paste0(boxdir,"/data/tmp"), layer ="tonga_mangrove")
tonga_mangrove_raster<-rasterize(tonga_mangrove,vav_depth,field=2)

tonga_mangrove_raster[is.na(tonga_mangrove_raster)]<-0

writeRaster(tonga_mangrove_raster,paste0(boxdir,"/data/tmp/vav_mangrove_raster.tif"))

mangrove_df<-as.data.frame(rasterToPoints(tonga_mangrove_raster))


tidy_mangrove<-tidy(tonga_mangrove)

temp_df<-data.frame(tonga_mangrove@data)



mangrove_df<-merge(tidy_mangrove,temp_df)



ggplot() +
  geom_polygon(data=mangrove_df,aes(x=long, y=lat, group=group),fill="darkgreen",show.legend = FALSE)+
 # scale_fill_continuous("Mangrove habitat", high="darkgreen",low="lightblue") +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))+
  coord_fixed(1.03) 
ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/mangrove.png")

rm(mangrove)

# Habitat data from  Millennium Coral Reef Mapping Project,
habitat<-readOGR(dsn = paste0(boxdir,"/data/raw/mill"),layer = "Tonga_v6")

vav_habitat<-crop(habitat,vavau_eez)

writeOGR(vav_habitat,dsn = paste0(boxdir,"/data/tmp"),driver="ESRI Shapefile",layer = "vav_habitat",overwrite = TRUE)

tidy_habitat<- broom::tidy(vav_habitat) #, region = "L4_ATTRIB")

temp_df<-data.frame(vav_habitat@data)

temp_df$id<-row.names(temp_df)

vav_habitat_df<-merge(tidy_habitat,temp_df,by="id")


ggplot() +
  geom_polygon(data = water, aes(x=long, y=lat, group=group),fill =  "lightblue", alpha =0.5, size = 0.5) +
  geom_polygon(data=vav_habitat_df,aes(x=long, y=lat, group=group, fill=L4_ATTRIB),show.legend = TRUE)+
  scale_fill_viridis("Benthic habitat",discrete=TRUE) +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed(1.03) 

ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/benthic_habitat.png")


# Mangrove ----------------------------------------------------------------


mangrove<-readOGR(dsn=boxdir,layer="/data/raw/Vavau magrove subset")
mangrove_df<-tidy(mangrove)
temp_df<-mangrove@data
mangrove_df<-merge(mangrove_df,temp_df)
base+
  geom_polygon(data=mangrove_df,aes(x=long,y=lat,group=group),fill="green",alpha=0.5)


# Current -----------------------------------------------------------------

#u_current<-brick(paste0(boxdir,"/data/raw/oscar_vel2017.nc"),varname="u")
#extent(u_current)<-extent(depth)
#u_current<-crop(u_current,ext)
#crs(u_current)<-repro
ext<-c(-174.254200, -173.790000,  -18.970000 , -18.500000)
v_current<-brick(paste0(boxdir,"/data/raw/oscar_vel2017.nc"),varname="v")
extent(v_current)<-extent(depth)
v_current<-projectExtent(v_current,depth)
extent(v_current)<-extent(depth)
v_current<-crop(v_current,ext)
max_u<-raster::calc(u_current,function(x){max(x)})

#um_current<-brick(paste0(boxdir,"/data/raw/oscar_vel2017.nc"),varname="um")

#vm_current<-brick(paste0(boxdir,"/data/raw/oscar_vel2017.nc"),varname="vm")




## Shiping

#nav_chart<-raster(paste0(boxdir,"/data/raw/chart/chart-nz-82-tonga.tif"))

ship<-raster(paste0(boxdir,"/data/raw/ships_proj.tif"))
#crs(ship)<-"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

#sp_ship<-projectRaster(ship,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

vav_ship<-crop(ship,ext)

ship_df<-as_data_frame(rasterToPoints(vav_ship))
ggplot() +
  geom_raster(data=ship_df,aes(x=x,y=y,fill = ships_proj),show.legend = TRUE) +
  scale_fill_continuous("Relative shipping activity", low="lightblue",high="darkred") +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

  
ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/shipping.png")



# Salinity

max_sal<-raster(paste0(boxdir,"/data/raw/Present.Surface.Salinity.Max.tif"))
vav_max_sal<-crop(max_sal,ext,snap="out")
vav_max_sal<-resample(vav_max_sal, vav_depth,method="bilinear")
writeRaster(vav_max_sal, filename=paste0(boxdir,'/data/tmp/max_salinity.tif'),overwrite=TRUE)

max_sal_df<-dplyr::as_data_frame(rasterToPoints(vav_max_sal))
max.sal<-ggplot() +
  geom_raster(data=max_sal_df,aes(x=x,y=y,fill = Present.Surface.Salinity.Max),show.legend = TRUE) +
  scale_fill_gradientn("Salinity (ppt)",colors= rev(terrain.colors(10))) +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
  xlab("Longitude") +
  ylab("") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle("(b) Max") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed(1.03) 

ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/max_salinity.png")

min_sal<-raster(paste0(boxdir,"/data/raw/Present.Surface.Salinity.Min.tif"))
vav_min_sal<-crop(min_sal,ext,snap="out")
vav_min_sal<-resample(vav_min_sal,vav_depth, method="bilinear")
writeRaster(vav_min_sal, filename=paste0(boxdir,'/data/tmp/min_salinity.tif'),overwrite = TRUE)

min_sal_df<-dplyr::as_data_frame(rasterToPoints(vav_min_sal))

mi.sal<-ggplot() +
  geom_raster(data=min_sal_df,aes(x=x,y=y,fill = Present.Surface.Salinity.Min),show.legend = TRUE) +
  scale_fill_gradientn("Salinity (ppt)",colors= rev(terrain.colors(10)))+
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
  xlab("") +
  ylab("Latitude") +
  theme_bw() +
  ggtitle("(b) Min") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed(1.03) 

ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/min_salinity.png")


mean_sal<-raster(paste0(boxdir,"/data/raw/Present.Surface.Salinity.Mean.tif"))
vav_mean_sal<-crop(mean_sal,ext,snap="out")
vav_mean_sal<-resample(vav_mean_sal,vav_depth, method="bilinear")
writeRaster(vav_mean_sal, filename=paste0(boxdir,'/data/tmp/mean_salinity.tif'),overwrite=TRUE)

avg_sal_df<-dplyr::as_data_frame(rasterToPoints(vav_mean_sal))

mean.sal<-ggplot() +
  geom_raster(data=avg_sal_df,aes(x=x,y=y,fill = Present.Surface.Salinity.Mean),show.legend = TRUE) +
  scale_fill_gradientn("Salinity (ppt)",colors= rev(terrain.colors(10))) +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
  xlab("") +
  ylab("") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle("(c) Mean") +
  coord_fixed(1.03) 
ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/mean_salinity.png")


ggarrange(mi.sal,max.sal,mean.sal,ncol=3,nrow=1,common.legend = FALSE,legend="top",
          label.x=0,label.y=0)
ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/all_salinity.png")
#### Storm Datahttps://brycemecum.com/2014/02/18/working-with-netcdf-files-in-r/

storm_shape<-readOGR(paste0(boxdir,"/data/raw/storms"),layer="Basin.SP.ibtracs_all_lines.v03r10")
#storm_points<-readOGR(paste0(boxdir,"/data/raw/storms"),layer="Basin.SP.ibtracs_all_points.v03r10")


vav_storm_l<-crop(storm_shape,ext)
#vav_storm_p<-crop(storm_points,ext)
writeOGR(vav_storm_l,dsn=paste0(boxdir,"/data/tmp/storms"),driver="ESRI Shapefile",layer="vav_storms")

vav_storm_l<-readOGR(dsn = paste0(boxdir,"/data/tmp/storms"), layer = "vav_storms")

vav_storm_l<-crop(vav_storm_l,ext)

tidy_storml<-tidy(vav_storm_l)

temp_df<-data.frame(vav_storm_l@data)

temp_df$id<-row.names(temp_df)
 

storml_df<-merge(tidy_storml,temp_df,by="id") %>%
 dplyr:: group_by(Name) %>%
  dplyr::arrange(hour,.by_group=TRUE) %>%
 dplyr:: mutate(max_wind=max(wmo_wind,na.rm=TRUE)) %>%
  dplyr::ungroup()

#look these up manually for cloese storm
storml_df$max_wind[storml_df$max_wind==-999]<-90
storml_df$max_wind[storml_df$max_wind==0]<-60

write.csv(storml_df,paste0(boxdir,"/data/tmp/storm.csv"))

ggplot() +

 # scale_fill_continuous("Salinity (ppt)") +
  geom_polygon(data = water, aes(x=long, y=lat, group=group),alpha= 0.5,fill =  "lightblue", colour = "black", size = 0.5) +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.5) +
  geom_line(data=storml_df,aes(x=long,y=lat,size=max_wind,color=Name), lty="dashed")+#arrow=arrow(ends = c("last"))) +
  scale_color_discrete("Storm Name") +
  scale_size_continuous("Average Wind Speed (kt)",range=c(0,2)) +
 # geom_point(data=stormp_df,aes(x=coords.x1,y=coords.x2,color=Name),size=2) +
  ylab("Latitude") +
  xlab("Longitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed(1.03) 
ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/storms.png")


# Aquaculture areas -------------------------------------------------------

aqua_df <- read.csv(paste0(boxdir,"/data/vav_aqua_areas.csv")) %>%
  dplyr::mutate(Lat1 = Lat*-1,
         Long1 = Long * -1) %>%
  dplyr::select(Lat1,Long1,Area)

coords<-aqua_df[,c("Long1","Lat1")]

crs(coords)<-repro

mounu<- Polygon(coords[c(1:4),])
oloua<- Polygon(coords[c(5:8),])
otea<- Polygon(coords[c(9:12),])
otumoto<- Polygon(coords[c(13:16),])
pangamotu<- Polygon(coords[c(17:20),])
#toku<- Polygon(coords[c(21:24),])
koko<- Polygon(coords[c(25:28),])
bay_vv<- Polygon(coords[c(29:32),])
bay_vv2<- Polygon(coords[c(33:36),])
matamaka<- Polygon(coords[c(37:40),])
utungake<- Polygon(coords[c(41:44),])
utungake2<-Polygon(coords[c(45:48),])
utungake3<-Polygon(coords[c(49:52),])
aloitalau<-Polygon(coords[c(53:56),])
holeva<-Polygon(coords[c(57:60),])
feletoa<-Polygon(coords[c(61:64),])
vaipua1<-Polygon(coords[c(65:68),])
vaipua2<-Polygon(coords[c(69:72),])
vaipua3<-Polygon(coords[c(73:76),])
tuanuku<-Polygon(coords[c(77:80),])


mounu<-Polygons(list(mounu),"Mounu Is")

oloua<-Polygons(list(oloua),"Oloua")

otea<-Polygons(list(otea),"Otea")

otumoto<-Polygons(list(otumoto),"Otumotu hahake Vv")

pangamotu<-Polygons(list(pangamotu), "Pangamotu Vv")

#toku<-Polygons(list(toku),"Toku Is")

koko<-Polygons(list(koko),"Koko Bay Vv")

bay_vv<-Polygons(list(bay_vv),"Bay of Vv")

bay_vv2<-Polygons(list(bay_vv2),"Bay of Vv2")

matamaka<-Polygons(list(matamaka),"Matamaka")

utungake<-Polygons(list(utungake),"Utungake1")

utungake2<-Polygons(list(utungake2),"Utungake2")

utungake3<-Polygons(list(utungake3),"Utungake3")

aloitalau<-Polygons(list(aloitalau),"Aloitalau")

holeva<-Polygons(list(holeva),"Holeva")

feletoa<-Polygons(list(feletoa),"Feletoa")

vaipua1<-Polygons(list(vaipua1),"Vaipua1")

vaipua2<-Polygons(list(vaipua2),"Vaipua2")

vaipua3<-Polygons(list(vaipua3),"Vaipua3")

tuanuku<-Polygons(list(tuanuku),"Tuanuku lakes")

all_aqua<-SpatialPolygons(list(mounu,oloua,otea,otumoto,pangamotu,koko,bay_vv,bay_vv2,matamaka,utungake,utungake2,
                               aloitalau,holeva,feletoa,vaipua1,vaipua2,vaipua3,tuanuku))

r<-c("Utungake3","Toku")
names<-as.data.frame(as.character(unique(aqua_df$Area)))%>%
  dplyr::filter(!as.character(unique(aqua_df$Area)) %in% r)
 # dplyr::filter(!as.character(unique(aqua_df$Area))==  "Utungake3") %>%
  #dplyr::filter(!as.character(unique(aqua_df$Area))==  "Toku") 

row.names(names)<-c("Mounu Is","Oloua","Otea","Otumotu hahake Vv","Pangamotu Vv","Koko Bay Vv","Bay of Vv","Bay of Vv2",
                    "Matamaka", "Utungake1","Utungake2", "Aloitalau","Holeva","Feletoa", "Vaipua1","Vaipua2","Vaipua3","Tuanuku lakes")                

crs(all_aqua)<-repro

aqua_area_df<-SpatialPolygonsDataFrame(all_aqua,names)
aqua_area_df<-crop(all_aqua,ext)
writeOGR(aqua_area_df,dsn=paste0(boxdir,"/data/tmp"),driver = "ESRI Shapefile",layer="aqua_areas",overwrite=TRUE)



tidy_aqua<-tidy(aqua_area_df)

temp_df<-data.frame(aqua_area_df@data)
temp_df$id<-row.names(temp_df)

#temp_df$id<-row.names(temp_df)


aqua_df<-merge(tidy_aqua,temp_df,by="id") 
names(aqua_df)<-c("id","long","lat","order","hole","piece","group","Name")

aqua_labels<-aqua_df %>%
 dplyr:: select(id,long,lat) %>%
 dplyr:: group_by(id) %>%
  dplyr::top_n(1,wt=long)

ovaka<-readOGR(dsn=paste0(boxdir,"/data/VAV_Shapefiles"),layer="Ovaka SMA Boundary")
ovaka_df<-tidy(ovaka) %>%
  filter(id!=0 & id!=1)

pending_sma<-readOGR(dsn=paste0(boxdir,"/data/VAV_Shapefiles"),layer="Pending SMA")
pending_sma_df<-as_data_frame(pending_sma)


#cr_base+
 

aqua_sma<-
ggplot() +
  geom_polygon(data=water,aes(x=long,y=lat,group=group),fill="lightblue",alpha=0.5) +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
  geom_polygon(data=aqua_df,aes(x=long,y=lat,group=group),colour="red",fill="navy",alpha=0.8) +
  geom_polygon(data=ovaka_df,aes(x=long,y=lat,group=group),color="red",fill=NA) +
  geom_point(data=pending_sma_df,aes(x=coords.x1,y=coords.x2),color="red",size=3) +

  # geom_text(data=aqua_labels,aes(x=long,y=lat,label=id),hjust=-1,vjust=-.5)+
  ylab("Latitude") +
  xlab("Longitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle("Vava'u Aquaculture areas") +
  coord_fixed(1.03) 
  ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/aqua_areas.png")



# add population data -----------------------------------------------------


vav<-"Vava'u"



village<-readOGR(paste0(boxdir,"/data/raw/ton_polbnda_adm3_village"), layer="ton_polbnda_adm3_village")

village<- village[village$ADM1_NAME %in% vav,]

village<-spTransform(village,repro)
  
pop_stats<-read.csv(paste0(boxdir,"/data/raw/ton_pplp_adm3_village.csv")) %>%
  dplyr::filter(adm1_name == vav) %>%
  dplyr::select(c(1:10)) %>%
  dplyr::mutate(log_T_Pop = log(T_Pop+0.01))



village_tidy <- broom::tidy(village)
temp_df<-village@data
temp_df$id<-row.names(temp_df)

village_df<-merge(village_tidy,temp_df)

village_pop<-merge(village_df,pop_stats,by.x="ADM3_NAME",by.y="adm3_name")

avg_chl<-raster(paste0(boxdir,"/data/raw/chl/average_chl.tif"))
crp_chl<-crop(avg_chl,village)
avg_chl_df<-as_data_frame(rasterToPoints(crp_chl)) 



chl<-
  ggplot() +
  geom_raster(data=avg_chl_df,aes(x=x,y=y,fill = average_chl)) +
  scale_fill_continuous("Chl_a",low="yellow",high="darkgreen") +
  geom_polygon(data=village_pop,aes(x=long,y=lat,group=group),fill="white",col="black")+
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle("Average from 2017") +
  coord_fixed(1.03) 

pop<-
ggplot()+
#  geom_raster(data=avg_chl_df,aes(x=x,y=y,fill=average_chl))+
 # scale_fill_continuous("Chl_a") +
  geom_polygon(data=village_pop,aes(x=long,y=lat,group=group,fill=T_Pop),col="black")+
  theme_bw() +
  scale_fill_viridis("Population",direction=-1) +
  ylab("Latitude") +
  xlab("Longitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed(1.03) 


ggarrange(pop,chl)

ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/vav_poulation.png")






wave<-brick(paste0(boxdir,"/data/raw/ssha_swh_5day_mean.nc"))
repro<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "

wave_repro<-projectRaster(wave,depth)

tonga_depth<-crop(depth,ext)

tonga_depth<-raster::mask(tonga_depth,tonga_eez)

tonga_depth<-raster::calc(tonga_depth,fun=function(x){ifelse(x<=0,(x*-1),NA)},progress='text',filename=paste0(boxdir,'/data/tmp/tonga_depth.tif'),overwrite=TRUE)


wri_mpa<-readOGR(dsn = paste0(boxdir,"/data/raw/MPAs"), layer ="MPAs_poly")

wri_mpa2<-spTransform(wri_mpa,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

tonga_wri_mpa<-crop(wri_mpa2,ext)

tonga_mpa_raster<-rasterize(tonga_wri_mpa,tonga_depth,field=1,progress='text')

tonga_mpa_raster<-mask(tonga_mpa_raster,tonga_eez)

tonga_mpa_raster<-resample(tonga_mpa_raster,tonga_depth,progress='text',filename=paste(boxdir,'/data/tmp/tonga_mpa_raster.tif',sep=""),overwrite=TRUE)



# mangrove ----------------------------------------------------------------

mangrove<-readOGR(dsn=paste0(boxdir,"/data/VAV_Shapefiles"),layer="Vavau magrove subset")
buf_mangrove<-gBuffer(mangrove,width=100,byid = TRUE)
t_mangrove<-spTransform(buf_mangrove,repro)
final_mangrove<-crop(t_mangrove,vav_depth)


writeOGR(final_mangrove,dsn=paste0(boxdir,"/data/tmp"),driver="ESRI Shapefile",layer='mangrove_shp',overwrite_layer = TRUE)

mangrove_df<-tidy(final_mangrove)
temp_df<-mangrove@data
temp_df$id<-row.names(temp_df)
mangrove_df<-merge(mangrove_df,temp_df)



ggplot() +
  geom_polygon(data=water,aes(x=long,y=lat,group=group),fill="lightblue",alpha=0.5) +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.5) +
  geom_polygon(data=mangrove_df,aes(x=long,y=lat,group=group),fill="red",alpha=0.7) +
   ylab("Latitude") +
  xlab("Longitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
 # ggtitle("Vava'u Aquaculture areas") +
  coord_fixed(1.03) 
ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/mangrove.png")


  

tonga_mangrove@data$CTYPE<-as.character(tonga_mangrove@data$CTYPE)
tonga_mangrove_raster<-rasterize(tonga_mangrove,tonga_depth,field=5,progress='text')
tonga_mangrove_raster[is.na(tonga_mangrove_raster)]<-0
tonga_mangrove_raster<-mask(tonga_mangrove_raster,tonga_depth,maskvalue=NA,inverse=FALSE,filename=paste(boxdir,"/data/tmp/tonga_mangrove_raster.tif",sep=""),overwrite=TRUE)


#writeRaster(tonga_mangrove_raster, paste0(boxdir,"/data/tmp/tonga_mangrove_raster.tif"))



writeRaster(tonga_seagrasse_raster, paste0(boxdir,"/data/tmp/tonga_seagrass_raster.tif"))


# waves -------------------------------------------------------------------

wave<-brick(paste0(boxdir,"/data/raw/wave/mean_wave_height"))

wave[wave==32767]<-NA

wave[wave==-32767]<-NA

max_wave<-calc(wave, function(x){max(x)})

min_wave<-calc(wave, function(x){min(x)})


re_max_wave<-resample(max_wave,vav_depth,method="bilinear")
re_min_wave<-resample(min_wave,vav_depth,method="bilinear")

max_wave_df<-as_data_frame(rasterToPoints(re_max_wave))
min_wave_df<-as_data_frame(rasterToPoints(re_min_wave))


cr_max_wave<-crop(re_max_wave,mangrove_ext)

cr_min_wave<-crop(re_min_wave,mangrove_ext)

cr_wave_df<-as_data_frame(rasterToPoints(cr_max_wave))

cr_wave_df<-as_data_frame(rasterToPoints(cr_min_wave))
cr_base

ggplot()+
  geom_raster(data=cr_wave_df,aes(x=x,y=y,fill=layer)) +
  scale_fill_viridis("Min mean wave height (m)")+
  geom_polygon(data=cr_land,aes(x=long,y=lat,group=group), fill="white",col="black",line=0.8) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/min_wave_zoom.png")


} else {
  
tonga_eez<-readOGR(paste0(boxdir,"/data/tmp"), layer ="tonga_eez_shape")  

tonga_depth<-raster(paste0(boxdir,"/data/tmp/tonga_depth.tif"))
tonga_mangrove<-readOGR(dsn=paste0(boxdir,"/data/tmp"),layer="tonga_mangrove") 
  
}


# curren --------------------------------------------------------------------

max_current<-raster(paste0(boxdir,"/data/raw/current/max.tif"))
min_current<-raster(paste0(boxdir,"/data/raw/current/min.tif"))
mean.current<-raster(paste0(boxdir,"/data/raw/current/mean.tif"))

current<-stack(max_current,min_current,mean.current)

vav_current<-crop(current,ext)
vav_current<-resample(vav_current,vav_depth)
#vav_current<-crop(vav_current,mangrove_ext)
vav_current_df<-as_data_frame(rasterToPoints(vav_current))

    
ggplot()+
  geom_raster(data=vav_current_df,aes(x=x,y=y,fill=min)) +
  scale_fill_viridis("Min surface current velocity (m-1)")+
  geom_polygon(data=land,aes(x=long,y=lat,group=group), fill="white",col="black",line=0.8) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed(1.03) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/min_current.png")


# plots -------------------------------------------------------------------

temp_df <- data.frame(tonga_eez@data)
temp_df$id <- 8

tidy_eez<-tidy(tonga_eez)

EEZ_df <- merge(tidy_eez, temp_df, by="id")


eez.land <- EEZ_df %>%
  filter(hole == "TRUE")

eez.water <- EEZ_df %>%
  filter(hole == FALSE)

base <- ggplot() + 
  geom_polygon(data = water,aes(x = long,y = lat,group = group), fill =  "lightblue", colour = "black", size = 0.5 , alpha = 0.5) +
  geom_polygon(data = land,aes(x = long,y = lat,group = group), fill =  "brown", colour = "black", size = 0.5) +
  theme(legend.position="none") +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed(1.03) 
  theme(plot.margin=grid:
 # coord_fixed(xlim =c(-85.5,-57.4),ylim = c(9.95,30))

mangrove_df<-fortify(tonga_mangrove)
tonga_mangrove@data<-  merge(tonga_mangrove@data,mangrove_df)

View(base+
  geom_polygon(data=tonga_mangrove,aes(x=polygons[[Polygons$labpt[1]]],y=polygons$labpt[2]),fill="red")
# Read in Tonga specific datasets -----------------------------------------

aqua_areas<-read_csv(paste0(boxdir,"/data/vav_aqua_areas.csv"))

deep_bioreg<-readOGR(dsn = paste0(boxdir,"/Tonga_deepwater_bioregions"),layer = "Tonga_deepwater_bioregions")

coast_bioreg<-readOGR(dsn = paste0(boxdir,"/Tonga_deepwater_bioregions"),layer = "Tonga_reef-associated_bioregions")

suma<-readOGR(dsn = paste0(boxdir,"/Tonga_deepwater_bioregions"),layer = "Tonga_reef-associated_bioregions")

###


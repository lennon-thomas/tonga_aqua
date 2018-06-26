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

if (prep_data == TRUE){

eez<-readOGR(dsn = paste0(boxdir,"/data/raw/World_EEZ_v10_20180221"),layer="eez_v10",stringsAsFactors=FALSE)


#depth <- raster(paste0("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/Suitability/raw/rotated_topo30.tif"))

#eez<-crop(eez,depth)

eez_name<-"Tongan Exclusive Economic Zone"

tonga_eez<- eez[eez$GeoName %in% eez_name,]


ext<-c( -174.2542, -173.79, -18.97, -18.15) #-18.5
    
vavau_eez<-crop(tonga_eez,ext)

writeOGR(vavau_eez, dsn=paste0(boxdir,"/data/tmp"),driver="ESRI Shapefile", layer="vavau_eez_shape",overwrite = TRUE)

vavau_eez<-readOGR(dsn=paste0(boxdir,"/data/tmp"), layer ="vavau_eez_shape")

tidy_eez<-tidy(vavau_eez)

temp_df<-data.frame(vavau_eez@data)

temp_df$id<-row.names(temp_df)

EEZ_df<-merge(tidy_eez,temp_df,by="id")

land<-EEZ_df %>%
  dplyr::filter(hole==TRUE)

water<-EEZ_df %>%
  dplyr::filter(hole==FALSE)
rm(eez)
#data from https://dusk.geo.orst.edu/tonga/mgr/
#depth2<-raster(paste0(boxdir,"/data/raw/map2mgr.grd"))

depth<-raster(paste0(boxdir,"/data/raw/gebco_depth.tif"))

vav_depth<-crop(depth,ext)

#vav_depth[vav_depth>=0]<-NA

#vav_depth<-raster::mask(vav_depth,vavau_eez)

#zero_value<-Which(vav_depth>0,cells=TRUE)

#vav_depth[zero_value]<-0

writeRaster(vav_depth,paste0(boxdir,"/data/tmp/vav_depth.tif"),overwrite = TRUE)

#Plot



depth_df<-as_data_frame(rasterToPoints(vav_depth)) %>%
  mutate(depth2 = gebco_depth *-1)

ggplot() +
  geom_raster(data=depth_df,aes(x=x,y=y,fill = gebco_depth),title="Depth (m)")+
  scale_fill_continuous("Depth (m)")+#,low="lightblue",high="navy") +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) + 
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))
 
ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/depth.png")

rm(depth)
# data from http://datadownload.unep-wcmc.org
coral<- readOGR(dsn = paste0(boxdir, "/data/raw/14_001_WCMC008_CoralReefs2010_v3/01_Data"),layer = "WCMC008_CoralReef2010_Py_v3")

tonga_coral<-crop(coral,ext)

tonga_coral_raster<-rasterize(tonga_coral,vav_depth,field=2,progress='text')

tonga_coral_raster[is.na(tonga_coral_raster)]<-0

coral_df<-as_data_frame(rasterToPoints(tonga_coral_raster))

writeRaster(tonga_coral_raster,paste0(boxdir,"/data/tmp/vav_coral_raster.tif"),overwrite = TRUE)

ggplot() +
  geom_raster(data=coral_df,aes(x=x,y=y,fill = layer),show.legend = FALSE) +
  scale_fill_continuous("Coral habitat", high="orange",low="lightblue") +
 # geom_polygon(data = water,aes(x=long,y=lat,group=group),fill = "lightblue") +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
 
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

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

mangrove_df<-as_data_frame(rasterToPoints(tonga_mangrove_raster))


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
  scale_y_continuous(expand = c(0,0))
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
  geom_polygon(data = water, aes(x=long, y=lat, group=group),fill =  "lightblue", colour = "black", size = 0.8) +
  geom_polygon(data=vav_habitat_df,aes(x=long, y=lat, group=group, fill=L4_ATTRIB),show.legend = TRUE)+
  scale_fill_viridis("Benthic habitat",discrete=TRUE) +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/benthic_habitat.png")


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

max_sal_df<-as_data_frame(rasterToPoints(vav_max_sal))
ggplot() +
  geom_raster(data=max_sal_df,aes(x=x,y=y,fill = Present.Surface.Salinity.Max),show.legend = TRUE) +
  scale_fill_gradientn("Salinity (ppt)",colors= rev(terrain.colors(10))) +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle("Max Salinity (ppt)") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/max_salinity.png")

min_sal<-raster(paste0(boxdir,"/data/raw/Present.Surface.Salinity.Min.tif"))
vav_min_sal<-crop(min_sal,ext,snap="out")
vav_min_sal<-resample(vav_min_sal,vav_depth, method="bilinear")
writeRaster(vav_min_sal, filename=paste0(boxdir,'/data/tmp/min_salinity.tif'),overwrite = TRUE)

min_sal_df<-as_data_frame(rasterToPoints(vav_min_sal))

ggplot() +
  geom_raster(data=min_sal_df,aes(x=x,y=y,fill = Present.Surface.Salinity.Min),show.legend = TRUE) +
  scale_fill_gradientn("Salinity (ppt)",colors= rev(terrain.colors(10)))+
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  ggtitle("Min Salinity (ppt)") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/min_salinity.png")


mean_sal<-raster(paste0(boxdir,"/data/raw/Present.Surface.Salinity.Mean.tif"))
vav_mean_sal<-crop(mean_sal,ext,snap="out")
vav_mean_sal<-resample(vav_mean_sal,vav_depth, method="bilinear")
writeRaster(vav_mean_sal, filename=paste0(boxdir,'/data/tmp/mean_salinity.tif'),overwrite=TRUE)

avg_sal_df<-as_data_frame(rasterToPoints(vav_mean_sal))

ggplot() +
  geom_raster(data=avg_sal_df,aes(x=x,y=y,fill = Present.Surface.Salinity.Mean),show.legend = TRUE) +
  scale_fill_gradientn("Salinity (ppt)",colors= rev(terrain.colors(10))) +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle("Average Salinity (ppt)")
ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/mean_salinity.png")


#### Storm Datahttps://brycemecum.com/2014/02/18/working-with-netcdf-files-in-r/

storm_shape<-readOGR(paste0(boxdir,"/data/raw/storms"),layer="Basin.SP.ibtracs_all_lines.v03r10")
#storm_points<-readOGR(paste0(boxdir,"/data/raw/storms"),layer="Basin.SP.ibtracs_all_points.v03r10")


vav_storm_l<-crop(storm_shape,ext)
#vav_storm_p<-crop(storm_points,ext)
writeOGR(vav_storm_l,dsn=paste0(boxdir,"/data/tmp/storms"),driver="ESRI Shapefile",layer="vav_storms")

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
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
  geom_line(data=storml_df,aes(x=long,y=lat,size=max_wind,color=Name), lty="dashed")+#arrow=arrow(ends = c("last"))) +
  scale_color_discrete("Storm Name") +
  scale_size_continuous("Average Wind Speed (kt)",range=c(0,2)) +
 # geom_point(data=stormp_df,aes(x=coords.x1,y=coords.x2,color=Name),size=2) +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))
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
toku<- Polygon(coords[c(21:24),])
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

toku<-Polygons(list(toku),"Toku Is")

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

all_aqua<-SpatialPolygons(list(mounu,oloua,otea,otumoto,pangamotu,toku,koko,bay_vv,bay_vv2,matamaka,utungake,utungake2,
                               aloitalau,holeva,feletoa,vaipua1,vaipua2,vaipua3,tuanuku))

names<-as.data.frame(as.character(unique(aqua_df$Area)))%>%
  dplyr::filter(!as.character(unique(aqua_df$Area))==  "Utungake3")

row.names(names)<-c("Mounu Is","Oloua","Otea","Otumotu hahake Vv","Pangamotu Vv","Toku Is","Koko Bay Vv","Bay of Vv","Bay of Vv2",
                    "Matamaka", "Utungake1","Utungake2", "Aloitalau","Holeva","Feletoa", "Vaipua1","Vaipua2","Vaipua3","Tuanuku lakes")                

crs(all_aqua)<-repro

aqua_area_df<-SpatialPolygonsDataFrame(all_aqua,names)

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

ggplot() +
  geom_polygon(data=water,aes(x=long,y=lat,group=group),fill="lightblue",alpha=0.5) +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
  geom_polygon(data=aqua_df,aes(x=long,y=lat,group=group),colour="red",fill="navy",alpha=0.8) +
 # geom_text(data=aqua_labels,aes(x=long,y=lat,label=id),hjust=-1,vjust=-.5)+
  ylab("Latitude") +
  xlab("Longitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle("Vava'u Aquaculture areas")
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
  ggtitle("Average from 2017")

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
  scale_y_continuous(expand = c(0,0))


ggarrange(pop,chl)

ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/vav_poulation.png")




ggplot()+
  geom_polygon(data=water,aes(x=lat,y=long,group=group),fill="lightblue",alpha=0.5)+
  geom_raster(data=village_pop,aes(x=lat,y=long,group=group,fill="T_Pop"),color="black")+
 # geom_point(data=village_pop,aes(x=lat))
  ylab("Latitude") +
  xlab("Longitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))



crs(test)<-repro
ggplot() + 
  geom_polygon(data = vav_habitat_df, aes(x = long, y = lat, group = group), colour = "black") +
  scale_fill_discrete(name ="Habitat Type") +
  theme_void() +
  geom_polygon(data = all_aqua,aes(x=long,y=lat,group=group),fill="yellow")


ggplot() + 
  geom_polygon(data = village_df, aes(x = long, y = lat, group = group, fill = log_T_Pop), colour = "black") +
  scale_fill_continuous(name ="log(Population)") +
  theme_void() +
  geom_polygon(data = vavau_eez,aes(x=long,y=lat,group=group),fill=NA,col="black")


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







tonga_mangrove@data$CTYPE<-as.character(tonga_mangrove@data$CTYPE)
tonga_mangrove_raster<-rasterize(tonga_mangrove,tonga_depth,field=5,progress='text')
tonga_mangrove_raster[is.na(tonga_mangrove_raster)]<-0
tonga_mangrove_raster<-mask(tonga_mangrove_raster,tonga_depth,maskvalue=NA,inverse=FALSE,filename=paste(boxdir,"/data/tmp/tonga_mangrove_raster.tif",sep=""),overwrite=TRUE)


#writeRaster(tonga_mangrove_raster, paste0(boxdir,"/data/tmp/tonga_mangrove_raster.tif"))



writeRaster(tonga_seagrasse_raster, paste0(boxdir,"/data/tmp/tonga_seagrass_raster.tif"))







} else {
  
tonga_eez<-readOGR(paste0(boxdir,"/data/tmp"), layer ="tonga_eez_shape")  

tonga_depth<-raster(paste0(boxdir,"/data/tmp/tonga_depth.tif"))
tonga_mangrove<-readOGR(dsn=paste0(boxdir,"/data/tmp"),layer="tonga_mangrove") 
  
}



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
  geom_polygon(data = eez.water,aes(x = long,y = lat,group = group), fill =  "lightblue", colour = "black", size = 0.07 , alpha = 0.5) +
  geom_polygon(data = eez.land,aes(x = long,y = lat,group = group), fill =  "brown", colour = "black", size = 0.07) +
  theme(legend.position="none") +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") 
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


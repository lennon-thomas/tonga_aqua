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

eez<-readOGR(dsn = '/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/Suitability/raw/World_EEZ_v9_20161021',layer="eez",stringsAsFactors=FALSE)


#depth <- raster(paste0("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/Suitability/raw/rotated_topo30.tif"))

#eez<-crop(eez,depth)

eez_name<-"Tongan Exclusive Economic Zone"

tonga_eez<- eez[eez$GeoName %in% eez_name,]


ext<-c( -174.2542, -173.79, -18.95, -18.56295) #-18.15
    
vavau_eez<-crop(tonga_eez,ext)

writeOGR(vavau_eez, dsn=paste0(boxdir,"/data/tmp"),driver="ESRI Shapefile", layer="vavau_eez_shape",overwrite = TRUE)

#data from https://dusk.geo.orst.edu/tonga/mgr/
depth<-raster(paste0(boxdir,"/data/raw/map2mgr.grd"))

vav_depth<-crop(depth,ext)

vav_depth<-raster::mask(vav_depth,vavau_eez)

zero_value<-Which(vav_depth<0,cells=TRUE)

vav_depth[zero_value]<-0

writeRaster(vav_depth,paste0(boxdir,"/data/tmp/vav_depth.tif"),overwrite = TRUE)

# data from http://datadownload.unep-wcmc.org
coral<- readOGR(dsn = paste0(boxdir, "/data/raw/14_001_WCMC008_CoralReefs2010_v3/01_Data"),layer = "WCMC008_CoralReef2010_Py_v3")

tonga_coral<-crop(coral,ext)

tonga_coral_raster<-rasterize(tonga_coral,vav_depth,field=2,progress='text')

tonga_coral_raster[is.na(tonga_coral_raster)]<-0

tonga_coral_raster<-raster::mask(tonga_coral_raster,vav_depth,maskvalue=NA,inverse=FALSE,filename=paste(boxdir,"/data/tmp/tonga_coral_raster.tif",sep=""),overwrite=TRUE)


mangrove<-readOGR(dsn = paste0("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2016/Carib_aqua_16/Suitability/raw/mangroves/01_Data"),layer = "14_001_WCMC010_MangroveUSGS2011_v1_3")

tonga_mangrove<-crop(mangrove,ext)

writeOGR(tonga_mangrove,dsn=paste0(boxdir,"/data/tmp"),driver="ESRI Shapefile", layer="tonga_mangrove",overwrite = TRUE)

tonga_mangrove_raster<-rasterize(tonga_mangrove,vav_depth,field=2)

tonga_mangrove_raster[is.na(tonga_mangrove_raster)]<-0

tonga_mangrove_raster<-raster::mask(tonga_mangrove_raster,vav_depth,maskvalue=NA,inverse=FALSE,filename=paste(boxdir,"/data/tmp/tonga_mangrove_raster.tif",sep=""),overwrite=TRUE)


# Habitat data from  Millennium Coral Reef Mapping Project,
habitat<-readOGR(dsn = paste0(boxdir,"/data/raw/mill"),layer = "Tonga_v6")

vav_habitat<-crop(habitat,ext)

writeOGR(vav_habitat,dsn = paste0(boxdir,"/data/tmp"),driver="ESRI Shapefile",layer = "vav_habitat")

vav_habitat_df <- broom::tidy(vav_habitat, region = "L4_ATTRIB")


# Aquaculture areas -------------------------------------------------------

aqua_df <- read.csv(paste0(boxdir,"/data/vav_aqua_areas.csv")) %>%
  mutate(Lat1 = Lat*-1,
         Long1 = Long * -1) %>%
  select(Lat1,Long1,Area)

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
  filter(!as.character(unique(aqua_df$Area))==  "Utungake3")

row.names(names)<-c("Mounu Is","Oloua","Otea","Otumotu hahake Vv","Pangamotu Vv","Toku Is","Koko Bay Vv","Bay of Vv","Bay of Vv2",
                    "Matamaka", "Utungake1","Utungake2", "Aloitalau","Holeva","Feletoa", "Vaipua1","Vaipua2","Vaipua3","Tuanuku lakes")                

crs(all_aqua)<-repro

aqua_area_df<-SpatialPolygonsDataFrame(all_aqua,names)

writeOGR(aqua_area_df,dsn=paste0(boxdir,"/data/tmp"),driver = "ESRI Shapefile",layer="aqua_areas",overwrite=TRUE)




# add population data -----------------------------------------------------

district<-readOGR(paste0(boxdir,"/data/raw/ton_polbnda_adm2_district"), layer="ton_polbnda_adm2_district")
vav<-"Vava'u"

district<- district[district$ADM1_NAME %in% vav,]

district<-spTransform(district,repro)

village<-readOGR(paste0(boxdir,"/data/raw/ton_polbnda_adm3_village"), layer="ton_polbnda_adm3_village")

village<- village[village$ADM1_NAME %in% vav,]

village<-spTransform(village,repro)
  
pop_stats<-read.csv(paste0(boxdir,"/data/raw/ton_pplp_adm3_village.csv")) %>%
  filter(adm1_name == vav) %>%
  select(c(1:10)) %>%
  mutate(log_T_Pop = log(T_Pop+0.01))



village_df <- broom::tidy(village, region = "ADM3_NAME")

village_df<-merge(village_df,pop_stats,by.x="id",by.y="adm3_name")


getCollection("MYD11C1")



crs(test)<-repro
ggplot() + 
  geom_polygon(data = vav_habitat_df, aes(x = long, y = lat, group = group, fill = log_T_Pop), colour = "black") +
  scale_fill_discrete(name ="Habitat Type") +
  theme_void() +
  geom_polygon(data = all_aqua,aes(x=long,y=lat,group=group),fill="yellow")


ggplot() + 
  geom_polygon(data = village_df, aes(x = long, y = lat, group = group, fill = log_T_Pop), colour = "black") +
  scale_fill_continuous(name ="log(Population)") +
  theme_void() +
  geom_polygon(data = vavau_eez,aes(x=long,y=lat,group=group),fill=NA,col="black")





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


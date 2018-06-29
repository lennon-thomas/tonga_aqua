turblibrary(maptools)
library(ncdf4)
library(raster)
library(rgdal)
library(broom)

# set working directory
setwd("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/raw/pk")    # indicate the path to the files

# verify the existence of MODISA_sst.csv file
file.exists("MODISA_pk.csv") # caution! new data will be appended to this file if it already exists
# if TRUE choose an option
# file.rename("MODISA_sst.csv","MODISA_sst.org")
# file.remove("MODISA_sst.csv")

# list and remove objects from workspace
ls()
rm(list = ls())

# create a list of nc files and indicate its length
f <- list.files(".", pattern="*.Kd_490_4km.nc",full.names=F)
lf<-length(f)

# load shapefile
shp.area <- readOGR(paste0("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/tmp"), layer ="vavau_eez_shape") 

tonga_depth<-raster("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/tmp/vav_depth.tif")

#readShapePoly("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/tmp/eez_tonga_shape.shp") # indicate the name and location of the shapefile

# plot shapefile
plot(shp.area)

# get the spatial extent of the shapefile
ext.area <- extent(tonga_depth)

for (i in 1:lf) {
  # progress indicator
  print(sprintf("Processing file %i from %s: %s",i,length(f),f[i]))
  
  # open netCDF file
  nc.data<-nc_open(f[i])
  
  # extract date information from nc file
  dateini<-ncatt_get(nc.data,0,"time_coverage_start")$value
  dateend<-ncatt_get(nc.data,0,"time_coverage_end")$value
  datemean<-mean(c(as.Date(dateend,"%Y-%m-%dT%H:%M:%OSZ"),as.Date(dateini,"%Y-%m-%dT%H:%M:%OSZ")))
  year<-substring(datemean,0,4)
  month<-substring(datemean,6,7)
  
  # close netCDF file
  nc_close(nc.data)
  
  # create a raster from nc file
  rst.data <- raster(f[i],varname="Kd_490")
  # proj4string(rst.data)=CRS("+init=EPSG:4326")
  
  # crop the raster to area extent
  crp.data <- crop(rst.data,ext.area,snap = "out")
  
  # set values higher than 45 to NA
  #  crp.data[crp.data>=45]<-NA
  
  # create a dummy raster with NAs
  # crp.na <- setValues(crp.data,NA)
  
  # create a raster mask with the area boundaries
  #rst.mask <- rasterize(shp.area,crp.na)
  
  rst.mask <- rasterize(shp.area,tonga_depth)
  
  # resample sst file to 2 1km2
  res.data <- resample(crp.data,tonga_depth,method="bilinear")
  
  # apply the mask to the raster with data
  # msk.data <- mask(x=res.dat,mask=rst.mask)
  
  msk.data<-crop(res.data,ext.area)
  
  writeRaster(msk.data,paste0(year,"_",month,"_Kd_490.tif"),overwrite = TRUE)
  # get statistics
  sta.min <- cellStats(msk.data, stat='min',na.rm=TRUE)
  sta.mean <- cellStats(msk.data, stat='mean',na.rm=TRUE)
  sta.median <- median(na.omit(values(msk.data)))
  sta.max <- cellStats(msk.data, stat='max',na.rm=TRUE)
  
  # prepare final data set
  dat.output<-data.frame(year,month,sta.min,sta.mean,sta.median,sta.max)
  names(dat.output)<-c("year","month","Kd490_min","Kd490_mean","Kd490_median","Kd490_max")
  
  # save csv file
  fe<-file.exists("MODISA_Kd_490.csv")
  write.table(dat.output,"MODISA_Kd_490.csv",row.names=FALSE,col.names=!fe,sep=",",dec=".",append=fe) # change separator and decimal strings if necessary 
  
  # clean workspace
  rm(nc.data,dateini,dateend,datemean,year,month,rst.data,crp.data,crp.na,rst.mask,msk.data,sta.min,sta.mean,sta.median,sta.max,dat.output,fe)
}

rm(ext.area,i,lf)

kd<-read.csv("MODISA_Kd_490.csv")

kdfiles<-list.files(".", pattern="*_Kd_490.tif",full.names=F)

kdlist<-lapply(kdfiles,raster)

all_kd<-brick(kdlist)

names(all_kd)<-f


# Write netCDF files
writeRaster(all_sst,"2008_2017_monthly_Kd_490.NetCDF",format="CDF",overwrite=TRUE,varname="SST",varunit="m -1",zname="Time",zunit="month",NAflag=-9999)

#tonga_depth<-crop(tonga_depth,all_sst)
#all_sst<-mask(all_sst,tonga_depth)


min_kd<-calc(all_kd,function(x){min(x,na.rm = TRUE)},filename = "min_kd.tif",overwrite = TRUE)

max_kd<-calc(all_kd,function(x){max(x, na.rm = TRUE)}, filename = "max_kd.tif",overwrite = TRUE)

average_kd<-calc(all_kd,function(x){mean(x, na.rm = TRUE)}, filename = "average_kd.tif", overwrite = TRUE)


# Create land mask and data frames for plotting for plotting

tidy_eez<-tidy(shp.area)

temp_df<-data.frame(shp.area@data)

temp_df$id<-0

EEZ_df<-merge(tidy_eez,temp_df,by="id")

land<-EEZ_df %>%
  filter(hole==TRUE)

min_kd_df<-as_data_frame(rasterToPoints(min_kd)) 
max_kd_df<-as_data_frame(rasterToPoints(max_kd)) 
avg_kd_df<-as_data_frame(rasterToPoints(average_kd)) 

#Plot
ggplot() +
  geom_raster(data=min_kd_df,aes(x=x,y=y,fill = min_kd))+
  scale_fill_continuous("Kd_490 m^-1",low="yellow",high="darkred") +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle("Minimum Kd_490 from 2008-2017") +
  coord_fixed(1.03) 
ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/min_kd_490.png")


ggplot() +
  geom_raster(data=max_kd_df,aes(x=x,y=y,fill = max_kd)) +
  scale_fill_continuous("Kd_490 m^-1",low="yellow",high="darkred") +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  ggtitle("Maximum Kd_490 from 2008-2017") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed(1.03) 
ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/max_Kd_490.png")

ggplot() +
  geom_raster(data=avg_kd_df,aes(x=x,y=y,fill = average_kd))+
  #scale_fill_continuous("SST (C)",low="green",high="blue") +
  scale_fill_continuous("Pk_490 m^-1",low="yellow",high="darkred") +
  geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  ggtitle("Average Kd_490 from 2008-2017")  +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed(1.03) 
ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/average_Kd_490.png")


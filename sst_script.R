# Antonio Olinto Avila-da-Silva, Instituto de Pesca, Brasil
# https://gist.github.com/aolinto/3a0872e0fb61b7ca3e69d35d286ae26c
# script to process Aqua MODIS Sea Surface Temperature
# files downloaded from https://oceancolor.gsfc.nasa.gov/cgi/l3
# Aqua MODIS Sea Surface temperature 11 u daytime Monthly 9 km SMI images
# all .L3m_MO_SST_sst_9km.nc files must be in the working directory
# the script will open each nc file to read date information
# the script will also transform nc file to raster, read sst data
# for a given area, compute its statistics and write them into
# a single csv file named MODISA_sst.csv
# Some reference pages
# http://geog.uoregon.edu/GeogR/topics/netCDF-read-ncdf4.html
# https://scottishsnow.wordpress.com/2014/08/24/many-rastered-beast/
# version 2017/06/09

# load libraries
# ncdf4 needs libnetcdf-dev netcdf-bin in Linux
# install.packages(c("rgeos","maptools","ncdf4","raster"))
library(maptools)
library(ncdf4)
library(raster)
library(rgdal)
library(broom)

# set working directory
setwd("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/raw/sst")    # indicate the path to the files

# verify the existence of MODISA_sst.csv file
file.exists("MODISA_sst.csv") # caution! new data will be appended to this file if it already exists
# if TRUE choose an option
# file.rename("MODISA_sst.csv","MODISA_sst.org")
# file.remove("MODISA_sst.csv")

# list and remove objects from workspace
ls()
rm(list = ls())

# create a list of nc files and indicate its length
f <- list.files(".", pattern="*.L3m_MO_SST_sst_4km.nc",full.names=F)
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
  rst.data <- raster(f[i],varname="sst")
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
  res.data <- resample(crp.data,rst.mask,method="bilinear")
  
  # apply the mask to the raster with data
 # msk.data <- mask(x=res.dat,mask=rst.mask)
  
  msk.data<-crop(res.data,ext.area)
  
  writeRaster(msk.data,paste0(year,"_",month,"_sst.tif"),overwrite = TRUE)
  # get statistics
  sta.min <- cellStats(msk.data, stat='min',na.rm=TRUE)
  sta.mean <- cellStats(msk.data, stat='mean',na.rm=TRUE)
  sta.median <- median(na.omit(values(msk.data)))
  sta.max <- cellStats(msk.data, stat='max',na.rm=TRUE)
  
  # prepare final data set
  dat.output<-data.frame(year,month,sta.min,sta.mean,sta.median,sta.max)
  names(dat.output)<-c("year","month","SSTmin","SSTmean","SSTmedian","SSTmax")
  
  # save csv file
  fe<-file.exists("MODISA_sst.csv")
  write.table(dat.output,"MODISA_sst.csv",row.names=FALSE,col.names=!fe,sep=",",dec=".",append=fe) # change separator and decimal strings if necessary 
  
  # clean workspace
  rm(nc.data,dateini,dateend,datemean,year,month,rst.data,crp.data,crp.na,rst.mask,msk.data,sta.min,sta.mean,sta.median,sta.max,dat.output,fe)
}

rm(ext.area,i,lf)

sst<-read.csv("MODISA_sst.csv")

sstfiles<-list.files(".", pattern="*_sst.tif",full.names=F)

sstlist<-lapply(sstfiles,raster)

all_sst<-brick(sstlist)

names(all_sst)<-f


# Write netCDF files
writeRaster(all_sst,"2008_2017_monthly_SST.NetCDF",format="CDF",overwrite=TRUE,varname="SST",varunit="degrees C",zname="Time",zunit="month",NAflag=-9999)

#tonga_depth<-crop(tonga_depth,all_sst)
#all_sst<-mask(all_sst,tonga_depth)


min_sst<-calc(all_sst,function(x){min(x,na.rm = TRUE)},filename = "min_sst.tif",overwrite = TRUE)

max_sst<-calc(all_sst,function(x){max(x, na.rm = TRUE)}, filename = "max_sst.tif",overwrite = TRUE)

average_sst<-calc(all_sst,function(x){mean(x, na.rm = TRUE)}, filename = "average_sst.tif", overwrite = TRUE)


# Create land mask and data frames for plotting for plotting

tidy_eez<-tidy(shp.area)

temp_df<-data.frame(shp.area@data)

temp_df$id<-0

EEZ_df<-merge(tidy_eez,temp_df,by="id")

land<-EEZ_df %>%
  filter(hole==TRUE)

 min_sst_df<-as_data_frame(rasterToPoints(min_sst)) 
 max_sst_df<-as_data_frame(rasterToPoints(max_sst)) 
 avg_sst_df<-as_data_frame(rasterToPoints(average_sst)) 
 
 #Plot
 ggplot() +
   geom_raster(data=min_sst_df,aes(x=x,y=y,fill = min_sst))+
   scale_fill_continuous("SST (C)",low="darkblue",high="lightblue") +
   geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
   xlab("Longitude") +
   ylab("Latitude") +
   theme_bw() +
   scale_x_continuous(expand = c(0,0)) +
   scale_y_continuous(expand = c(0,0)) +
  ggtitle("Minimum SST from 2008-2017")
  ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/min_sst.png")
 
  
  ggplot() +
    geom_raster(data=max_sst_df,aes(x=x,y=y,fill = max_sst),title="Maximum SST from 2008-2017")+
    scale_fill_continuous("SST (C)",low="darkblue",high="lightblue") +
    geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    ggtitle("Maximum SST from 2008-2017") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))
  ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/max_sst.png")
  
  ggplot() +
    geom_raster(data=avg_sst_df,aes(x=x,y=y,fill = average_sst),title="Average SST from 2008-2017")+
    #scale_fill_continuous("SST (C)",low="green",high="blue") +
    scale_fill_continuous("SST (C)",low="darkblue",high="lightblue") +
    geom_polygon(data = land, aes(x=long, y=lat, group=group),fill =  "white", colour = "black", size = 0.8) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    ggtitle("Average SST from 2008-2017")  +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))
  ggsave("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/plots/average_sst.png")
  
  
  
  
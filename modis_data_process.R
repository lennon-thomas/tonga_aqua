# load libraries
# ncdf4 needs libnetcdf-dev netcdf-bin in Linux
# install.packages(c("ncdf4","reshape2"))
library("ncdf4")
library("reshape2")
library("sdmpredictors")
library("leaflet")
library(sdmpredictors) 



# set working directory
setwd("/Users/lennonthomas/Box Sync/Waitt Institute/Blue Halo 2018/Vavau/Aquaculture/data/raw/sst")   # indicate the path to the files
file.exists("MODISA_sst.csv")     # caution new data will be appended to this file if it already exists
 file.rename("MODISA_sst.csv","MODISA_sst.old")
 file.remove("MODISA_sst.csv")

# list and remove objects
ls()
rm(list = ls())

# set the study area
lonmax= -173.79
lonmin=-174.2542
latmax=-18.15
latmin=-18.95
#ext<-c( -174.2542, -173.79, -18.95, -18.15) # -18.56295
# create a list of files and indicate its length
f <- list.files(".", pattern="*.L3m_MO_SST4_sst4_4km.nc",full.names=F)

allsst<-lapply(f,brick)

tonga_sst<-crop(allsst[[1]],ext)
(lf<-length(f))

# variable
var<-"sst4" 

for (i in 1:lf) {
  # progress indicator
  print(paste("Processing file",i,"from",length(f),sep=" "))
  # open netCDF file
  data<-nc_open(f)
  # extract data
  lon<-ncvar_get(data,"lon")
  lat<-ncvar_get(data,"lat")
  value<-ncvar_get(data,var)
  unit<-ncatt_get(data,var,"units")$value
  # matrix to data.frame
  dimnames(value)<-list(lon=lon,lat=lat)
  dat.var<-melt(value,id="lon")
  # select data from the study area taking out missing data
  dat.varSAtmp<-subset(dat.var,lon<=lonmax & lon>=lonmin & lat<=latmax & lat>=latmin & value<45)
  # extract date information
  dateini<-ncatt_get(data,0,"time_coverage_start")$value
  dateend<-ncatt_get(data,0,"time_coverage_end")$value
  datemean<-mean(c(as.Date(dateend,"%Y-%m-%dT%H:%M:%OSZ"),as.Date(dateini,"%Y-%m-%dT%H:%M:%OSZ")))
  year<-substring(datemean,0,4)
  month<-substring(datemean,6,7)
  # prepare final data set
  dat.varSA<-data.frame(rep(as.integer(year,nrow(dat.varSAtmp))),rep(as.integer(month,nrow(dat.varSAtmp))),
                        dat.varSAtmp,rep(unit,nrow(dat.varSAtmp)),rep(var,nrow(dat.varSAtmp)))
  names(dat.varSA)<-c("year","month","lon","lat","value","unit","var")
  # save csv file
  fe<-file.exists("MODISA_sst.csv")
  write.table(dat.varSA,"MODISA_sst.csv",row.names=FALSE,col.names=!fe,sep=" ",dec=",",append=fe)
  # close connection
  nc_close(data)
  # clean workspace
  rm(data,lon,lat,value,unit,dat.var,dat.varSAtmp,dateini,dateend,datemean,year,month,dat.varSA,fe)
}
rm(var,f,i,latmax,latmin,lf,lonmax,lonmin)

sst<-read_csv("MODISA_sst.csv")

##Salinity

lyr<-list_layers("Bio-ORACLE")

salinity <- load_layers(c("BO2_salinitymax_ss", "BO2_salinitymean_ss", "BO2_salinitymin_ss","BO2_salinityrange_ss","BO2_salinityltmax_ss","BO2_salinityltmin_ss")) 

ext<-c( -174.2542, -173.79, -18.95, -18.15) # -18.56295

tonga_salinity<-crop(salinity,ext)

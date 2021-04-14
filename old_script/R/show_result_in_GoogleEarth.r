setwd("C:/Users/Huijie Qiao/git/ees_3d/R")
library(rgdal)
library(raster)
library(stringr)
library(dplyr)
library(RSQLite)
library(DBI)
library("ape")
library("phangorn")
library("phytools")
library("geiger")
library("stringr")
library(tidyverse)
library(plotKML)
data(eberg_contours)
## Not run: 
plotKML(shape_t)


map<-read.table("../../ees_3d_data/22816_GOOD_BROAD.log", head=F, sep=",")
colnames(map)<-c("YEAR", "ID", "group_id", "sp_id")

shape <- readOGR(dsn = "../../ees_3d_data", layer = "isea3h8p")
y=999
for (y in c(999:0)){
  
  item <- map[which(map$YEAR==y),]
  shape_t<-shape
  shape_t@coords<-subset(shape_t@coords, shape_t$global_id %in% item$ID)
  shape_t@data$global_id<-as.numeric(as.character(shape_t@data$global_id))
  shape_t@data<-subset(shape_t@data, shape_t$global_id %in% item$ID)
  shape_t@data<-inner_join(shape_t@data, item, by=c("global_id"="ID"))
  plot(shape_t, col=rainbow(length(unique(item$group_id))+1)[shape_t@data$group_id+1], cex=0.2)
  #plotKML(shape_t, colour="group_id", size="YEAR")
  print(table(item$group_id))
  writeOGR(shape_t, dsn = "../../ees_3d_data", 
           layer = as.character(y), driver="ESRI Shapefile", overwrite_layer=T)
  x<-readline(prompt=sprintf("Year is %d, Found %d groups. (X=exit): ", y, length(unique(item$group_id))))
  if (tolower(x)=="x"){
    break()
  }
}

j=2

library(rgdal)
library(raster)
library(stringr)
library(dplyr)
setwd("~/git/ees_3d/R")
folders<-c("Debiased_Maximum_Monthly_Precipitation",
           "Debiased_Maximum_Monthly_Temperature",
           "Debiased_Mean_Annual_Precipitation",
           "Debiased_Mean_Annual_Temperature",
           "Debiased_Minimum_Monthly_Temperature")


shape <- readOGR(dsn = "/home/huijieqiao/git/ees_3d_data/ISEA3H8/isea3hGen/outputfiles", layer = "isea3h8")
shape@data$v<-NA

args = commandArgs(trailingOnly=TRUE)
folders<-folders[as.numeric(j)]
tif_template<-"/home/huijieqiao/git/ees_3d_data/Raster/RAW/%s/TIF/%s.tif"
mask<-raster("/home/huijieqiao/git/ees_3d_data/Raster/mask.tif")
var<-folders[1]
is_na<-!is.na(values(mask))


if (F){
  aa<-data.frame()
  for (i in c(1:length(shape@polygons))){
    print(i)
    item<-data.frame(id=i, area=shape@polygons[[i]]@area)
    if (nrow(aa)==0){
      aa=item
    }else{
      aa<-bind_rows(aa, item)
    }
  }
  aa$area_int<-round(aa$area)
  tt<-data.frame(table(aa$area_int))
  hist(aa$area)
  for (area in unique(aa$area_int)){
    shape_t<-shape
    shape_t@polygons<-subset(shape_t@polygons, (aa$area_int==area))
    shape_t@data<-subset(shape_t@data, (aa$area_int==area))
    print(paste(area, length(shape_t@polygons)))
    
    writeOGR(shape_t, dsn = "/home/huijieqiao/git/ees_3d_data/ISEA3H8/Area_Split", 
             layer = sprintf("l_%d", area), driver="ESRI Shapefile", overwrite_layer=T)
  }
}

#for (var in folders){
  dsn<-sprintf("/home/huijieqiao/git/ees_3d_data/ISEA3H8/Maps/%s", var)
  if (!dir.exists(dsn)){
    dir.create(dsn)
  }
  csv<-sprintf("/home/huijieqiao/git/ees_3d_data/ISEA3H8/CSV/%s", var)
  if (!dir.exists(csv)){
    dir.create(csv)
  }
  y=0
  shape_list<-list()
  st<-NULL
  for (y in c(0:1200)){
    
    y_str<-str_pad(y, 4, pad = "0")
    shape_list[[y_str]]<-shape
    print(paste("Stacking", var, y))
    r<-raster(sprintf(tif_template, var, y_str))
    p<-as_tibble(rasterToPoints(r))
    if (is.null(st)){
      st<-p
    }else{
      v<-values(r)
      st[, sprintf("X%s", y_str)]<-values(r)[is_na]
    }
  }
  st$label<-paste(st$x, st$y)
  i=55939
  
  dist_fun <- function(arg1, arg2){
    col <- arg1 + arg2
    x <- as.data.frame(col)
  }
  
  
  for (i in c(1:length(shape@polygons))){
    p<-shape@polygons[[i]]@Polygons[[1]]
    ps = Polygons(list(p),1)
    sps = SpatialPolygons(list(ps))
    #plot(sps)
    print(paste("Cropping", i, length(shape@polygons), var))
    v<-NaN
    tryCatch(
      {
        r_c<-crop(mask, sps)
        v<-mean(values(r_c), na.rm=T)
      },
      error=function(cond) {
        next()
      }
    )
    
    
    if (is.nan(v)){
      next()
    }
    p<-as_tibble(rasterToPoints(r_c))
    p$label<-paste(p$x, p$y)
    print("Extracting data from stacked raster")
    v<-st %>% filter(label %in% p$label)
    
    v_mean<-colMeans(v[, c(3:(ncol(v)-1))], na.rm=T)
    print("putting to data.frame")
    for (y in c(0:1200)){
      y_str<-str_pad(y, 4, pad = "0")
      shape_list[[y_str]]@data[i, "v"]<-v_mean[y+1]
    }
  }
  
  #fix the error polygons
  point <- readOGR(dsn = "/home/huijieqiao/git/ees_3d_data/ISEA3H8/isea3hGen/outputfiles", layer = "isea3h8p")
  p_df<- tibble(global_id=point$global_id, x=point@coords[,1], y=point@coords[,2])
  
  for (i in c(1:length(shape@polygons))){
    print(i)
    if (shape@polygons[[i]]@area>50){
      for (y in c(0:1200)){
        y_str<-str_pad(y, 4, pad = "0")
        shape_list[[y_str]]@data[i, "v"]<-NA
      }
      p<-p_df[i,]
      v<-extract(mask, p[, c("x", "y")])
      if (is.na(v)){
        next()
      }
      
      #dist<-st %>% rowwise() %>% do(d = sqrt((.$x - p[1])^2 + (.$y-p[2])^2))
      st_t<-st
      st_t$dist<-apply(st[,c('x','y')], 1, function(x) sqrt((x[1] - p$x)^2 + (x[2]-p$y)^2) )
      st_t<-st_t[which(st_t$dist==min(st_t$dist)),]
      for (y in c(0:1200)){
        y_str<-str_pad(y, 4, pad = "0")
        shape_list[[y_str]]@data[i, "v"]<-st_t[, sprintf("X%s", y_str)]
      }
    }
  }
  
  for (y in c(0:1200)){
    y_str<-str_pad(y, 4, pad = "0")
    print(paste("Saving", var, y_str))
    shape_t<-shape_list[[y_str]]
    shape_t@polygons<-subset(shape_t@polygons, !is.na(shape_t@data$v))
    shape_t@data<-subset(shape_t@data, !is.na(shape_t@data$v))
    writeOGR(shape_t, dsn = dsn, 
             layer = y_str, driver="ESRI Shapefile", overwrite_layer=T)
    write.table(shape_t@data, sprintf("%s/%s.csv", csv, y_str), row.names = F, sep=" ", quote=F)
    
  }
#}


 
  if (F){
    library("geosphere")
    distMeeus(point@coords[20,], point@coords[21,])
    distMeeus(point@coords[21,], point@coords[22,])
    distMeeus(point@coords[21,], point@coords[23,])
    
    ids<-c(20, 21, 22, 23, 24,102,103,104,105,184,185,186,
           31104,31105,31185,31186,31266,31267,31348)
    shape_t<-shape
    shape_t@polygons<-subset(shape_t@polygons, shape_t$global_id %in% ids)
    shape_t@data<-subset(shape_t@data, shape_t$global_id %in% ids)
    writeOGR(shape_t, dsn = "/home/huijieqiao/git/ees_3d_data/ISEA3H8/test", 
             layer = "test", driver="ESRI Shapefile", overwrite_layer=T)
    
  }

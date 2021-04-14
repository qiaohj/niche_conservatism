library(raster)
setwd("~/git/ees_3d/R")
args = commandArgs(trailingOnly=TRUE)
group<-args[1]
if (is.na(group)){
  group<-"Mammals"
}
base<-sprintf("/home/huijieqiao/Experiments/IUCN_FIX/Raster/IUCN_Range_By_Species/%s", group)
files<-list.files(base)
f<-files[1]
max_prec<-raster("/home/huijieqiao/git/ees_3d_data/Raster/RAW/Debiased_Maximum_Monthly_Precipitation/TIF/0000.tif")
max_temp<-raster("/home/huijieqiao/git/ees_3d_data/Raster/RAW/Debiased_Maximum_Monthly_Temperature/TIF/0000.tif")
min_temp<-raster("/home/huijieqiao/git/ees_3d_data/Raster/RAW/Debiased_Minimum_Monthly_Temperature/TIF/0000.tif")
annual_temp<-raster("/home/huijieqiao/git/ees_3d_data/Raster/RAW/Debiased_Mean_Annual_Temperature/TIF/0000.tif")
annual_prec<-raster("/home/huijieqiao/git/ees_3d_data/Raster/RAW/Debiased_Mean_Annual_Precipitation/TIF/0000.tif")
dir.create(sprintf("../../ees_3d_data/SMART_SPECIES/IUCN/Raster/%s", group), showWarnings = F)
dir.create(sprintf("../../ees_3d_data/SMART_SPECIES/IUCN/RDA/%s", group), showWarnings = F)

for (f in files){
  
  target<-sprintf("../../ees_3d_data/SMART_SPECIES/IUCN/Raster/%s/%s", group, f)
  if (file.exists(target)){
    next()
  }
  if (!endsWith(f, "tif")){
    next()
  }
  print(f)
  r<-raster(sprintf("%s/%s", base, f))
  r<-resample(r, max_prec, method="ngb")
  p<-data.frame(rasterToPoints(r))
  p$max_prec<-extract(max_prec, p[, c("x", "y")])
  p$max_temp<-extract(max_temp, p[, c("x", "y")])
  p$min_temp<-extract(min_temp, p[, c("x", "y")])
  p$annual_temp<-extract(annual_temp, p[, c("x", "y")])
  p$annual_prec<-extract(annual_prec, p[, c("x", "y")])
  p<-p[complete.cases(p),]
  writeRaster(r, target, overwrite=T)
  saveRDS(p, file=sprintf("../../ees_3d_data/SMART_SPECIES/IUCN/RDA/%s/%s", group, gsub("\\.tif", "\\.rda", f)))
}

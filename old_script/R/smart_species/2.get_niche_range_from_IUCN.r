library(raster)
setwd("~/git/ees_3d/R")
args = commandArgs(trailingOnly=TRUE)
group<-args[1]
if (is.na(group)){
  group<-"Amphibians"
}
base<-sprintf("../../ees_3d_data/SMART_SPECIES/IUCN/RDA/%s", group)
files<-list.files(base)
f<-files[1]

vars<-c("max_prec", "max_temp", "min_temp", "annual_temp", "annual_prec")
result<-data.frame()
for (f in files){
  
  rda<-sprintf("%s/%s", base, f)

  if (!endsWith(f, "rda")){
    next()
  }
  print(f)
  
  df<-readRDS(rda)
  if (nrow(df)==0){
    next()
  }
  item<-data.frame(name=gsub("\\.rda", "", f), area=nrow(df))
  var<-vars[1]
  for (var in vars){
    outliersValue<-boxplot.stats(df[, var])$out
    v<-df[, var][!df[, var] %in% outliersValue]
    item[, sprintf("max_%s", var)]<-max(v)
    item[, sprintf("min_%s", var)]<-min(v)
    item[, sprintf("mean_%s", var)]<-mean(v)
  }  
  if (nrow(result)==0){
    result<-item
  }else{
    result<-rbind(result, item)
  }
}
saveRDS(result, sprintf("../../ees_3d_data/SMART_SPECIES/IUCN/NicheBreadth/%s.rda", group))

if (F){
  quantile(result$area, seq(0, 1, by=0.1))
  hist(result$area)
  result<-result[which(result$area>=100),]
  dim(result)
  quantile(result$max_max_prec-result$min_max_prec, seq(0, 1, by=0.1))
  quantile(result$max_max_temp-result$min_min_temp, seq(0, 1, by=0.1))
  result$prec_range<-result$max_max_prec-result$min_max_prec
  result$temp_range<-result$max_max_temp-result$min_min_temp
  plot(result$mean_max_prec, result$prec_range)
  plot(result$mean_annual_temp, result$temp_range)
  #Amphibians, prec:5.6 temp:23.7
}
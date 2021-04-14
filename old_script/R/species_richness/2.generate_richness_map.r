library(dplyr)
library(rgdal)
library(raster)
library(stringr)

setwd("~/git/ees_3d/R/species_richness")
args = commandArgs(trailingOnly=TRUE)
nb<-args[1]
da<-args[2]

if (is.na(nb)){
  nb<-"NARROW"
} 
if (is.na(da)){
  da<-"POOR"
}

shape <- readOGR(dsn = "/home/huijieqiao/git/ees_3d_data/ISEA3H8/isea3hGen/outputfiles", layer = "isea3h8p")
colfunc <- colorRampPalette(c("blue", "red"))
worldmap<-readOGR(dsn = "/home/huijieqiao/git/ees_3d_data/Shape/continents", layer = "continent")

source<-"/home/huijieqiao/git/ees_3d_data/TEST/Results"
target<-sprintf("%s/../Figures/Richness/%s_%s", source, nb, da)
dir.create(target, showWarnings=F)
print("loading data")
df<-readRDS(sprintf("%s/../Tables/%s.%s.rda", source, nb, da))
y<-names(df)[1199]

#if (F){
  max_col<-c()
  print("calculating the max richness")
  for (y_i in c(1:length(df))){
    y<-names(df)[y_i]
    v<- max((df[[y]]%>%count(ID))$n)
    max_col<-c(max_col,v )
    print(paste(y, v))
  }
  max_col<-max(max_col)
#}

dir.create(sprintf("%s/../Tables/%s.%s", source, nb, da), showWarnings = F)
for (y_i in c(1:length(df))){
  y<-names(df)[y_i]
  print(y)
  richness_df<-df[[y]]%>%count(ID)
  png(filename = sprintf("%s/%s.png", target, str_pad(y_i, 4, pad="0")),
      width = 1600, height = 900, units = "px")
  plot(worldmap, border="grey", main=(as.numeric(y)*-1))
  shape_t<-shape
  shape_t@coords<-subset(shape_t@coords, shape_t$global_id %in% richness_df$ID)
  shape_t@data<-subset(shape_t@data, shape_t$global_id %in% richness_df$ID)
  shape_t@data$global_id<-as.numeric(as.character(shape_t@data$global_id))
  shape_t@data<-inner_join(shape_t@data, richness_df, by=c("global_id"="ID"))
  saveRDS(shape_t, sprintf("%s/../Tables/%s.%s/%s.rda", source, nb, da, str_pad(y_i, 4, pad="0")))
  shape_t@data$n[which(shape_t@data$n>200)]<-200
  plot(shape_t, col=colfunc(200)[shape_t@data$n], cex=0.65, pch=15, add=T)
  #plot(shape_t, col=colfunc(max(shape_t@data$n))[shape_t@data$n], cex=0.65, pch=15, add=T)
  #plot(shape_t, col=shape_t@data$n, cex=0.65, pch=15, add=T)
  
  dev.off()
}
plot(shape_t)

cmd<-"ffmpeg -framerate 10 -pattern_type glob -i '%s/*.png' -c:v libx264 -pix_fmt yuv420p %s/../%s_%s.mp4"
print(sprintf(cmd, target, target, nb, da))
y_i=length(df)


       
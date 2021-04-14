
library("dplyr")
library("DBI")
library("phytools")
library("tidyr")
library("data.table")
library("tibble")
library("raster")
library("sp")
library("ggplot2")
library("sgeostat")



base<-"/media/huijieqiao/Speciation_Extin/Speciation_Extinction"

rdas<-list.files(sprintf("%s/Data/item_richness", base), pattern="\\.rda", full.names = T)

mask<-readRDS(sprintf("%s/Data/ENV/mask_df.rda", base))
mask<-mask%>%dplyr::distinct(global_id, lon, lat)
colnames(mask)<-toupper(colnames(mask))

i=18
bind<-function(df1, df2){
  if (is.null(df1)){
    df1<-df2
  }else{
    df1<-bind_rows(df1, df2)
  }
  return(df1)
}



for (i in c(43:length(rdas))){
  print(paste(i, length(rdas), rdas[i]))
  target<-sprintf("%s/Data/item_richness/df_N_SP_%d.rda", base, i)
  df<-readRDS(target)
  if (is.na(df)){
    next()
  }
  colnames(df)<-toupper(colnames(df))
  if (i!=1){
    df<-left_join(df, mask, by=c("LON", "LAT"))
  }
  saveRDS(df, target)
}

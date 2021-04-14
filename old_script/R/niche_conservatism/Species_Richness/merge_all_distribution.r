

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



base<-"/home/huijieqiao/git/ees_3d_data/niche_conservatism"
db_base<-"/home/huijieqiao/git/ees_3d_data/SMART_SPECIES"
base2<-"/media/huijieqiao/Butterfly/SMART_SPECIES"

rdas<-list.files(sprintf("%s/Data/items_distribution", base), pattern="\\.rda", full.names = T)

distribution<-NULL

i=1

bind<-function(df1, df2){
  if (is.null(df1)){
    df1<-df2
  }else{
    df1<-bind_rows(df1, df2)
  }
  return(df1)
}
for (i in c(1:length(rdas))){
  print(paste(i, length(rdas), rdas[i]))
  f<-rdas[i]
  df<-readRDS(f)
  distribution<-bind(distribution, df)
  print(nrow(distribution))
}



distribution[which(distribution$evo_ratio==0.01), "evo_ratio"]<-0.005

saveRDS(distribution, sprintf("%s/Data/distribution.rda", base))

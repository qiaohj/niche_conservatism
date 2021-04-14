

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

rdas<-list.files(sprintf("%s/Data/items_with_NA", base), pattern="\\.rda", full.names = T)

stat<-NULL
detail<-NULL
speciation_df<-NULL
extinction_df<-NULL
sp_character<-NULL
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
  if (grepl("stat_", f)){
    stat<-bind(stat, df)
  }
  if (grepl("detail_", f)){
    detail<-bind(detail, df)
  }
  if (grepl("speciation_df_", f)){
    speciation_df<-bind(speciation_df, df)
  }
  if (grepl("extinction_df_", f)){
    extinction_df<-bind(extinction_df, df)
  }
  if (grepl("sp_character_", f)){
    sp_character<-bind(sp_character, df)
  }
}



if (F){
  stat<-readRDS(sprintf("%s/Data/stat.rda", base))
  detail<-readRDS(sprintf("%s/Data/detail.rda", base))
  sp_character<-readRDS(sprintf("%s/Data/sp_character.rda", base))
  extinction_df<-readRDS(sprintf("%s/Data/extinction_df.rda", base))
  speciation_df<-readRDS(sprintf("%s/Data/speciation_df.rda", base))
}


stat[which(stat$evo_ratio==0.01), "evo_ratio"]<-0.005
stat[which(stat$evo_ratio==0.1), "evo_ratio"]<-0.05
colnames(stat)<-toupper(colnames(stat))
colnames(stat)[1]<-"SEED_ID"
colnames(stat)[6]<-"START_POINT"
colnames(detail)[13]<-"START_POINT"
detail[which(detail$EVO_RATIO==0.01), "EVO_RATIO"]<-0.005
detail[which(detail$EVO_RATIO==0.1), "EVO_RATIO"]<-0.05
sp_character[which(sp_character$EVO_RATIO==0.01), "EVO_RATIO"]<-0.005
sp_character[which(sp_character$EVO_RATIO==0.1), "EVO_RATIO"]<-0.05
sp_character<-sp_character%>%dplyr::ungroup()
extinction_df[which(extinction_df$EVO_RATIO==0.01), "EVO_RATIO"]<-0.005
extinction_df[which(extinction_df$EVO_RATIO==0.1), "EVO_RATIO"]<-0.05
speciation_df[which(speciation_df$EVO_RATIO==0.01), "EVO_RATIO"]<-0.005
speciation_df[which(speciation_df$EVO_RATIO==0.1), "EVO_RATIO"]<-0.05

saveRDS(stat, sprintf("%s/Data/stat.rda", base))
saveRDS(detail, sprintf("%s/Data/detail.rda", base))
saveRDS(sp_character, sprintf("%s/Data/sp_character.rda", base))
saveRDS(extinction_df, sprintf("%s/Data/extinction_df.rda", base))
saveRDS(speciation_df, sprintf("%s/Data/speciation_df.rda", base))



if (F){
  cc<-c()
  for (f in seq(from=11001, to=12000, by=100)){
    cc<-c(cc, sprintf("Rscript data.r %d %d", f, f+99))
  }
}

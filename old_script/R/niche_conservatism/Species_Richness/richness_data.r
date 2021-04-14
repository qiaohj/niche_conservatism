

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

i=17
bind<-function(df1, df2){
  if (is.null(df1)){
    df1<-df2
  }else{
    df1<-bind_rows(df1, df2)
  }
  return(df1)
}

#df_N_SP_all<-NULL
for (i in c(1:length(rdas))){
  print(paste(i, length(rdas), rdas[i]))
  target<-sprintf("%s/Data/item_richness/df_N_SP_%d.rda", base, i)
  if (file.exists(target)){
    next()
  }
  saveRDS(NA, target)
  f<-rdas[i]
  df<-readRDS(f)
  if (is.null(df)){
    next()
  }
  colnames(df)
  if (F){
    #i=16
    sp_list<-unique(df$SP_ID)
    n_sp<-length(sp_list)
    n_sp_2<-n_sp/2
    df_sub1<-df %>%dplyr::filter(SP_ID %in% sp_list[1:n_sp_2])
    df_N_SPsub_1<-df_sub1%>%
      dplyr::group_by(Y, GLOBAL_ID, lon, lat, MIN_TEMP, MAX_TEMP, MAX_PREC, SEED_ID, DA, NB, EVO_TYPE, EVO_RATIO, START_POINT)%>%
      dplyr::summarise(N_SP=n())
    
    df_sub2<-df %>%dplyr::filter(SP_ID %in% sp_list[(n_sp_2+1):n_sp])
    df_N_SPsub_2<-df_sub2%>%
      dplyr::group_by(Y, GLOBAL_ID, lon, lat, MIN_TEMP, MAX_TEMP, MAX_PREC, SEED_ID, DA, NB, EVO_TYPE, EVO_RATIO, START_POINT)%>%
      dplyr::summarise(N_SP=n())
    df_N_SP<-bind_rows(df_N_SPsub_1, df_N_SPsub_2)
    print(paste(nrow(df_sub1), nrow(df_sub2), nrow(df)))
  }
  
  df_N_SP<-df%>%
    dplyr::group_by(Y, ID, lon,lat, MIN_TEMP, MAX_TEMP, MAX_PREC, 
                    global_id, da, nb, evo_type, evo_ratio)%>%
    dplyr::summarise(N_SP=n())
  if (nrow(df_N_SP)>0){
    colnames(df_N_SP)<-c("Y", "GLOBAL_ID", "LON", "LAT", 
                       "MIN_TEMP", "MAX_TEMP", "MAX_PREC", 
                       "SEED_ID", "DA", "NB", "EVO_TYPE", "EVO_RATIO", "N_SP")
  }
  saveRDS(df_N_SP, target)
  #df_N_SP_all<-bind(df_N_SP_all, df_N_SP)
  #22, 17
}


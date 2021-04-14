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

seeds_rep<-readRDS(sprintf("%s/Data/seeds_rep.rda", base))

rdas<-list.files(sprintf("%s/Data/items_distribution", base), pattern="\\.rda", full.names = T)

distribution<-NULL

i=100
bind<-function(df1, df2){
  if (is.null(df1)){
    df1<-df2
  }else{
    df1<-bind_rows(df1, df2)
  }
  return(df1)
}

i=75
for (i in c(1:length(rdas))){
  print(paste(i, length(rdas), rdas[i]))
  target<-sprintf("%s/Data/item_richness_rep/df_richness_%d.rda", base, i)
  if (file.exists(target)){
    next()
  }
  saveRDS(NULL, target)
  source<-sprintf("%s/Data/item_richness/df_N_SP_%d.rda", base, i)
  df_N_SP<-readRDS(source)
  nrow<-nrow(df_N_SP)
  if (is.null(nrow)){
    nrow<-0
  }
  if (nrow==0){
    next()
  }
  df_richness<-NULL
  rep=1
  for (rep in unique(seeds_rep$REP)){
    print(paste(i, length(rdas), rdas[i], "REP:", rep))
    seeds<-seeds_rep[which(seeds_rep$REP==rep),]
    item<-df_N_SP%>%dplyr::filter(SEED_ID %in% seeds$GLOBAL_ID)
    if (nrow(item)==0){
      next()
    }
    item_se<-item%>%dplyr::ungroup()%>%
      dplyr::group_by(Y, GLOBAL_ID, LON, LAT, MIN_TEMP, MAX_TEMP, MAX_PREC, DA, NB, EVO_TYPE, EVO_RATIO)%>%
      dplyr::summarise(N_SP=sum(N_SP))%>%dplyr::ungroup()
    item_se$REP<-rep
    df_richness<-bind(df_richness, item_se)
  }
  saveRDS(df_richness, target)
}


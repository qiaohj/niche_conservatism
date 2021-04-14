library("dplyr")
library("ggplot2")
base<-"/home/huijieqiao/git/ees_3d_data/niche_conservatism"


year=100
rep=100

i=35
speciation_by_lat<-NULL
extinction_by_lat<-NULL
for (i in c(1:rep)){ 
  print(i)
  sub_speciation_df<-readRDS(sprintf("%s/Data/items_rep_lat/speciation_df_rep_lat_%d.rda", base, i))
  sub_speciation_df$LAT_ROUND<-round(sub_speciation_df$LAT)
  #sub_speciation_df%>%dplyr::group_by(GLOBAL_ID)%>%dplyr::summarise(N=n_distinct(DA,NB,EVO_TYPE,EVO_RATIO,WARP_LABEL))
  item_natural_barrier<-sub_speciation_df%>%dplyr::filter(is.na(MIN_TEMP_IN))%>%
    dplyr::group_by(LAT_ROUND, NB, DA, EVO_TYPE, EVO_RATIO, REP)%>%
    dplyr::summarise(N=n_distinct(SEED_ID, Y))
  item_natural_barrier$ENV<-"NATURAL"
  
  item_min_temp<-sub_speciation_df%>%dplyr::filter(!MIN_TEMP_IN)%>%
    dplyr::group_by(LAT_ROUND, NB, DA, EVO_TYPE, EVO_RATIO, REP)%>%
    dplyr::summarise(N=n_distinct(SEED_ID, Y))
  item_min_temp$ENV<-"MIN_TEMP"

  item_max_temp<-sub_speciation_df%>%dplyr::filter(!MAX_TEMP_IN)%>%
    dplyr::group_by(LAT_ROUND, NB, DA, EVO_TYPE, EVO_RATIO, REP)%>%
    dplyr::summarise(N=n_distinct(SEED_ID, Y))
  item_max_temp$ENV<-"MAX_TEMP"
  
  item_max_prec<-sub_speciation_df%>%dplyr::filter(!MAX_PREC_IN)%>%
    dplyr::group_by(LAT_ROUND, NB, DA, EVO_TYPE, EVO_RATIO, REP)%>%
    dplyr::summarise(N=n_distinct(SEED_ID, Y))
  item_max_prec$ENV<-"MAX_PREC"
  
  item<-bind_rows(item_min_temp, item_max_temp)
  item<-bind_rows(item, item_max_prec)
  item<-bind_rows(item, item_natural_barrier)
  if (is.null(speciation_by_lat)){
    speciation_by_lat<-item
  }else{
    speciation_by_lat<-bind_rows(speciation_by_lat, item)
  }
  if (F){
    ggplot(item, aes(x=LAT_ROUND, y=N, color=factor(ENV)))+
      #geom_point()+
      geom_line()+
      theme_bw()+
      facet_wrap( ~ EVO_TYPE+EVO_RATIO+DA+NB, ncol=4, scales = 'free')
  }
  
  sub_extinction_df<-readRDS(sprintf("%s/Data/items_rep_lat/extinction_df_rep_lat_%d.rda", base, i))
  sub_extinction_df$LAT_ROUND<-round(sub_extinction_df$LAT)
  item_min_temp<-sub_extinction_df%>%dplyr::filter(!MIN_TEMP_IN)%>%
    dplyr::group_by(LAT_ROUND, NB, DA, EVO_TYPE, EVO_RATIO, REP)%>%
    dplyr::summarise(N=n_distinct(SEED_ID, Y))
  item_min_temp$ENV<-"MIN_TEMP"
  
  item_max_temp<-sub_extinction_df%>%dplyr::filter(!MAX_TEMP_IN)%>%
    dplyr::group_by(LAT_ROUND, NB, DA, EVO_TYPE, EVO_RATIO, REP)%>%
    dplyr::summarise(N=n_distinct(SEED_ID, Y))
  item_max_temp$ENV<-"MAX_TEMP"
  
  item_max_prec<-sub_extinction_df%>%dplyr::filter(!MAX_PREC_IN)%>%
    dplyr::group_by(LAT_ROUND, NB, DA, EVO_TYPE, EVO_RATIO, REP)%>%
    dplyr::summarise(N=n_distinct(SEED_ID, Y))
  item_max_prec$ENV<-"MAX_PREC"
  
  item<-bind_rows(item_min_temp, item_max_temp)
  item<-bind_rows(item, item_max_prec)
  if (is.null(extinction_by_lat)){
    extinction_by_lat<-item
  }else{
    extinction_by_lat<-bind_rows(extinction_by_lat, item)
  }
}

saveRDS(speciation_by_lat, sprintf("%s/Data/speciation_by_lat.rda", base))
saveRDS(extinction_by_lat, sprintf("%s/Data/extinction_by_lat.rda", base))

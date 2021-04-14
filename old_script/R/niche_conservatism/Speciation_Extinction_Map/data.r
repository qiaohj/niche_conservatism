library("dplyr")
library("ggplot2")
base<-"/home/huijieqiao/git/ees_3d_data/niche_conservatism"


year=100
rep=100

i=50
speciation_map<-NULL
extinction_map<-NULL
for (i in c(1:rep)){ 
  print(i)
  sub_detail_df<-readRDS(sprintf("%s/Data/items_rep/speciation_df_rep_%d.rda", base, i))
  sub_detail_df$LAT_ROUND<-round(sub_detail_df$LAT)
  sub_detail_df$LON_ROUND<-round(sub_detail_df$LON)
  
  item_min_temp<-sub_detail_df%>%dplyr::filter(!MIN_TEMP_IN)%>%
    dplyr::group_by(LAT_ROUND, LON_ROUND, GLOBAL_ID, EVO_TYPE, EVO_RATIO, REP)%>%
    dplyr::summarise(N=n_distinct(Y))
  item_min_temp$ENV<-"MIN_TEMP"

  item_max_temp<-sub_detail_df%>%dplyr::filter(!MAX_TEMP_IN)%>%
    dplyr::group_by(LAT_ROUND, LON_ROUND, GLOBAL_ID, EVO_TYPE, EVO_RATIO, REP)%>%
    dplyr::summarise(N=n_distinct(Y))
  item_max_temp$ENV<-"MAX_TEMP"
  
  item_max_prec<-sub_detail_df%>%dplyr::filter(!MAX_PREC_IN)%>%
    dplyr::group_by(LAT_ROUND, LON_ROUND, GLOBAL_ID, EVO_TYPE, EVO_RATIO, REP)%>%
    dplyr::summarise(N=n_distinct(Y))
  item_max_prec$ENV<-"MAX_PREC"
  
  item_all<-sub_detail_df%>%
    dplyr::group_by(LAT_ROUND, LON_ROUND, GLOBAL_ID, EVO_TYPE, EVO_RATIO, REP)%>%
    dplyr::summarise(N=n_distinct(Y))
  item_all$ENV<-"ALL"
  
  item<-bind_rows(item_min_temp, item_max_temp)
  item<-bind_rows(item, item_max_prec)
  item<-bind_rows(item, item_all)
  if (is.null(speciation_map)){
    speciation_map<-item
  }else{
    speciation_map<-bind_rows(speciation_map, item)
  }
  if (F){
    ggplot(item_all, aes(x=LON_ROUND, y=LAT_ROUND, color=N))+
      geom_point()+
      #geom_line()+
      theme_bw()+
      facet_wrap( ~ EVO_TYPE+EVO_RATIO, ncol=2, scales = 'free')
  }
  
  sub_detail_df<-readRDS(sprintf("%s/Data/items_rep/extinction_df_rep_%d.rda", base, i))
  sub_detail_df$LAT_ROUND<-round(sub_detail_df$LAT)
  sub_detail_df$LON_ROUND<-round(sub_detail_df$LON)
  item_min_temp<-sub_detail_df%>%dplyr::filter(!MIN_TEMP_IN)%>%
    dplyr::group_by(LAT_ROUND, LON_ROUND, GLOBAL_ID, EVO_TYPE, EVO_RATIO, REP)%>%
    dplyr::summarise(N=n_distinct(Y))
  item_min_temp$ENV<-"MIN_TEMP"
  
  item_max_temp<-sub_detail_df%>%dplyr::filter(!MAX_TEMP_IN)%>%
    dplyr::group_by(LAT_ROUND, LON_ROUND, GLOBAL_ID, EVO_TYPE, EVO_RATIO, REP)%>%
    dplyr::summarise(N=n_distinct(Y))
  item_max_temp$ENV<-"MAX_TEMP"
  
  item_max_prec<-sub_detail_df%>%dplyr::filter(!MAX_PREC_IN)%>%
    dplyr::group_by(LAT_ROUND, LON_ROUND, GLOBAL_ID, EVO_TYPE, EVO_RATIO, REP)%>%
    dplyr::summarise(N=n_distinct(Y))
  item_max_prec$ENV<-"MAX_PREC"
  
  item_all<-sub_detail_df%>%
    dplyr::group_by(LAT_ROUND, LON_ROUND, GLOBAL_ID, EVO_TYPE, EVO_RATIO, REP)%>%
    dplyr::summarise(N=n_distinct(Y))
  item_all$ENV<-"ALL"
  
  item<-bind_rows(item_min_temp, item_max_temp)
  item<-bind_rows(item, item_max_prec)
  item<-bind_rows(item, item_all)
  
  if (is.null(extinction_map)){
    extinction_map<-item
  }else{
    extinction_map<-bind_rows(extinction_map, item)
  }
}

saveRDS(speciation_map, sprintf("%s/Data/speciation_map.rda", base))
saveRDS(extinction_map, sprintf("%s/Data/extinction_map.rda", base))

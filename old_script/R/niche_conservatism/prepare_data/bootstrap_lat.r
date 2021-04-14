library("dplyr")
library("Hmisc")
base<-"/home/huijieqiao/git/ees_3d_data/niche_conservatism"

fix_type<-function(x){
  x[which(x==1)]<-"Lazy"
  x[which(x==2)]<-"Darwin I"
  x[which(x==3)]<-"AI Lamarck"
  x[which(x==4)]<-"AI"
  x[which(x==5)]<-"Lamarck II"
  x[which(x==6)]<-"Darwin xII"
  x[which(x==7)]<-"Darwin II"
  x[which(x==8)]<-"Combined"
  x[which(x==9)]<-"Lamarck I"
  x
}

fix_df<-function(p_df){
  colnames(p_df)<-toupper(colnames(p_df))
  p_df<-p_df%>%dplyr::filter(EVO_TYPE %in% c(1,2,4,5,7,8,9))
  p_df$EVO_TYPE<-fix_type(p_df$EVO_TYPE)
  
  p_df_0<-p_df%>%dplyr::filter(EVO_RATIO==1)
  p_df_1<-p_df%>%dplyr::filter(EVO_RATIO!=1)
  p_df_0$EVO_RATIO<-0.05
  p_df<-bind_rows(p_df_1, p_df_0)
  p_df_0$EVO_RATIO<-0.005
  p_df<-bind_rows(p_df, p_df_0)
  
  p_df[which(p_df$NB=="MODERATE"), "NB"]<-"BROAD"
  p_df$WARP_LABEL<-paste(p_df$NB, p_df$DA, p_df$EVO_RATIO)
  p_df
}
stat<-readRDS(sprintf("%s/Data/stat.rda", base))
stat<-fix_df(stat)

sample_size<-10
rep<-100
if (F){
  
  mask_df<-readRDS(sprintf("%s/Data/ENV/mask_df.rda", base))
  stat<-inner_join(stat, mask_df%>%dplyr::filter(Y==0), by=c("GLOBAL_ID"="global_id"))
  cuts<-seq(from=-92.5, to=92.5, by=5)
  
  seeds<-unique(stat[, c("GLOBAL_ID", "lon", "lat")])
  seeds$cuts<-cut2(seeds$lat, cuts = cuts)
  
  seeds_rep<-NULL
  for (i in c(1:rep)){
    print(i)
    seeds_df <- seeds %>% dplyr::group_by(cuts) %>% dplyr::sample_n(10, replace=T)
    seeds_df$REP<-i
    if (is.null(seeds_rep)){
      seeds_rep<-seeds_df
    }else{
      seeds_rep<-bind_rows(seeds_rep, seeds_df)
    }
  }
  saveRDS(seeds_rep, sprintf("%s/Data/seeds_rep_lat.rda", base))
}
seeds_rep<-readRDS(sprintf("%s/Data/seeds_rep_lat.rda", base))


detail<-readRDS(sprintf("%s/Data/detail.rda", base))
detail<-fix_df(detail)
speciation_df<-readRDS(sprintf("%s/Data/speciation_df.rda", base))
speciation_df<-fix_df(speciation_df)
extinction_df<-readRDS(sprintf("%s/Data/extinction_df.rda", base))
extinction_df<-fix_df(extinction_df)
colnames(extinction_df)
sp_character<-readRDS(sprintf("%s/Data/sp_character.rda", base))
sp_character<-fix_df(sp_character)


i=1

for (i in c(1:rep)){
  if (file.exists(sprintf("%s/Data/items_rep_lat/stat_df_rep_lat_%d.rda", base, i))){
    next()
  }
  saveRDS(NA, sprintf("%s/Data/items_rep_lat/stat_df_rep_lat_%d.rda", base, i))
  
  stat_df_rep<-NULL
  detail_df_rep<-NULL
  speciation_df_rep<-NULL
  extinction_df_rep<-NULL
  sp_character_rep<-NULL
  print(i)
  sub_seeds<-seeds_rep%>%dplyr::filter(REP==i)
  sub_stat_df<-stat%>%dplyr::filter(SEED_ID %in% sub_seeds$GLOBAL_ID)
  sub_stat_df$REP<-i
  sub_detail_df<-detail%>%dplyr::filter(SEED_ID %in% sub_seeds$GLOBAL_ID)
  sub_detail_df$REP<-i
  sub_speciation_df<-speciation_df%>%dplyr::filter(SEED_ID %in% sub_seeds$GLOBAL_ID)
  sub_speciation_df$REP<-i
  sub_extinction_df<-extinction_df%>%dplyr::filter(SEED_ID %in% sub_seeds$GLOBAL_ID)
  sub_extinction_df$REP<-i
  sub_sp_character<-sp_character%>%dplyr::filter(SEED_ID %in% sub_seeds$GLOBAL_ID)
  sub_sp_character$REP<-i
  
  seed_N<-data.frame(table(sub_seeds$GLOBAL_ID))
  seed_N<-seed_N[which(seed_N$Freq>1),]
  j=1
  sub_stat_df_2<-sub_stat_df
  sub_detail_df_2<-sub_detail_df
  sub_speciation_df_2<-sub_speciation_df
  sub_extinction_df_2<-sub_extinction_df
  sub_sp_character_2<-sub_sp_character
  for (j in c(1:nrow(seed_N))){
    print(paste(i, j, nrow(seed_N)))
    seed_item<-seed_N[j,]
    
    sub_stat_df_item<-sub_stat_df%>%dplyr::filter(SEED_ID %in% seed_item$Var1)
    sub_stat_df_item<-bind_rows(replicate(seed_item$Freq-1, sub_stat_df_item, simplify = FALSE))
    sub_stat_df_2<-bind_rows(sub_stat_df_2, sub_stat_df_item)
    
    sub_detail_df_item<-sub_detail_df%>%dplyr::filter(SEED_ID %in% seed_item$Var1)
    sub_detail_df_item<-bind_rows(replicate(seed_item$Freq-1, sub_detail_df_item, simplify = FALSE))
    sub_detail_df_2<-bind_rows(sub_detail_df_2, sub_detail_df_item)
    
    sub_speciation_df_item<-sub_speciation_df%>%dplyr::filter(SEED_ID %in% seed_item$Var1)
    sub_speciation_df_item<-bind_rows(replicate(seed_item$Freq-1, sub_speciation_df_item, simplify = FALSE))
    sub_speciation_df_2<-bind_rows(sub_speciation_df_2, sub_speciation_df_item)
    
    sub_extinction_df_item<-sub_extinction_df%>%dplyr::filter(SEED_ID %in% seed_item$Var1)
    sub_extinction_df_item<-bind_rows(replicate(seed_item$Freq-1, sub_extinction_df_item, simplify = FALSE))
    sub_extinction_df_2<-bind_rows(sub_extinction_df_2, sub_extinction_df_item)
    
    sub_sp_character_item<-sub_sp_character%>%dplyr::filter(SEED_ID %in% seed_item$Var1)
    sub_sp_character_item<-bind_rows(replicate(seed_item$Freq-1, sub_sp_character_item, simplify = FALSE))
    sub_sp_character_2<-bind_rows(sub_sp_character_2, sub_sp_character_item)
  }
  sub_stat_df<-sub_stat_df_2
  sub_detail_df<-sub_detail_df_2
  sub_speciation_df<-sub_speciation_df_2
  sub_extinction_df<-sub_extinction_df_2
  sub_sp_character<-sub_sp_character_2
  
  if (is.null(speciation_df_rep)){
    stat_df_rep<-sub_stat_df
    detail_df_rep<-sub_detail_df
    speciation_df_rep<-sub_speciation_df
    extinction_df_rep<-sub_extinction_df
    sp_character_rep<-sub_sp_character
  }else{
    stat_df_rep<-bind_rows(stat_df_rep, sub_stat_df)
    detail_df_rep<-bind_rows(detail_df_rep, sub_detail_df)
    speciation_df_rep<-bind_rows(speciation_df_rep, sub_speciation_df)
    extinction_df_rep<-bind_rows(extinction_df_rep, sub_extinction_df)
    sp_character_rep<-bind_rows(sp_character_rep, sub_sp_character)
  }
  
  saveRDS(stat_df_rep, sprintf("%s/Data/items_rep_lat/stat_df_rep_lat_%d.rda", base, i))
  saveRDS(detail_df_rep, sprintf("%s/Data/items_rep_lat/detail_df_rep_lat_%d.rda", base, i))
  saveRDS(speciation_df_rep, sprintf("%s/Data/items_rep_lat/speciation_df_rep_lat_%d.rda", base, i))
  saveRDS(extinction_df_rep, sprintf("%s/Data/items_rep_lat/extinction_df_rep_lat_%d.rda", base, i))
  saveRDS(sp_character_rep, sprintf("%s/Data/items_rep_lat/sp_character_rep_lat_%d.rda", base, i))
  
}
#saveRDS(stat_df_rep, sprintf("%s/Data/stat_df_rep.rda", base))
#saveRDS(detail_df_rep, sprintf("%s/Data/detail_df_rep.rda", base))
#saveRDS(speciation_df_rep, sprintf("%s/Data/speciation_df_rep.rda", base))
#saveRDS(extinction_df_rep, sprintf("%s/Data/extinction_df_rep.rda", base))
#saveRDS(sp_character_rep, sprintf("%s/Data/sp_character_rep.rda", base))


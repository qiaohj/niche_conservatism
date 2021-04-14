library("dplyr")
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

sample_size<-100
rep<-100
if (F){
  seeds<-unique(stat$GLOBAL_ID)
  seeds_rep<-NULL
  for (i in c(1:rep)){
    print(i)
    sub_seeds<-data.frame(GLOBAL_ID=seeds[sample(length(seeds), sample_size)], REP=i)
    if (is.null(seeds_rep)){
      seeds_rep<-sub_seeds
    }else{
      seeds_rep<-bind_rows(seeds_rep, sub_seeds)
    }
  }
  mask_df<-readRDS(sprintf("%s/Data/ENV/mask_df.rda", base))
  seeds_rep<-inner_join(seeds_rep, mask_df%>%dplyr::filter(Y==0), by=c("GLOBAL_ID"="global_id"))
  saveRDS(seeds_rep, sprintf("%s/Data/seeds_rep.rda", base))
  
}
seeds_rep<-readRDS(sprintf("%s/Data/seeds_rep.rda", base))
detail<-readRDS(sprintf("%s/Data/detail.rda", base))
detail<-fix_df(detail)
speciation_df<-readRDS(sprintf("%s/Data/speciation_df.rda", base))
speciation_df<-fix_df(speciation_df)
extinction_df<-readRDS(sprintf("%s/Data/extinction_df.rda", base))
extinction_df<-fix_df(extinction_df)
sp_character<-readRDS(sprintf("%s/Data/sp_character.rda", base))
sp_character<-fix_df(sp_character)


i=1

for (i in c(1:rep)){
  if (file.exists(sprintf("%s/Data/items_rep/stat_df_rep_%d.rda", base, i))){
    next()
  }
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

  saveRDS(stat_df_rep, sprintf("%s/Data/items_rep/stat_df_rep_%d.rda", base, i))
  saveRDS(detail_df_rep, sprintf("%s/Data/items_rep/detail_df_rep_%d.rda", base, i))
  saveRDS(speciation_df_rep, sprintf("%s/Data/items_rep/speciation_df_rep_%d.rda", base, i))
  saveRDS(extinction_df_rep, sprintf("%s/Data/items_rep/extinction_df_rep_%d.rda", base, i))
  saveRDS(sp_character_rep, sprintf("%s/Data/items_rep/sp_character_rep_%d.rda", base, i))
  
}
#saveRDS(stat_df_rep, sprintf("%s/Data/stat_df_rep.rda", base))
#saveRDS(detail_df_rep, sprintf("%s/Data/detail_df_rep.rda", base))
#saveRDS(speciation_df_rep, sprintf("%s/Data/speciation_df_rep.rda", base))
#saveRDS(extinction_df_rep, sprintf("%s/Data/extinction_df_rep.rda", base))
#saveRDS(sp_character_rep, sprintf("%s/Data/sp_character_rep.rda", base))


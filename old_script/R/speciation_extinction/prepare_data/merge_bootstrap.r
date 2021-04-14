library(dplyr)
rep<-100
base<-"/home/huijieqiao/git/ees_3d_data/niche_conservatism"
stat_df_rep<-NULL

for (i in c(1:rep)){
  print(i)
  item<-readRDS(sprintf("%s/Data/items_rep/stat_df_rep_%d.rda", base, i))
  stat_speciation<-item%>%
    dplyr::select(GLOBAL_ID, DA, NB, EVO_TYPE, EVO_RATIO, N_SPECIATION, WARP_LABEL)
  colnames(stat_speciation)[6]<-"N"
  stat_speciation$TYPE<-"SPECIATION"
  stat_extinction<-item%>%
    dplyr::select(GLOBAL_ID, DA, NB, EVO_TYPE, EVO_RATIO, N_EXTINCTION, WARP_LABEL)
  colnames(stat_extinction)[6]<-"N"
  stat_extinction$TYPE<-"EXTINCTION"
  item_df<-bind_rows(stat_extinction, stat_speciation)
  item_df$REP<-i
  if (is.null(stat_df_rep)){
    stat_df_rep<-item_df
  }else{
    stat_df_rep<-bind_rows(stat_df_rep, item_df)
  }
}
saveRDS(stat_df_rep, sprintf("%s/Data/stat_df_rep.rda", base))



detail_df_rep<-NULL
speciation_df_rep<-NULL
extinction_df_rep<-NULL
sp_character_rep<-NULL
#saveRDS(detail_df_rep, sprintf("%s/Data/items_rep/detail_df_rep_%d.rda", base, i))
#saveRDS(speciation_df_rep, sprintf("%s/Data/items_rep/speciation_df_rep_%d.rda", base, i))
#saveRDS(extinction_df_rep, sprintf("%s/Data/items_rep/extinction_df_rep_%d.rda", base, i))
#saveRDS(sp_character_rep, sprintf("%s/Data/items_rep/sp_character_rep_%d.rda", base, i))
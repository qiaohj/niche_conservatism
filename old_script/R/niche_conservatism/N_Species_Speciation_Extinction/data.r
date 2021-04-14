library("dplyr")
base<-"/home/huijieqiao/git/ees_3d_data/niche_conservatism"


year=100
rep=100
speciation_extinction_by_year<-NULL
for (i in c(1:rep)){
  sub_detail_df<-readRDS(sprintf("%s/Data/items_rep/detail_df_rep_%d.rda", base, i))
  for (year in c(1200:0)){
    print(paste(year, i))
    N_SPECIES<-sub_detail_df%>%dplyr::filter((TO<=year)&(FROM>year))%>%
      dplyr::group_by(DA, NB, EVO_TYPE, EVO_RATIO, WARP_LABEL)%>%
      dplyr::summarise(N_SPECIES=n_distinct(SP))
    
    N_SPECIATION<-sub_detail_df%>%dplyr::filter((year==TO)&(TYPE=="NODE"))%>%
      dplyr::group_by(DA, NB, EVO_TYPE, EVO_RATIO, WARP_LABEL)%>%
      dplyr::summarise(N_SPECIATION=n())
    N_EXTINCTION<-sub_detail_df%>%dplyr::filter((year==TO)&(TYPE=="LEAF"))%>%
      dplyr::group_by(DA, NB, EVO_TYPE, EVO_RATIO, WARP_LABEL)%>%
      dplyr::summarise(N_EXTINCTION=n())
    
    item<-left_join(N_SPECIES, N_SPECIATION, by=c("DA", "NB", "EVO_TYPE", "EVO_RATIO", "WARP_LABEL"))
    item<-left_join(item, N_EXTINCTION, by=c("DA", "NB", "EVO_TYPE", "EVO_RATIO", "WARP_LABEL"))
    item[is.na(item)]<-0
    item$YEAR<-year
    item$REP<-i
    if (is.null(speciation_extinction_by_year)){
      speciation_extinction_by_year<-item
    }else{
      speciation_extinction_by_year<-bind_rows(speciation_extinction_by_year, item)
    }
  }
  saveRDS(speciation_extinction_by_year, 
          sprintf("%s/Data/speciation_extinction_by_year.rda", base))
}
saveRDS(speciation_extinction_by_year, 
        sprintf("%s/Data/speciation_extinction_by_year.rda", base))

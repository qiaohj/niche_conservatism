library("dplyr")
library(Rmisc)

base<-"/home/huijieqiao/git/ees_3d_data/niche_conservatism"


year=100
rep=19
richness_df<-NULL
cuts<-seq(from=-92.5, to=92.5, by=5)
i=1
for (i in c(1:rep)){ 
  print(i)
  sub_sp_character<-readRDS(sprintf("%s/Data/items_rep_lat/sp_character_rep_lat_%d.rda", base, i))
  sub_sp_character<-sub_sp_character%>%dplyr::filter(Y==0)
  for (j in c(2:length(cuts))){
    lat_from<-cuts[j-1]
    lat_to<-cuts[j]
    sub_item<-sub_sp_character%>%dplyr::filter(between(MIN_LAT, lat_from, lat_to)|
                                                 between(MAX_LAT, lat_from, lat_to))
    if (nrow(sub_item)<=0){
      next()
    }
    N_sp<-sub_item%>%dplyr::group_by(DA, NB, EVO_TYPE, EVO_RATIO, REP, WARP_LABEL)%>%
      dplyr::summarise(N_SP=n())
    N_sp$LAT_FROM<-lat_from
    N_sp$LAT_TO<-lat_to
    N_sp$LAT<-(lat_from+lat_to)/2
    if (is.null(richness_df)){
      richness_df<-N_sp
    }else{
      richness_df<-bind_rows(richness_df, N_sp)
    }
  }
  if (F){
    ggplot(richness_df, aes(x=LAT, y=N_SP, color=factor(EVO_TYPE)))+
      geom_line()+
      theme_bw()+
      facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')
  }
}
saveRDS(richness_df, 
        sprintf("%s/Data/richness_df_lat_boost_lat_based.rda", base))
richness_df_se<-richness_df%>%
  dplyr::group_by(EVO_TYPE, EVO_RATIO, LAT)%>%
  dplyr::summarise(N_SP_ALL=mean(N_SP),
                   CI=CI(N_SP)[2]-CI(N_SP)[3])
richness_df_se[which(richness_df_se$LAT==0),]
ggplot(richness_df_se, aes(x=LAT, y=N_SP_ALL, color=factor(EVO_TYPE)))+
  geom_line()+
  geom_errorbar(aes(ymin=N_SP_ALL-CI, ymax=N_SP_ALL+CI), width=.2,
                position=position_dodge(0.05))+
  theme_bw()+
  facet_wrap( ~ EVO_RATIO, ncol=1, scales = 'free')

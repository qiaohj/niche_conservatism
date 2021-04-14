library("dplyr")
library("Rmisc")
library("ggplot2")
library("RColorBrewer")
colors<-brewer.pal(8, "Dark2")
event_type_col<-colors[c(3, 1)]
evo_type_col<-c("Darwin I"=colors[1],
                "Darwin II"=colors[3],
                "Lazy"=colors[7])
base<-"/home/huijieqiao/git/ees_3d_data/niche_conservatism"

range_df<-readRDS(sprintf("%s/Data/range_df.rda", base))

if (F){
  range_temp<-range_df%>%
    dplyr::group_by(DA, NB, Y, EVO_TYPE, EVO_RATIO, WARP_LABEL)%>%
    dplyr::summarise(MEAN=mean(MEAN_RANG_TEMP),
                     SD=mean(SD_RANG_TEMP),
                     CI=mean(CI_RANG_TEMP),
                     N=n())
  #range_temp$LABEL<-paste(range_temp$EVO_TYPE)
  range_temp$WARP_LABEL<-paste(range_temp$NB, range_temp$DA, range_temp$EVO_RATIO)
  range_temp$Y<-range_temp$Y*-1
  saveRDS(range_temp, sprintf("%s/Data/range_temp.rda", base))
  
  range_prec<-range_df%>%
    dplyr::group_by(DA, NB, Y, EVO_TYPE, EVO_RATIO, WARP_LABEL)%>%
    dplyr::summarise(MEAN=mean(MEAN_RANG_PREC),
                     SD=mean(SD_RANG_PREC),
                     CI=mean(CI_RANG_PREC),
                     N=n())
  #range_temp$LABEL<-paste(range_temp$EVO_TYPE)
  range_prec$WARP_LABEL<-paste(range_prec$NB, range_prec$DA, range_prec$EVO_RATIO)
  range_prec$Y<-range_prec$Y*-1
  saveRDS(range_prec, sprintf("%s/Data/range_prec.rda", base))
  
  range_area<-range_df%>%
    dplyr::group_by(DA, NB, Y, EVO_TYPE, EVO_RATIO, WARP_LABEL)%>%
    dplyr::summarise(MEAN=mean(MEAN_AREA),
                     SD=mean(SD_AREA),
                     CI=mean(CI_AREA),
                     N=n())
  #range_temp$LABEL<-paste(range_temp$EVO_TYPE)
  range_area$WARP_LABEL<-paste(range_area$NB, range_area$DA, range_area$EVO_RATIO)
  range_area$Y<-range_area$Y*-1
  saveRDS(range_area, sprintf("%s/Data/range_area.rda", base))
  
  range_lat<-range_df%>%
    dplyr::group_by(DA, NB, Y, EVO_TYPE, EVO_RATIO, WARP_LABEL)%>%
    dplyr::summarise(MEAN=mean(MEAN_RANGE_LAT),
                     SD=mean(SD_RANGE_LAT),
                     CI=mean(CI_RANGE_LAT),
                     N=n())
  #range_temp$LABEL<-paste(range_temp$EVO_TYPE)
  range_lat$WARP_LABEL<-paste(range_lat$NB, range_lat$DA, range_lat$EVO_RATIO)
  range_lat$Y<-range_lat$Y*-1
  saveRDS(range_lat, sprintf("%s/Data/range_lat.rda", base))
  
  range_lon<-range_df%>%
    dplyr::group_by(DA, NB, Y, EVO_TYPE, EVO_RATIO, WARP_LABEL)%>%
    dplyr::summarise(MEAN=mean(MEAN_RANGE_LON),
                     SD=mean(SD_RANGE_LON),
                     CI=mean(CI_RANGE_LON),
                     N=n())
  #range_temp$LABEL<-paste(range_temp$EVO_TYPE)
  range_lon$WARP_LABEL<-paste(range_lon$NB, range_lon$DA, range_lon$EVO_RATIO)
  range_lon$Y<-range_lon$Y*-1
  saveRDS(range_lon, sprintf("%s/Data/range_lon.rda", base))
}
range_temp<-readRDS(sprintf("%s/Data/range_temp.rda", base))
p<-ggplot(range_temp %>% filter(Y>=-1100), aes(x=Y, y=MEAN, color=factor(EVO_TYPE)))+
  
  geom_errorbar(aes(ymin=MEAN-CI, ymax=MEAN+CI), width=.2,
                position=position_dodge(.9), alpha=0.2)+
  geom_line()+
  scale_fill_manual(values=evo_type_col)+
  scale_color_manual(values=evo_type_col)+
  theme_bw()+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')

ggsave(p, filename=sprintf("%s/Figures/range_temp_by_year.pdf", base))


range_prec<-readRDS(sprintf("%s/Data/range_prec.rda", base))
p<-ggplot(range_prec %>% filter(Y>=-1100), aes(x=Y, y=MEAN, color=factor(EVO_TYPE)))+
  
  geom_errorbar(aes(ymin=MEAN-CI, ymax=MEAN+CI), width=.2,
                position=position_dodge(.9), alpha=0.2)+
  scale_fill_manual(values=evo_type_col)+
  scale_color_manual(values=evo_type_col)+
  geom_line()+
  theme_bw()+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')

ggsave(p, filename=sprintf("%s/Figures/range_prec_by_year.pdf", base))

range_area<-readRDS(sprintf("%s/Data/range_area.rda", base))
p<-ggplot(range_area %>% filter(Y>=-1100), aes(x=Y, y=MEAN, color=factor(EVO_TYPE)))+
  
  geom_errorbar(aes(ymin=MEAN-CI, ymax=MEAN+CI), width=.2,
                position=position_dodge(.9), alpha=0.2)+
  scale_fill_manual(values=evo_type_col)+
  scale_color_manual(values=evo_type_col)+
  geom_line()+
  theme_bw()+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')

ggsave(p, filename=sprintf("%s/Figures/range_area_by_year.pdf", base))

range_lat<-readRDS(sprintf("%s/Data/range_lat.rda", base))
p<-ggplot(range_lat %>% filter(Y>=-1100), aes(x=Y, y=MEAN, color=factor(EVO_TYPE)))+
  
  geom_errorbar(aes(ymin=MEAN-CI, ymax=MEAN+CI), width=.2,
                position=position_dodge(.9), alpha=0.2)+
  scale_fill_manual(values=evo_type_col)+
  scale_color_manual(values=evo_type_col)+
  geom_line()+
  theme_bw()+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')

ggsave(p, filename=sprintf("%s/Figures/range_lat_by_year.pdf", base))

range_lon<-readRDS(sprintf("%s/Data/range_lon.rda", base))
p<-ggplot(range_lon %>% filter(Y>=-1100), aes(x=Y, y=MEAN, color=factor(EVO_TYPE)))+
  
  geom_errorbar(aes(ymin=MEAN-CI, ymax=MEAN+CI), width=.2,
                position=position_dodge(.9), alpha=0.2)+
  geom_line()+
  scale_fill_manual(values=evo_type_col)+
  scale_color_manual(values=evo_type_col)+
  theme_bw()+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')

ggsave(p, filename=sprintf("%s/Figures/range_lon_by_year.pdf", base))

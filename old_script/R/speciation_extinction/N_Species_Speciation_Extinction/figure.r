library("ggplot2")
library("dplyr")
library("Hmisc")
library("Rmisc")
library("vegan")
library("lubridate")
library("zoo")
library("stats")
library("GeneCycle")
library("TTR")
library("forecast")
library("gridExtra")
library("tseries")
library("SimilarityMeasures")
library("patchwork")
library("hrbrthemes")
library("pastecs")
library("mgcv")

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
cols<-c("turquoise",
        "chocolate",
        "black")
rep=100
x_year_label<-"Year (*100)"
ribbon_alpha<-0.1


stat_df_rep<-readRDS(sprintf("%s/Data/stat_df_rep.rda", base))
stat_df_sum<-stat_df_rep%>%dplyr::group_by(DA, NB, EVO_TYPE, EVO_RATIO, WARP_LABEL, TYPE, REP)%>%
  dplyr::summarise(N=sum(N))

stat_df_sum_se<-stat_df_sum%>%ungroup()%>%
  dplyr::group_by(DA, NB, EVO_TYPE, EVO_RATIO, WARP_LABEL, TYPE)%>%
  dplyr::summarise(MEAN=mean(N),
                   SD=sd(N),
                   CI=CI(N)[2]-CI(N)[3],
                   N_SEED=n())
p<-ggplot(stat_df_sum_se, 
          aes(x=EVO_TYPE, y= MEAN, fill=factor(TYPE)))+
  geom_bar(stat="identity", position = "dodge2")+
  geom_errorbar(aes(ymin=MEAN-CI, ymax=MEAN+CI), width=.2,
                position=position_dodge(.9))+
  theme_bw()+
  xlab("niche evolution type")+
  ylab("N")+
  ggtitle("N of speciation and extinction per niche evolution type")+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')
ggsave(p, filename=sprintf("%s/Figures/speciation_extinction_by_scenario.png", base))

p<-ggplot(stat_df_sum %>% dplyr::filter(TYPE=="SPECIATION"), 
          aes(x=EVO_TYPE, y= N, fill=factor(EVO_TYPE)))+
  geom_boxplot(position = "dodge2")+
  theme_bw()+
  xlab("niche evolution type")+
  ylab("N")+
  ggtitle("N of speciation per niche evolution type")+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')
ggsave(p, filename=sprintf("%s/Figures/speciation_by_scenario.png", base))

p<-ggplot(stat_df_sum %>% dplyr::filter(TYPE=="EXTINCTION"), 
          aes(x=EVO_TYPE, y= N, fill=factor(EVO_TYPE)))+
  geom_boxplot(position = "dodge2")+
  theme_bw()+
  xlab("niche evolution type")+
  ylab("N")+
  ggtitle("N of extinction per niche evolution type")+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')
ggsave(p, filename=sprintf("%s/Figures/extinction_by_scenario.png", base))


stat<-readRDS(sprintf("%s/Data/stat.rda", base))
stat<-fix_df(stat)
stat_rank<-stat%>%
  dplyr::group_by(GLOBAL_ID, DA, NB, EVO_RATIO, WARP_LABEL) %>% 
  dplyr::mutate(RANK_SPECIATION = rank(N_SPECIATION * -1, ties.method = "min"),
                RANK_EXTINCTION = rank(N_EXTINCTION * -1, ties.method = "min"),
                ALL_SPECIATION = n_distinct(N_SPECIATION),
                ALL_EXTINCTION = n_distinct(N_EXTINCTION))

stat_rank_speciation<-stat_rank%>%dplyr::filter(ALL_SPECIATION!=1)

stat_rank_speciation<-stat_rank_speciation%>%ungroup()%>%
  dplyr::group_by(DA, NB, EVO_TYPE, EVO_RATIO, WARP_LABEL, RANK_SPECIATION) %>% 
  dplyr::summarise(N=n())

p<-ggplot(stat_rank_speciation, 
          aes(x=EVO_TYPE, y= N, fill=factor(RANK_SPECIATION)))+
  geom_bar(stat="identity", position = "dodge2")+
  theme_bw()+
  xlab("niche evolution type")+
  ylab("N")+
  ggtitle("Rank speciation per niche evolution type")+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')

ggsave(p, filename=sprintf("%s/Figures/speciation_rank_by_scenario.png", base))

stat_rank_extinction<-stat_rank%>%dplyr::filter(ALL_EXTINCTION!=1)

stat_rank_extinction<-stat_rank_extinction%>%ungroup()%>%
  dplyr::group_by(DA, NB, EVO_TYPE, EVO_RATIO, WARP_LABEL, RANK_EXTINCTION) %>% 
  dplyr::summarise(N=n())
p<-ggplot(stat_rank_extinction, 
          aes(x=EVO_TYPE, y= N, fill=factor(RANK_EXTINCTION)))+
  geom_bar(stat="identity", position = "dodge2")+
  theme_bw()+
  xlab("niche evolution type")+
  ylab("N")+
  ggtitle("Rank extinction per niche evolution type")+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')

ggsave(p, filename=sprintf("%s/Figures/extinction_rank_by_scenario.png", base))


speciation_extinction_by_year<-readRDS(sprintf("%s/Data/speciation_extinction_by_year.rda", base))
speciation_extinction_by_year_se<-speciation_extinction_by_year%>%ungroup()%>%
  dplyr::group_by(DA, NB, EVO_TYPE, EVO_RATIO, WARP_LABEL, YEAR)%>%
  dplyr::summarise(MEAN_N_SPECIES=mean(N_SPECIES),
                   SD_N_SPECIES=sd(N_SPECIES),
                   CI_N_SPECIES=CI(N_SPECIES)[2]-CI(N_SPECIES)[3],
                   MEAN_N_SPECIATION=mean(N_SPECIATION),
                   SD_N_SPECIATION=sd(N_SPECIATION),
                   CI_N_SPECIATION=CI(N_SPECIATION)[2]-CI(N_SPECIATION)[3],
                   MEAN_N_EXTINCTION=mean(N_EXTINCTION),
                   SD_N_EXTINCTION=sd(N_EXTINCTION),
                   CI_N_EXTINCTION=CI(N_EXTINCTION)[2]-CI(N_EXTINCTION)[3],
                   N_SEED=n())

saveRDS(speciation_extinction_by_year_se, 
        sprintf("%s/Data/speciation_extinction_by_year_se.rda", base))
speciation_extinction_by_year_se$YEAR<-speciation_extinction_by_year_se$YEAR*-1

speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==0), "MEAN_N_EXTINCTION"]<-0
speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==0), "SD_N_EXTINCTION"]<-0
speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==0), "CI_N_EXTINCTION"]<-0

speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==0), "MEAN_N_SPECIATION"]<-0
speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==0), "SD_N_SPECIATION"]<-0
speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==0), "CI_N_SPECIATION"]<-0

speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==-1200), "MEAN_N_SPECIATION"]<-0
speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==-1200), "SD_N_SPECIATION"]<-0
speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==-1200), "CI_N_SPECIATION"]<-0

speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==-1199), "MEAN_N_SPECIATION"]<-0
speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==-1199), "SD_N_SPECIATION"]<-0
speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==-1199), "CI_N_SPECIATION"]<-0

speciation_extinction_by_year_se$AVERAGE_SPECIATION<-
  speciation_extinction_by_year_se$MEAN_N_SPECIATION/
  speciation_extinction_by_year_se$MEAN_N_SPECIES
speciation_extinction_by_year_se$AVERAGE_EXTINCTION<-
  speciation_extinction_by_year_se$MEAN_N_EXTINCTION/
  speciation_extinction_by_year_se$MEAN_N_SPECIES


p<-ggplot(speciation_extinction_by_year_se, 
          aes(x=YEAR, y= MEAN_N_SPECIES, color=factor(EVO_TYPE)))+
  geom_line()+
  geom_errorbar(aes(ymin=MEAN_N_SPECIES-CI_N_SPECIES, ymax=MEAN_N_SPECIES+CI_N_SPECIES), width=.2,
                position=position_dodge(.9), alpha=ribbon_alpha)+
  theme_bw()+
  xlab("Time step")+
  ylab("N Species")+
  ggtitle("N of Species by year")+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')
ggsave(p, filename=sprintf("%s/Figures/species_by_year.png", base))

speciation_extinction_by_year_se$EVO_TYPE<-factor(speciation_extinction_by_year_se$EVO_TYPE, 
                                                  levels = c("Lazy", "Darwin II", "Darwin I"))
p<-ggplot(speciation_extinction_by_year_se %>% dplyr::filter(YEAR>=-1100), 
          aes(x=YEAR, y= MEAN_N_SPECIATION, color=EVO_TYPE))+
  geom_line()+
  geom_errorbar(aes(ymin=MEAN_N_SPECIATION-CI_N_SPECIATION, 
                    ymax=MEAN_N_SPECIATION+CI_N_SPECIATION), width=.2,
                position=position_dodge(.9), alpha=ribbon_alpha)+
  theme_bw()+
  xlab("Time step")+
  ylab("N SPECIATION")+
  ggtitle("N of SPECIATION by year")+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')
ggsave(p, filename=sprintf("%s/Figures/speciation_by_year.png", base))

p<-ggplot(speciation_extinction_by_year_se %>% dplyr::filter((YEAR>=-1100)&(YEAR<0)), 
          aes(x=YEAR, y= MEAN_N_EXTINCTION, color=EVO_TYPE))+
  geom_line()+
  #geom_errorbar(aes(ymin=MEAN_N_EXTINCTION-CI_N_EXTINCTION, ymax=MEAN_N_EXTINCTION+CI_N_EXTINCTION), width=.2,
  #              position=position_dodge(.9), alpha=ribbon_alpha)+
  theme_bw()+
  xlab("Time step")+
  ylab("N EXTINCTION")+
  ggtitle("N of EXTINCTION by year")+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')
ggsave(p, filename=sprintf("%s/Figures/extinction_by_year.png", base))

p<-ggplot(speciation_extinction_by_year_se %>% dplyr::filter(YEAR>=-1100), 
          aes(x=YEAR, y= AVERAGE_SPECIATION, color=EVO_TYPE))+
  geom_line()+
  #geom_errorbar(aes(ymin=MEAN_N_SPECIATION-CI_N_SPECIATION, ymax=MEAN_N_SPECIATION+CI_N_SPECIATION), width=.2,
  #              position=position_dodge(.9), alpha=ribbon_alpha)+
  theme_bw()+
  xlab("Time step")+
  ylab("N SPECIATION PER SPECIES")+
  ggtitle("SPECIATION PER SPECIES by year")+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')
ggsave(p, filename=sprintf("%s/Figures/speciation_per_species_by_year.png", base))

p<-ggplot(speciation_extinction_by_year_se %>% dplyr::filter((YEAR>=-1100)&(YEAR<0)), 
          aes(x=YEAR, y= AVERAGE_EXTINCTION, color=EVO_TYPE))+
  geom_line()+
  #geom_errorbar(aes(ymin=MEAN_N_EXTINCTION-CI_N_EXTINCTION, ymax=MEAN_N_EXTINCTION+CI_N_EXTINCTION), width=.2,
  #              position=position_dodge(.9), alpha=ribbon_alpha)+
  theme_bw()+
  xlab("Time step")+
  ylab("N EXTINCTION PER SPECIES")+
  ggtitle("N EXTINCTION PER SPECIES by year")+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')
ggsave(p, filename=sprintf("%s/Figures/extinction_per_species_by_year.png", base))





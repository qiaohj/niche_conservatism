library("dplyr")
library("Rmisc")
library("ggplot2")
base<-"/home/huijieqiao/git/ees_3d_data/niche_conservatism"

speciation_by_lat<-readRDS(sprintf("%s/Data/speciation_by_lat.rda", base))
extinction_by_lat<-readRDS(sprintf("%s/Data/extinction_by_lat.rda", base))

speciation_by_lat_se<-speciation_by_lat%>%
  dplyr::group_by(LAT_ROUND, EVO_TYPE, EVO_RATIO, ENV, DA, NB)%>%
  dplyr::summarise(MEAN=mean(N),
                   SD=sd(N),
                   CI=CI(N)[2]-CI(N)[3],
                   N=n())
ggplot(speciation_by_lat_se%>%filter((EVO_TYPE=="Lazy")&(EVO_RATIO==0.005)), 
       aes(x=LAT_ROUND, y=MEAN, color=factor(ENV)))+
  #geom_point()+
  geom_errorbar(aes(ymin=MEAN-CI, ymax=MEAN+CI), width=.2,
                position=position_dodge(.9))+
  geom_line()+
  theme_bw()+
  facet_wrap( ~ EVO_TYPE+EVO_RATIO+DA+NB, ncol=1, scales = 'free')

p<-ggplot(speciation_by_lat_se%>%filter(EVO_TYPE=="Lazy"), aes(x=LAT_ROUND, y=MEAN, color=factor(ENV)))+
  #geom_point()+
  geom_errorbar(aes(ymin=MEAN-CI, ymax=MEAN+CI), width=.2,
                position=position_dodge(.9))+
  geom_line()+
  theme_bw()+
  facet_wrap( ~ EVO_TYPE+EVO_RATIO+DA+NB, ncol=4, scales = 'free')
ggsave(p, filename=sprintf("%s/Figures/speciation_by_lat.pdf", base))

extinction_by_lat_se<-extinction_by_lat%>%
  dplyr::group_by(LAT_ROUND, EVO_TYPE, EVO_RATIO, ENV)%>%
  dplyr::summarise(MEAN=mean(N),
                   SD=sd(N),
                   CI=CI(N)[3]-CI(N[2]),
                   N=n())
p<-ggplot(extinction_by_lat_se, aes(x=LAT_ROUND, y=MEAN, color=factor(ENV)))+
  #geom_point()+
  #geom_errorbar(aes(ymin=MEAN-CI, ymax=MEAN+CI), width=.2,
  #              position=position_dodge(.9))+
  geom_line()+
  theme_bw()+
  facet_wrap( ~ EVO_TYPE+EVO_RATIO, ncol=2, scales = 'free')
ggsave(p, filename=sprintf("%s/Figures/extinction_by_lat.pdf", base))

library(ggplot2)
library(ggthemes)
library(data.table)
library(ggpubr)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")

if (F){
  d1<-readRDS("../Data/N_speciation_extinction_items/N_speciation_extinction_BROAD_GOOD.rda")
  d1$species_evo_level<-0
  
  d2<-readRDS("../Data/N_speciation_extinction_items/N_speciation_extinction_BROAD_POOR.rda")
  d2$species_evo_level<-0
  
  d3<-readRDS("../Data/N_speciation_extinction_items/N_speciation_extinction_NARROW_GOOD.rda")
  d3$species_evo_level<-0
  
  d4<-readRDS("../Data/N_speciation_extinction_items/N_speciation_extinction_NARROW_POOR.rda")
  d4$species_evo_level<-0
  
  d
  
  d<-rbindlist(list(d1, d2, d3, d4))
  saveRDS(d, "../Data/N_speciation_extinction/N_speciation_extinction.rda")
  
  d_final<-d[year==0]
  summary(d_final$N_SPECIES)
  mean<-mean(d_final[N_SPECIES>0]$N_SPECIES)
  sd<-sd(d_final[N_SPECIES>0]$N_SPECIES)
  
  d1<-readRDS("../Data/N_speciation_extinction_items/N_speciation_extinction_BROAD_GOOD_without_IQR_outliers.rda")
  d1$species_evo_level<-0
  
  d2<-readRDS("../Data/N_speciation_extinction_items/N_speciation_extinction_BROAD_POOR_without_IQR_outliers.rda")
  d2$species_evo_level<-0
  
  d3<-readRDS("../Data/N_speciation_extinction_items/N_speciation_extinction_NARROW_GOOD_without_IQR_outliers.rda")
  d3$species_evo_level<-0
  
  d4<-readRDS("../Data/N_speciation_extinction_items/N_speciation_extinction_NARROW_POOR_without_IQR_outliers.rda")
  d4$species_evo_level<-0
  
  d
  
  d<-rbindlist(list(d1, d2, d3, d4))
  saveRDS(d, "../Data/N_speciation_extinction/N_speciation_extinction_without_3SD_outliers.rda")
  
  
  d1<-readRDS("../Data/distribution_traits_items/distribution_traits_BROAD_GOOD_without_3SD_outliers.rda")
  d1$species_evo_level<-0
  
  d2<-readRDS("../Data/distribution_traits_items/distribution_traits_BROAD_POOR_without_3SD_outliers.rda")
  d2$species_evo_level<-0
  #colnames(d2)[17]<-"global_id"
  d3<-readRDS("../Data/distribution_traits_items/distribution_traits_NARROW_GOOD_without_3SD_outliers.rda")
  d3$species_evo_level<-0
  
  d4<-readRDS("../Data/distribution_traits_items/distribution_traits_NARROW_POOR_without_3SD_outliers.rda")
  d4$species_evo_level<-0
  
  
  
  d<-rbindlist(list(d1, d2, d3, d4))
  
  saveRDS(d, "../Data/distribution_traits/distribution_traits_without_3SD_outliers.rda")
  
  
  
  d1<-readRDS("../Data/distribution_traits_items/distribution_traits_BROAD_GOOD.rda")
  d1$species_evo_level<-0
  
  d2<-readRDS("../Data/distribution_traits_items/distribution_traits_BROAD_POOR.rda")
  d2$species_evo_level<-0
  #colnames(d2)[17]<-"global_id"
  d3<-readRDS("../Data/distribution_traits_items/distribution_traits_NARROW_GOOD.rda")
  d3$species_evo_level<-0
  
  d4<-readRDS("../Data/distribution_traits_items/distribution_traits_NARROW_POOR.rda")
  d4$species_evo_level<-0
  
  
  
  d<-rbindlist(list(d1, d2, d3, d4))
  
  saveRDS(d, "../Data/distribution_traits/distribution_traits.rda")
  
}
if (F){
  d<-readRDS("../Data/N_speciation_extinction/N_speciation_extinction.rda")
  #cols<-c("nb", "da", "global_id", "species_evo_type", "directional_speed", "species_evo_level")
  #d[, min_year:=min(year), by=cols]
  
  #d_unique<-unique(d[, ..cols])
  d$outlier<-"F"
  outliers<-readRDS("../Data/outliers/outliers_3SD.rda")
  d[global_id %in% unique(outliers$global_id)]$outlier<-"T"
  d_end<-d[year==0]
  range(d_end$N_SPECIES)
  sum(d_end$N_SPECIES)
  d_se<-d[, .(N=.N, 
              N_SPECIES=sum(N_SPECIES), 
              SD_N_SPECIES=sd(N_SPECIES),
              MEDIAN_N_SPECIES=quantile(N_SPECIES, 0.5),
              QUANTILE_25_N_SPECIES=quantile(N_SPECIES, 0.25),
              QUANTILE_75_N_SPECIES=quantile(N_SPECIES, 0.75)
  ),
  by=list(nb, da, species_evo_level, species_evo_type, directional_speed, year)]
  
  #d_quantile<-d[year==0, .(N_99=quantile(N_SPECIES, 0.99)),
  #              by=list(nb, da, species_evo_level, species_evo_type, directional_speed)]
  #d_with_quantile<-merge(d_quantile, d_end, 
  #                       by=c("nb", "da", "species_evo_level", "species_evo_type", "directional_speed"))
  #outliers<-d_with_quantile[N_SPECIATION>N_99]
  
  d_se_without_ourliers<-d[outlier=="F", .(N=.N, N_SPECIES=sum(N_SPECIES), SD_N_SPECIES=sd(N_SPECIES)),
                           by=list(nb, da, species_evo_level, species_evo_type, 
                                   directional_speed, year, outlier)]
  sum(d[year==0 & outlier=="F"]$N_SPECIES)
  d_se_without_ourliers$label<-paste(d_se_without_ourliers$nb, d_se_without_ourliers$da)
  d_se_without_ourliers$label2<-paste(d_se_without_ourliers$species_evo_type, d_se_without_ourliers$directional_speed)
  
  p<-ggplot(d_se_without_ourliers[((directional_speed %in% c(0) & species_evo_type==1) |
                                     (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                                     (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7))) &
                                    species_evo_level==0 & year==0])+
    geom_bar(aes(x=label2, y=N_SPECIES, fill=label), 
             stat = "identity", position = position_dodge(0.9))+
    scale_fill_colorblind()
  p
  d[N_ALL_SPECIES<N_SPECIES]$N_ALL_SPECIES<-d[N_ALL_SPECIES<N_SPECIES]$N_SPECIES
  #ggsave(p, filename="../Figures/N_Species/N_Species_end_no_outliers.png", width=12, height=10)
  d$R_SPECIATION_SPECIES_YEAR<-d$N_SPECIATION_YEAR/d$N_SPECIES
  d[is.nan(R_SPECIATION_SPECIES_YEAR)]$R_SPECIATION_SPECIES_YEAR<-0
  d$R_EXTINCTION_SPECIES_YEAR<-d$N_EXTINCTION_YEAR/d$N_SPECIES
  d[is.nan(R_EXTINCTION_SPECIES_YEAR)]$R_EXTINCTION_SPECIES_YEAR<-0
  
  d$R_SPECIATION_SPECIES<-d$N_SPECIATION/d$N_ALL_SPECIES
  d[is.nan(R_SPECIATION_SPECIES)]$R_SPECIATION_SPECIES<-0
  d$R_EXTINCTION_SPECIES<-d$N_EXTINCTION/d$N_ALL_SPECIES
  d[is.nan(R_EXTINCTION_SPECIES)]$R_EXTINCTION_SPECIES<-0
  
  d_se<-d[N_SPECIES>0, .(N=.N, 
                         N_SPECIES=sum(N_SPECIES), 
                         N_ALL_SPECIES=sum(N_ALL_SPECIES), 
                         N_SPECIATION=sum(N_SPECIATION),
                         N_EXTINCTION=sum(N_EXTINCTION),
                         N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
                         N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
                         R_EXTINCTION_SPECIES=mean(R_EXTINCTION_SPECIES, na.rm=T),
                         R_SPECIATION_SPECIES=mean(R_SPECIATION_SPECIES, na.rm=T),
                         SD_R_EXTINCTION_SPECIES=sd(R_EXTINCTION_SPECIES, na.rm=T),
                         SD_R_SPECIATION_SPECIES=sd(R_SPECIATION_SPECIES, na.rm=T),
                         R_EXTINCTION_SPECIES_YEAR=mean(R_EXTINCTION_SPECIES_YEAR, na.rm=T),
                         R_SPECIATION_SPECIES_YEAR=mean(R_SPECIATION_SPECIES_YEAR, na.rm=T),
                         SD_R_EXTINCTION_SPECIES_YEAR=sd(R_EXTINCTION_SPECIES_YEAR, na.rm=T),
                         SD_R_SPECIATION_SPECIES_YEAR=sd(R_SPECIATION_SPECIES_YEAR, na.rm=T),
                         MEDIAN_N_SPECIES=quantile(N_SPECIES, 0.5),
                         QUANTILE_25_N_SPECIES=quantile(N_SPECIES, 0.25),
                         QUANTILE_75_N_SPECIES=quantile(N_SPECIES, 0.75),
                         MEDIAN_N_SPECIATION=quantile(N_SPECIATION, 0.5),
                         QUANTILE_25_N_SPECIATION=quantile(N_SPECIATION, 0.25),
                         QUANTILE_75_N_SPECIATION=quantile(N_SPECIATION, 0.75),
                         MEDIAN_N_EXTINCTION=quantile(N_EXTINCTION, 0.5),
                         QUANTILE_25_N_EXTINCTION=quantile(N_EXTINCTION, 0.25),
                         QUANTILE_75_N_EXTINCTION=quantile(N_EXTINCTION, 0.75)
  ),
  by=list(nb, da, species_evo_level, species_evo_type, directional_speed, year, outlier)]
  
  plot(d_se$R_SPECIATION_SPECIES, d_se$R_SPECIATION_SPECIES_YEAR)
  plot(d_se$R_EXTINCTION_SPECIES, d_se$R_EXTINCTION_SPECIES_YEAR)
  hist(d_se[N_SPECIES>0]$R_SPECIATION_SPECIES)
  d_se[N_SPECIES<=0]
  d_se_seed<-d[N_SPECIES>0, .(N_SEED=length(unique(global_id))),
               by=list(nb, da, species_evo_level, species_evo_type, directional_speed, year, outlier)]
  
  range(d$N_SPECIES)
  d_se$label<-paste(d_se$nb, d_se$da)
  d_se$label2<-paste(d_se$species_evo_type, d_se$directional_speed)
  
  d_se_seed$label<-paste(d_se_seed$nb, d_se_seed$da)
  d_se_seed$label2<-paste(d_se_seed$species_evo_type, d_se_seed$directional_speed)
  
  saveRDS(d_se, "../Figures/Figure1.Speciation.Extinction.Rate/figure_data.rda")
  saveRDS(d_se_seed, "../Figures/Figure1.Speciation.Extinction.Rate/d_se_seed.rda")
  
  d_se_all<-d[N_SPECIES>0, .(N=.N, 
                             N_SPECIES=sum(N_SPECIES), 
                             N_ALL_SPECIES=sum(N_ALL_SPECIES), 
                             
                             N_SPECIATION=sum(N_SPECIATION),
                             N_EXTINCTION=sum(N_EXTINCTION),
                             N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
                             N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
                             R_EXTINCTION_SPECIES=mean(R_EXTINCTION_SPECIES, na.rm=T),
                             R_SPECIATION_SPECIES=mean(R_SPECIATION_SPECIES, na.rm=T),
                             SD_R_EXTINCTION_SPECIES=sd(R_EXTINCTION_SPECIES, na.rm=T),
                             SD_R_SPECIATION_SPECIES=sd(R_SPECIATION_SPECIES, na.rm=T),
                             R_EXTINCTION_SPECIES_YEAR=mean(R_EXTINCTION_SPECIES_YEAR, na.rm=T),
                             R_SPECIATION_SPECIES_YEAR=mean(R_SPECIATION_SPECIES_YEAR, na.rm=T),
                             SD_R_EXTINCTION_SPECIES_YEAR=sd(R_EXTINCTION_SPECIES_YEAR, na.rm=T),
                             SD_R_SPECIATION_SPECIES_YEAR=sd(R_SPECIATION_SPECIES_YEAR, na.rm=T),
                             MEDIAN_N_SPECIES=quantile(N_SPECIES, 0.5),
                             QUANTILE_25_N_SPECIES=quantile(N_SPECIES, 0.25),
                             QUANTILE_75_N_SPECIES=quantile(N_SPECIES, 0.75),
                             MEDIAN_N_SPECIATION=quantile(N_SPECIATION, 0.5),
                             QUANTILE_25_N_SPECIATION=quantile(N_SPECIATION, 0.25),
                             QUANTILE_75_N_SPECIATION=quantile(N_SPECIATION, 0.75),
                             MEDIAN_N_EXTINCTION=quantile(N_EXTINCTION, 0.5),
                             QUANTILE_25_N_EXTINCTION=quantile(N_EXTINCTION, 0.25),
                             QUANTILE_75_N_EXTINCTION=quantile(N_EXTINCTION, 0.75)
  ),
  by=list(species_evo_level, species_evo_type, directional_speed, year)]
  saveRDS(d_se_all, "../Figures/Figure1.Speciation.Extinction.Rate/figure_data_all.rda")
}



d_se<-readRDS("../Figures/Figure1.Speciation.Extinction.Rate/figure_data.rda")
d_se_seed<-readRDS("../Figures/Figure1.Speciation.Extinction.Rate/d_se_seed.rda")
source("commons/functions.r")
#1: conservatism
#2: shift-directional
#3: expansion-directional
#4: expansion-omnidirectional
#5: random-central
#6: random-symmetrical
#7: random-asymmetrical
d_se<-d_se[((directional_speed %in% c(0) & species_evo_type==1) |
              (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
              (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7))) & 
             species_evo_level==0]
d_se$evo_type<-format_evoType(d_se$species_evo_type)

d_se_all<-d_se[, .(N_SPECIES=sum(N_SPECIES),
                   N_SPECIATION=sum(N_SPECIATION),
                   N_EXTINCTION=sum(N_EXTINCTION),
                   N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
                   N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
                   R_SPECIATION_SPECIES=mean(R_SPECIATION_SPECIES) * 1000,
                   R_EXTINCTION_SPECIES=mean(R_EXTINCTION_SPECIES) * 1000),
               by=list(species_evo_level, species_evo_type, 
                       directional_speed, year, evo_type, outlier)]
cor(d_se$N_SPECIES, d_se$N_SPECIATION)
hist(d_se_all$R_SPECIATION_SPECIES)
d_se_all$label<-sprintf("%s (%s)", d_se_all$evo_type, as.character(d_se_all$directional_speed))

d_se_all_100<-d_se_all
d_se_all_100$year_100<-floor(d_se_all_100$year/100) * 100
d_se_all_100<-d_se_all_100[, .(N_SPECIES=sum(N_SPECIES),
                               N_SPECIATION=sum(N_SPECIATION),
                               N_EXTINCTION=sum(N_EXTINCTION),
                               N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
                               N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
                               R_SPECIATION_SPECIES=mean(R_SPECIATION_SPECIES),
                               R_EXTINCTION_SPECIES=mean(R_EXTINCTION_SPECIES),
                               SD_R_SPECIATION_SPECIES=sd(R_SPECIATION_SPECIES),
                               SD_R_EXTINCTION_SPECIES=sd(R_EXTINCTION_SPECIES)),
                           by=list(species_evo_level, species_evo_type, 
                                   directional_speed, year_100, evo_type, outlier)]
hist(d_se_all_100$R_SPECIATION_SPECIES)
d_se_all_100[species_evo_type %in% c(5, 6, 7)]$directional_speed<-0

p1<-ggplot(d_se_all[outlier=="F" & year<=1000])+
  #geom_ribbon(aes(x=year_100*-10, ymin=R_SPECIATION_SPECIES-SD_R_SPECIATION_SPECIES, 
  #                ymax=R_SPECIATION_SPECIES+SD_R_SPECIATION_SPECIES, 
  #                fill=evo_type, 
  #                linetype=factor(directional_speed)), alpha=0.3)+
  geom_line(aes(x=year*-0.1, y=N_SPECIATION, color=evo_type, 
                linetype=factor(directional_speed)))+
  #geom_text(data=d_se_all[outlier=="F" & year==0], aes(x=1, y=R_SPECIATION_SPECIES, label=label, color=evo_type), 
  #          hjust="left", vjust="middle", size=5)+
  labs(x="Xk years before present", y="Number of speciation per 1k species", 
       color="Evolution type", linetype="Evolution rate")+
  theme_bw()+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  scale_y_continuous(breaks=c(0, 20000, 40000, 60000, 80000), labels=c("0k", "20k", "40k", "60k", "80k"))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")
p1

p2<-ggplot(d_se_all[outlier=="F" & year<=1000])+
  #geom_ribbon(aes(x=year_100*-10, ymin=R_EXTINCTION_SPECIES-SD_R_EXTINCTION_SPECIES, 
  #                ymax=R_EXTINCTION_SPECIES+SD_R_EXTINCTION_SPECIES, 
  #                fill=evo_type, 
  #                linetype=factor(directional_speed)), alpha=0.3)+
  geom_line(aes(x=year*-0.1, y=N_EXTINCTION, color=evo_type, 
                linetype=factor(directional_speed)))+
  #geom_text(data=d_se_all[outlier=="F" & year==0], aes(x=1, y=R_SPECIATION_SPECIES, label=label, color=evo_type), 
  #          hjust="left", vjust="middle", size=5)+
  labs(x="Xk years before present", y="Number of extinction per 1k species", 
       color="Evolution type", linetype="Evolution rate")+
  theme_bw()+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  scale_y_continuous(breaks=c(0, 2000, 4000, 6000, 8000, 10000, 12000), labels=c("0k", "2k", "4k", "6k", "8k", "10k", "12k"))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")
p2
d_se_all[species_evo_type %in% c(5, 6, 7)]$directional_speed<-0
p3<-ggplot(d_se_all[outlier=="F" & year<=1000])+
  #geom_ribbon(aes(x=year_100*-10, ymin=R_EXTINCTION_SPECIES-SD_R_EXTINCTION_SPECIES, 
  #                ymax=R_EXTINCTION_SPECIES+SD_R_EXTINCTION_SPECIES, 
  #                fill=evo_type, 
  #                linetype=factor(directional_speed)), alpha=0.3)+
  geom_line(aes(x=year*-0.1, y=N_SPECIES, color=evo_type, 
                linetype=factor(directional_speed)))+
  #geom_text(data=d_se_all[outlier=="F" & year==0], aes(x=1, y=R_SPECIATION_SPECIES, label=label, color=evo_type), 
  #          hjust="left", vjust="middle", size=5)+
  labs(x="Xk years before present", y="Number of species", 
       color="Evolution type", linetype="Evolution rate")+
  theme_bw()+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  scale_y_continuous(breaks=c(0, 20000, 40000, 60000, 80000), labels=c("0k", "20k", "40k", "60k", "80k"))+
  theme(legend.position = c(0.3, 0.51), legend.box="horizontal")
#theme(legend.position = "none")
p3
p<-ggarrange(plotlist=list(p1, p2, p3), nrow=3, ncol=1, common.legend = F)
ggsave(p, filename="../Figures/Figure1.Speciation.Extinction.Rate/Figure1.N.Speciation.Extinction.png", width=8, height=12)


p1<-ggplot(d_se_all_100[outlier=="F" & year_100<=1000])+
  #geom_ribbon(aes(x=year_100*-10, ymin=R_SPECIATION_SPECIES-SD_R_SPECIATION_SPECIES, 
  #                ymax=R_SPECIATION_SPECIES+SD_R_SPECIATION_SPECIES, 
  #                fill=evo_type, 
  #                linetype=factor(directional_speed)), alpha=0.3)+
  geom_line(aes(x=year_100*-0.1, y=R_SPECIATION_SPECIES, color=evo_type, 
                linetype=factor(directional_speed)))+
  #geom_text(data=d_se_all[outlier=="F" & year==0], aes(x=1, y=R_SPECIATION_SPECIES, label=label, color=evo_type), 
  #          hjust="left", vjust="middle", size=5)+
  labs(x="Xk years before present", y="Number of speciation per 1k species", 
       color="Evolution type", linetype="Evolution rate")+
  theme_bw()+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  scale_y_continuous(breaks=c(0, 0.5, 1, 1.5, 2, 2.5), labels=c("0", "0.5", "1.0", "1.5", "2.0", "2.5"))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")
p1

p2<-ggplot(d_se_all_100[outlier=="F" & year_100<=1000])+
  #geom_ribbon(aes(x=year_100*-10, ymin=R_EXTINCTION_SPECIES-SD_R_EXTINCTION_SPECIES, 
  #                ymax=R_EXTINCTION_SPECIES+SD_R_EXTINCTION_SPECIES, 
  #                fill=evo_type, 
  #                linetype=factor(directional_speed)), alpha=0.3)+
  geom_line(aes(x=year_100*-0.1, y=R_EXTINCTION_SPECIES, color=evo_type, 
                linetype=factor(directional_speed)))+
  #geom_text(data=d_se_all[outlier=="F" & year==0], aes(x=1, y=R_SPECIATION_SPECIES, label=label, color=evo_type), 
  #          hjust="left", vjust="middle", size=5)+
  labs(x="Xk years before present", y="Number of extinction per 1k species", 
       color="Evolution type", linetype="Evolution rate")+
  theme_bw()+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  scale_y_continuous(breaks=c(0, 0.3, 0.6, 0.9, 1.2), labels=c(0, 0.3, 0.6, 0.9, 1.2))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")
p2
d_se_all[species_evo_type %in% c(5, 6, 7)]$directional_speed<-0
p3<-ggplot(d_se_all[outlier=="F" & year<=1000])+
  #geom_ribbon(aes(x=year_100*-10, ymin=R_EXTINCTION_SPECIES-SD_R_EXTINCTION_SPECIES, 
  #                ymax=R_EXTINCTION_SPECIES+SD_R_EXTINCTION_SPECIES, 
  #                fill=evo_type, 
  #                linetype=factor(directional_speed)), alpha=0.3)+
  geom_line(aes(x=year*-0.1, y=N_SPECIES, color=evo_type, 
                linetype=factor(directional_speed)))+
  #geom_text(data=d_se_all[outlier=="F" & year==0], aes(x=1, y=R_SPECIATION_SPECIES, label=label, color=evo_type), 
  #          hjust="left", vjust="middle", size=5)+
  labs(x="Xk years before present", y="Number of species", 
       color="Evolution type", linetype="Evolution rate")+
  theme_bw()+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  scale_y_continuous(breaks=c(0, 20000, 40000, 60000, 80000), labels=c("0k", "20k", "40k", "60k", "80k"))+
  theme(legend.position = c(0.3, 0.51), legend.box="horizontal")
  #theme(legend.position = "none")
p3
p<-ggarrange(plotlist=list(p1, p2, p3), nrow=3, ncol=1, common.legend = F)
ggsave(p, filename="../Figures/Figure1.Speciation.Extinction.Rate/Figure1.Speciation.Extinction.Rate.png", width=8, height=12)

#BY NB and DA

d_se_all<-d_se[, .(N_SPECIES=sum(N_SPECIES),
                   N_SPECIATION=sum(N_SPECIATION),
                   N_EXTINCTION=sum(N_EXTINCTION),
                   N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
                   N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
                   R_SPECIATION_SPECIES=mean(R_SPECIATION_SPECIES) * 1000,
                   R_EXTINCTION_SPECIES=mean(R_EXTINCTION_SPECIES) * 1000),
               by=list(species_evo_level, species_evo_type, 
                       directional_speed, year, evo_type, outlier, nb, da)]
cor(d_se$N_SPECIES, d_se$N_SPECIATION)
hist(d_se_all$R_SPECIATION_SPECIES)
d_se_all$label<-sprintf("%s (%s)", d_se_all$evo_type, as.character(d_se_all$directional_speed))

d_se_all_100<-d_se_all
d_se_all_100$year_100<-floor(d_se_all_100$year/100) * 100
d_se_all_100<-d_se_all_100[, .(N_SPECIES=sum(N_SPECIES),
                               N_SPECIATION=sum(N_SPECIATION),
                               N_EXTINCTION=sum(N_EXTINCTION),
                               N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
                               N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
                               R_SPECIATION_SPECIES=mean(R_SPECIATION_SPECIES),
                               R_EXTINCTION_SPECIES=mean(R_EXTINCTION_SPECIES),
                               SD_R_SPECIATION_SPECIES=sd(R_SPECIATION_SPECIES),
                               SD_R_EXTINCTION_SPECIES=sd(R_EXTINCTION_SPECIES)),
                           by=list(species_evo_level, species_evo_type, 
                                   directional_speed, year_100, evo_type, outlier, nb, da)]
hist(d_se_all_100$R_SPECIATION_SPECIES)
d_se_all_100[species_evo_type %in% c(5, 6, 7)]$directional_speed<-0
p1<-ggplot(d_se_all_100[outlier=="F" & year_100<=1000])+
  #geom_ribbon(aes(x=year_100*-10, ymin=R_SPECIATION_SPECIES-SD_R_SPECIATION_SPECIES, 
  #                ymax=R_SPECIATION_SPECIES+SD_R_SPECIATION_SPECIES, 
  #                fill=evo_type, 
  #                linetype=factor(directional_speed)), alpha=0.3)+
  geom_line(aes(x=year_100*-0.1, y=R_SPECIATION_SPECIES, color=evo_type, 
                linetype=factor(directional_speed)))+
  #geom_text(data=d_se_all[outlier=="F" & year==0], aes(x=1, y=R_SPECIATION_SPECIES, label=label, color=evo_type), 
  #          hjust="left", vjust="middle", size=5)+
  labs(x="Xk years before present", y="Number of speciation per 1k species", 
       color="Evolution type", linetype="Evolution rate")+
  theme_bw()+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  scale_y_continuous(breaks=c(0, 0.5, 1, 1.5, 2, 2.5), labels=c("0", "0.5", "1.0", "1.5", "2.0", "2.5"))+
  facet_grid(da~nb)
p1
ggsave(p1, filename="../Figures/Figure1.Speciation.Extinction.Rate/FigureS1.Speciation.Rate.png", width=8, height=6)

p2<-ggplot(d_se_all_100[outlier=="F" & year_100<=1000])+
  #geom_ribbon(aes(x=year_100*-10, ymin=R_EXTINCTION_SPECIES-SD_R_EXTINCTION_SPECIES, 
  #                ymax=R_EXTINCTION_SPECIES+SD_R_EXTINCTION_SPECIES, 
  #                fill=evo_type, 
  #                linetype=factor(directional_speed)), alpha=0.3)+
  geom_line(aes(x=year_100*-0.1, y=R_EXTINCTION_SPECIES, color=evo_type, 
                linetype=factor(directional_speed)))+
  #geom_text(data=d_se_all[outlier=="F" & year==0], aes(x=1, y=R_SPECIATION_SPECIES, label=label, color=evo_type), 
  #          hjust="left", vjust="middle", size=5)+
  labs(x="Xk years before present", y="Number of extinction per 1k species", 
       color="Evolution type", linetype="Evolution rate")+
  theme_bw()+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  scale_y_continuous(breaks=c(0, 0.3, 0.6, 0.9, 1.2), labels=c(0, 0.3, 0.6, 0.9, 1.2))+
  facet_grid(da~nb)
p2
ggsave(p2, filename="../Figures/Figure1.Speciation.Extinction.Rate/FigureS1.Extinction.Rate.png", width=8, height=6)


d_se_all[species_evo_type %in% c(5, 6, 7)]$directional_speed<-0
p3<-ggplot(d_se_all[outlier=="F" & year<=1000])+
  #geom_ribbon(aes(x=year_100*-10, ymin=R_EXTINCTION_SPECIES-SD_R_EXTINCTION_SPECIES, 
  #                ymax=R_EXTINCTION_SPECIES+SD_R_EXTINCTION_SPECIES, 
  #                fill=evo_type, 
  #                linetype=factor(directional_speed)), alpha=0.3)+
  geom_line(aes(x=year*-0.1, y=N_SPECIES, color=evo_type, 
                linetype=factor(directional_speed)))+
  #geom_text(data=d_se_all[outlier=="F" & year==0], aes(x=1, y=R_SPECIATION_SPECIES, label=label, color=evo_type), 
  #          hjust="left", vjust="middle", size=5)+
  labs(x="Xk years before present", y="Number of species", 
       color="Evolution type", linetype="Evolution rate")+
  theme_bw()+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  scale_y_continuous(breaks=c(0, 20000, 40000, 60000, 80000), labels=c("0k", "20k", "40k", "60k", "80k"))+
  facet_grid(da~nb)
#theme(legend.position = "none")
ggsave(p3, filename="../Figures/Figure1.Speciation.Extinction.Rate/FigureS1.N_Species.png", width=8, height=6)


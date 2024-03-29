library(ggplot2)
library(ggthemes)
library(data.table)
library(ggpubr)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
if (F){
  d<-readRDS("../Data/N_speciation_extinction/N_speciation_extinction.rda")
  #cols<-c("nb", "da", "global_id", "species_evo_type", "directional_speed", "species_evo_level")
  #d[, min_year:=min(year), by=cols]
  
  #d_unique<-unique(d[, ..cols])
  d$outlier<-"F"
  outliers<-readRDS("../Data/outliers/outliers_3SD.rda")
  d[global_id %in% unique(outliers$global_id)]$outlier<-"T"
  d_end<-d[year==0]
  d_end<-d_end[((directional_speed %in% c(0) & species_evo_type==1) |
                  (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                  (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7))) &
                 outlier=="F"]
  d_end$evo_type<-format_evoType(d_end$species_evo_type)
  d_end$label<-sprintf("%s (%s)", d_end$evo_type, d_end$directional_speed)
  d_end[species_evo_type %in% c(1, 5, 6, 7)]$label<-d_end[species_evo_type %in% c(1, 5, 6, 7)]$evo_type
  saveRDS(d_end, "../Figures/Figure2.N_Speciation.Extinction.Species/figure_data_raw.rda")
  d_se_all<-d_end[, .(N_SPECIES=sum(N_SPECIES),
                     N_SPECIATION=sum(N_SPECIATION),
                     N_EXTINCTION=sum(N_EXTINCTION)
                     ),
                 by=list(species_evo_level, species_evo_type, 
                         directional_speed, year, evo_type, outlier, da, nb, label)]
  d_se_all$label<-factor(d_se_all$label, levels=c("conservatism", 
                                                  "shift-directional (0.1)", "shift-directional (0.5)",
                                                  "expansion-directional (0.1)", "expansion-directional (0.5)",
                                                  "expansion-omnidirectional (0.1)", "expansion-omnidirectional (0.5)",
                                                  "random-central", "random-symmetrical", "random-asymmetrical"))
  saveRDS(d_se_all, "../Figures/Figure2.N_Speciation.Extinction.Species/figure_data.rda")
}
d_se_all<-readRDS("../Figures/Figure2.N_Speciation.Extinction.Species/figure_data.rda")
source("commons/functions.r")

p1<-ggplot(d_se_all[outlier=="F"], aes(x=label, y=N_SPECIATION))+
  geom_boxplot()+geom_point(aes(color=nb, shape=da))+
  labs(x="", y="Average number of speciation", 
       color="Niche breadth", shape="Dispersal abilisy")+
  theme_bw()+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  rotate_x_text()+
  scale_y_continuous(breaks=c(0, 10000, 20000, 30000, 40000), labels=c("0k", "10k", "20k", "30k", "40k"))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")
p1

p2<-ggplot(d_se_all[outlier=="F"], aes(x=label, y=N_EXTINCTION))+
  geom_boxplot()+geom_point(aes(color=nb, shape=da))+
  labs(x="", y="Average number of extinction", 
       color="Niche breadth", shape="Dispersal abilisy")+
  theme_bw()+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  rotate_x_text()+
  scale_y_continuous(breaks=c(0, 1000, 2000, 3000, 4000), labels=c("0k", "1k", "2k", "3k", "4k"))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")
p2

p3<-ggplot(d_se_all[outlier=="F"], aes(x=label, y=N_SPECIES))+
  geom_boxplot()+geom_point(aes(color=nb, shape=da))+
  labs(x="", y="Average number of species", 
       color="Niche breadth", shape="Dispersal abilisy")+
  theme_bw()+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  rotate_x_text()+
  scale_y_continuous(breaks=c(0, 10000, 20000, 30000, 40000), labels=c("0k", "10k", "20k", "30k", "40k"))+
  theme(legend.position = c(0.8, 0.8), legend.box="horizontal")
p3
p<-ggarrange(plotlist=list(ggarrange(plotlist=list(p1, p2), nrow=2, ncol=1), p3), 
             nrow=2, ncol=1, common.legend = F)
p
ggsave(p, filename="../Figures/Figure2.N_Speciation.Extinction.Species/Figure2.N_Speciation.Extinction.Species.png", 
       width=8, height=12)


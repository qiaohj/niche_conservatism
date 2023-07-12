library(ggplot2)
library(ggthemes)
library(data.table)
library(ggpubr)
library(ggh4x)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")

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
  saveRDS(d_end, "../Figures/Figure3.SP.EX.SPECIES/figure_data_raw.rda")
  d_se_all<-d_end[, .(N_SPECIES=mean(N_SPECIES),
                      N_SPECIATION=mean(N_SPECIATION),
                      N_EXTINCTION=mean(N_EXTINCTION),
                      sd_N_SPECIES=sd(N_SPECIES),
                      sd_N_SPECIATION=sd(N_SPECIATION),
                      sd_N_EXTINCTION=sd(N_EXTINCTION),
                      CI_N_SPECIES=my_CI(N_SPECIES),
                      CI_N_SPECIATION=my_CI(N_SPECIATION),
                      CI_N_EXTINCTION=my_CI(N_EXTINCTION)
                      
                      
  ),
  by=list(species_evo_level, species_evo_type, 
          directional_speed, year, evo_type, outlier, da, nb, label)]
  d_se_all$label<-factor(d_se_all$label, levels=c("conservatism", 
                                                  "shift-directional (0.1)", "shift-directional (0.5)",
                                                  "expansion-directional (0.1)", "expansion-directional (0.5)",
                                                  "expansion-omnidirectional (0.1)", "expansion-omnidirectional (0.5)",
                                                  "random-central", "random-symmetrical", "random-asymmetrical"))
  
  d_se_all2<-d_end[, .(N_SPECIES=mean(N_SPECIES),
                      N_SPECIATION=mean(N_SPECIATION),
                      N_EXTINCTION=mean(N_EXTINCTION),
                      sd_N_SPECIES=sd(N_SPECIES),
                      sd_N_SPECIATION=sd(N_SPECIATION),
                      sd_N_EXTINCTION=sd(N_EXTINCTION),
                      CI_N_SPECIES=my_CI(N_SPECIES),
                      CI_N_SPECIATION=my_CI(N_SPECIATION),
                      CI_N_EXTINCTION=my_CI(N_EXTINCTION)
                      
                      
  ),
  by=list(species_evo_level, species_evo_type, 
          directional_speed, year, evo_type, outlier, label)]
  d_se_all2$label<-factor(d_se_all2$label, levels=c("conservatism", 
                                                  "shift-directional (0.1)", "shift-directional (0.5)",
                                                  "expansion-directional (0.1)", "expansion-directional (0.5)",
                                                  "expansion-omnidirectional (0.1)", "expansion-omnidirectional (0.5)",
                                                  "random-central", "random-symmetrical", "random-asymmetrical"))
  
  saveRDS(rbindlist(list(d_se_all, d_se_all2), fill=T), "../Figures/Figure3.SP.EX.SPECIES/figure_data.rda")
}
d<-readRDS("../Figures/Figure3.SP.EX.SPECIES/figure_data_raw.rda")
d<-formatLabels(d)
d_se_all<-readRDS("../Figures/Figure3.SP.EX.SPECIES/figure_data.rda")
d_se_all<-formatLabels(d_se_all)
d_se_all[is.na(nb)]$nb<-"ALL"
d_se_all[is.na(da)]$da<-"ALL"
p1<-ggplot(d_se_all, aes(x=label_x, y=N_SPECIATION))+
  geom_errorbar(aes(color=nb, width=0.3, fill=da,
                    ymin=N_SPECIATION-CI_N_SPECIATION, 
                    ymax=N_SPECIATION+CI_N_SPECIATION), 
                position=position_dodge(width=0.5))+
  geom_point(aes(color=nb, shape=da), position=position_dodge(width=0.5), size=2)+
  labs(x="", y="Average number of speciations", 
       color="Niche breadth", shape="Dispersal ability")+
  theme_bw()+
  scale_color_manual(values=c("ALL"="#000000",
                              "BROAD"="#EE6677",
                              "NARROW"="#4477AA")
                     )+
  
  scale_shape_manual(values=c("ALL"=0, "GOOD"=1, "POOR"=4))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")
p1

p2<-ggplot(d_se_all, aes(x=label_x, y=N_EXTINCTION))+
  geom_errorbar(aes(color=nb, width=0.3, fill=da,
                    ymin=N_EXTINCTION-CI_N_EXTINCTION, 
                    ymax=N_EXTINCTION+CI_N_EXTINCTION), 
                position=position_dodge(width=0.5))+
  geom_point(aes(color=nb, shape=da), position=position_dodge(width=0.5), size=2)+
  labs(x="", y="Average number of extinctions", 
       color="Niche breadth", shape="Dispersal ability")+
  theme_bw()+
  scale_color_manual(values=c("ALL"="#000000",
                              "BROAD"="#EE6677",
                              "NARROW"="#4477AA")
  )+
  
  scale_shape_manual(values=c("ALL"=0, "GOOD"=1, "POOR"=4))+
  theme(legend.position = c(0.9, 0.8), 
        legend.box="horizontal",
        legend.background = element_rect(fill=bg),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())
p2


p3<-ggplot(d_se_all, aes(x=label_x, y=N_SPECIES))+
  geom_errorbar(aes(color=nb, width=0.3, fill=da,
                    ymin=N_SPECIES-CI_N_SPECIES, 
                    ymax=N_SPECIES+CI_N_SPECIES), 
                position=position_dodge(width=0.5))+
  geom_point(aes(color=nb, shape=da), position=position_dodge(width=0.5), size=2)+
  labs(x="", y="Average number of species", 
       color="Niche breadth", shape="Dispersal ability")+
  theme_bw()+
  scale_color_manual(values=c("ALL"="#000000",
                              "BROAD"="#EE6677",
                              "NARROW"="#4477AA")
  )+
  guides(x = guide_axis_nested(delim = "&"))+
  scale_shape_manual(values=c("ALL"=0, "GOOD"=1, "POOR"=4))+
  theme(ggh4x.axis.nestline.x = element_line(linewidth = 1.1),
        ggh4x.axis.nesttext.x = element_text(size = 10, face ="bold"),
        legend.position = "none")
p3



p<-ggarrange(plotlist=list(ggarrange(plotlist=list(p1, p2), nrow=2, ncol=1), p3), 
             nrow=2, ncol=1, common.legend = F, heights=c(1.7, 1))
p
ggsave(p, filename="../Figures/Figure3.SP.EX.SPECIES/N_Speciation.Extinction.Species.png", 
       width=14, height=10)

ggsave(p, filename="../Figures/Figure3.SP.EX.SPECIES/N_Speciation.Extinction.Species.pdf", 
       width=13, height=10)


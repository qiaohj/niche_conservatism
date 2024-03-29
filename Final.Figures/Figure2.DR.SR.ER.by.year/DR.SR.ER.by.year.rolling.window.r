library(ggplot2)
library(ggthemes)
library(data.table)
library(ggpubr)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")

d<-readRDS("../Data/tslm_and_glm/d_ndr.rda")

coms<-data.table(expand.grid(nb=c(NA, "NB"), da=c(NA, "DA")))
i=2
var="N_GROUP"
for (i in c(1:nrow(coms))){
  com<-coms[i]
  print(i)
  if (is.na(com$nb) & is.na(com$da)){
    d_se<-d[, .(R_SPECIATION_SPECIES=mean(R_SPECIATION_SPECIES),
                sd_R_SPECIATION_SPECIES=sd(R_SPECIATION_SPECIES),
                R_EXTINCTION_SPECIES=mean(R_EXTINCTION_SPECIES),
                sd_R_EXTINCTION_SPECIES=sd(R_EXTINCTION_SPECIES),
                net_dr=mean(net_dr),
                sd_net_dr=sd(net_dr)),
            by=list(from, to, evo_type, species_evo_type, directional_speed, label)]
  }
  if (is.na(com$nb) & !is.na(com$da)){
    d_se<-d[, .(R_SPECIATION_SPECIES=mean(R_SPECIATION_SPECIES),
                sd_R_SPECIATION_SPECIES=sd(R_SPECIATION_SPECIES),
                R_EXTINCTION_SPECIES=mean(R_EXTINCTION_SPECIES),
                sd_R_EXTINCTION_SPECIES=sd(R_EXTINCTION_SPECIES),
                net_dr=mean(net_dr),
                sd_net_dr=sd(net_dr)),
            by=list(from, to, evo_type, species_evo_type, directional_speed, label, da)]
  }
  
  if (!is.na(com$nb) & is.na(com$da)){
    d_se<-d[, .(R_SPECIATION_SPECIES=mean(R_SPECIATION_SPECIES),
                sd_R_SPECIATION_SPECIES=sd(R_SPECIATION_SPECIES),
                R_EXTINCTION_SPECIES=mean(R_EXTINCTION_SPECIES),
                sd_R_EXTINCTION_SPECIES=sd(R_EXTINCTION_SPECIES),
                net_dr=mean(net_dr),
                sd_net_dr=sd(net_dr)),
            by=list(from, to, evo_type, species_evo_type, directional_speed, label, nb)]
  }
  if (!is.na(com$nb) & !is.na(com$da)){
    d_se<-d[, .(R_SPECIATION_SPECIES=mean(R_SPECIATION_SPECIES),
                sd_R_SPECIATION_SPECIES=sd(R_SPECIATION_SPECIES),
                R_EXTINCTION_SPECIES=mean(R_EXTINCTION_SPECIES),
                sd_R_EXTINCTION_SPECIES=sd(R_EXTINCTION_SPECIES),
                net_dr=mean(net_dr),
                sd_net_dr=sd(net_dr)),
            by=list(from, to, evo_type, species_evo_type, directional_speed, label, nb, da)]
  }
  
  d_se<-formatLabels(d_se)
  p1<-ggplot(d_se)+geom_line(aes(x=to/10, y=R_SPECIATION_SPECIES, 
                                 color=evo_types_label_color, 
                                 linetype=evo_line_type,
                                 group=label))+
    scale_color_manual(values=evo_type_color, breaks=evo_types_label_color)+
    scale_linetype_manual(values=c(1, 2),
                          breaks=c("0.5*", "0.1"))+
    labs(x="K years before present", y="Number of speciestion per 1k species",
         color="Evolution type", linetype="Change percentage")+
    theme_bw()+
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())
  
  
  p2<-ggplot(d_se)+geom_line(aes(x=to/10, y=R_EXTINCTION_SPECIES, 
                                 color=evo_types_label_color, 
                                 linetype=evo_line_type,
                                 group=label))+
    scale_color_manual(values=evo_type_color, breaks=evo_types_label_color)+
    scale_linetype_manual(values=c(1, 2),
                          breaks=c("0.5*", "0.1"))+
    labs(x="K years before present", y="Number of extinction per 1k species",
         color="Evolution type", linetype="Change percentage")+
    theme_bw()+
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())
  p3<-ggplot(d_se)+geom_line(aes(x=to/10, y=net_dr, 
                                 color=evo_types_label_color, 
                                 linetype=evo_line_type,
                                 group=label))+
    scale_color_manual(values=evo_type_color, breaks=evo_types_label_color)+
    scale_linetype_manual(values=c(1, 2),
                          breaks=c("0.5*", "0.1"))+
    labs(x="K years before present", y="Net per capita diversification rate",
         color="Evolution type", linetype="Change percentage")+
    theme_bw()
  width<-10
  height<-9
  if (is.na(com$nb) & !is.na(com$da)){
    p1<-p1+facet_wrap(~da, nrow=1)
    p2<-p2+facet_wrap(~da, nrow=1)
    p3<-p3+facet_wrap(~da, nrow=1)
    width<-12
    height<-9
  }
  if (!is.na(com$nb) & is.na(com$da)){
    p1<-p1+facet_wrap(~nb, nrow=1)
    p2<-p2+facet_wrap(~nb, nrow=1)
    p3<-p3+facet_wrap(~nb, nrow=1)
    width<-12
    height<-9
  }
  if (!is.na(com$nb) & !is.na(com$da)){
    p1<-p1+facet_grid(da~nb)
    p2<-p2+facet_grid(da~nb)
    p3<-p3+facet_grid(da~nb)
    width<-12
    height<-15
    
  }
  p<-ggarrange(plotlist=list(p1, p2, p3), nrow=3, ncol=1, common.legend = T, legend = "right")
  
  ggsave(p, filename=sprintf("../Figures/Figure2.DR.SR.ER.by.year/Figure2.DR.SR.ER.by.year.%s.%s.png", com$nb, com$da), 
         width=width, height=height, bg = "white")
  
  
}

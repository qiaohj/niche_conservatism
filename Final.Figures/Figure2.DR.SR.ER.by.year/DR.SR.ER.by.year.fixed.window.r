library(ggplot2)
library(ggthemes)
library(data.table)
library(ggpubr)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")

d<-readRDS("../Data/tslm_and_glm/d_ndr.rda")
d<-d[floor((d$to+0)/100)==(d$to+0)/100]
coms<-data.table(expand.grid(nb=c(NA, "NB"), da=c(NA, "DA")))
i=1
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
  p1<-ggplot(d_se)+geom_line(aes(x=to/10, y=R_SPECIATION_SPECIES/1000, 
                                 color=label_line, 
                                 linetype=label_line))+
    scale_color_manual("Evolution scenario", values=evo_type_color)+
    scale_linetype_manual("Evolution scenario", 
                          values=evo_type_line)+
    labs(x=x_label, y="net speciestions")+
    theme_bw()+
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank())
  
  p1
  p2<-ggplot(d_se)+geom_line(aes(x=to/10, y=R_EXTINCTION_SPECIES/1000, 
                                 color=label_line, 
                                 linetype=label_line))+
    scale_color_manual("Evolution scenario", values=evo_type_color)+
    scale_linetype_manual("Evolution scenario", values=evo_type_line)+
    labs(x=x_label, y="net extinctions")+
    theme_bw()+
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank())
  p2
  p3<-ggplot(d_se)+geom_line(aes(x=to/10, y=net_dr, 
                                 color=label_line, 
                                 linetype=label_line,
                                 group=label))+
    scale_color_manual("Evolution scenario", values=evo_type_color)+
    scale_linetype_manual("Evolution scenario", values=evo_type_line)+
    labs(x=x_label, y="Net per capita diversification rate",
         linetype="", color="Evolution scenario")+
    theme_bw()+
    theme(legend.title = element_blank())
  p3
  width<-12
  height<-9
  if (is.na(com$nb) & !is.na(com$da)){
    p1<-p1+facet_wrap(~da, nrow=1)
    p2<-p2+facet_wrap(~da, nrow=1)
    p3<-p3+facet_wrap(~da, nrow=1)
    width<-14
    height<-9
  }
  if (!is.na(com$nb) & is.na(com$da)){
    p1<-p1+facet_wrap(~nb, nrow=1)
    p2<-p2+facet_wrap(~nb, nrow=1)
    p3<-p3+facet_wrap(~nb, nrow=1)
    width<-14
    height<-9
  }
  if (!is.na(com$nb) & !is.na(com$da)){
    p1<-p1+facet_grid(da~nb)
    p2<-p2+facet_grid(da~nb)
    p3<-p3+facet_grid(da~nb)
    width<-14
    height<-15
    
  }
  p<-ggarrange(plotlist=list(p1, p2, p3), nrow=3, ncol=1, common.legend = T, legend = "bottom")
  p
  ggsave(p, filename=sprintf("../Figures/Figure2.DR.SR.ER.by.year/fixed.window.Figure2.DR.SR.ER.by.year.%s.%s.png", com$nb, com$da), 
         width=width, height=height, bg = "white")
  
  ggsave(p, filename=sprintf("../Figures/Figure2.DR.SR.ER.by.year/fixed.window.Figure2.DR.SR.ER.by.year.%s.%s.pdf", com$nb, com$da), 
         width=width, height=height, bg = "white")
}

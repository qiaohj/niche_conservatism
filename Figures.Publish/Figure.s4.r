library(ggplot2)
library(ggthemes)
library(data.table)
library(ggpubr)
library(ggh4x)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")

d<-readRDS("../Figures/Figure3.SP.EX.SPECIES/figure_data_raw.rda")
d<-formatLabels(d)
d_se_all<-readRDS("../Figures/Figure3.SP.EX.SPECIES/figure_data.rda")
d_se_all<-formatLabels(d_se_all)
d_se_all[is.na(nb)]$nb<-"ALL"
d_se_all[is.na(da)]$da<-"ALL"
fwrite(d_se_all, "../Figures.Publish/Data/Figure.S4/Figure.S4.csv")
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

unique(d_se_all[, c("da", "nb", "N")])
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

#ggsave(p, filename="../Figures.Publish/Figures/Figure.S4/Figure.S4.pdf", 
#       width=13, height=10)


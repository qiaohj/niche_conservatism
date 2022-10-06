library(data.table)
library(ggplot2)
library(ggthemes)
library(RSQLite)
library(DBI)
library(sf)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
d_se<-readRDS("../Data/distribution_traits_se.rda")
d_se$evo_type<-format_evoType(d_se$species_evo_type)
d_se$label<-format_evoType_amplitude(d_se$evo_type, d_se$directional_speed, order=-1)

p<-ggplot(d_se[species_evo_level==0 & year==0])+
  geom_point(aes(x=N_CELLS, y=label, color=nb, shape=da), 
             position=position_dodge(0.9))+
  geom_errorbarh(aes(xmin=N_CELLS-SD_N_CELLS, 
                     xmax=N_CELLS+SD_N_CELLS, 
                     y=label, color=nb, linetype=da), 
                 position=position_dodge(width = 0.9))+
  #facet_grid(nb~da, scale="free")+
  theme_bw()+
  labs(y="", x="Number of cells", color="Niche breadth",
       shape="Dispersal ability", linetype="Dispersal ability")+
  scale_y_discrete(labels = scales::label_wrap(10))+
  scale_color_manual(values=colorBlindGrey8[6:7])+
  theme(axis.text.y = element_text(size=10),
        legend.position = c(0.9, 0.8),
        legend.background = element_rect(fill=bg))
#scale_fill_colorblind()
#scale_y_log10()
p
ggsave(p, filename="../Figures/Figure3/fig.3.png", width=12, height=6)


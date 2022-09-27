library(data.table)
library(ggplot2)
setwd("~/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
d<-readRDS("../Data/niche_traits_se.rda")

d$evo_type<-format_evoType(d$species_evo_type)
d$label<-format_evoType_amplitude(d$evo_type, d$directional_speed, order=-1)
d

ggplot(d[species_evo_level==0 & year==1200])+
  geom_point(aes(x=V_range, y=label, color=nb, shape=da))+
  facet_wrap(~V_L, scale="free")

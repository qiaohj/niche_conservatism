library(data.table)
library(ggplot2)
setwd("~/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
d<-readRDS("../Data/niche_traits_se.rda")

d$evo_type<-format_evoType(d$species_evo_type)
d$label<-format_evoType_amplitude(d$evo_type, d$directional_speed, order=1)
d$label<-format_evoType_amplitude(d$evo_type, d$directional_speed, order=1)
d$VAR<-d$V_L
unique(d$V_L)
d[VAR=="Debiased_Maximum_Monthly_Temperature"]$VAR<-"Maximum Monthly Temperature"
d[VAR=="Debiased_Minimum_Monthly_Temperature"]$VAR<-"Minimum Monthly Temperature"
d[VAR=="Debiased_Maximum_Monthly_Precipitation"]$VAR<-"Maximum Monthly Precipitation"
df_fn<-data.table(yintercept=c(5, 10, 40, 60, 40, 60), VAR=c("Maximum Monthly Precipitation", "Maximum Monthly Precipitation",
                                                     "Maximum Monthly Temperature", "Maximum Monthly Temperature",
                                                     "Minimum Monthly Temperature", "Minimum Monthly Temperature"))

p<-ggplot(d[species_evo_level==0 & year==1200])+
  geom_hline(data=df_fn, aes(yintercept = yintercept), linetype=2, alpha=0.4)+
  geom_point(aes(y=V_range, x=label, color=nb, shape=da, group=da), position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=V_range-sd_V_range, ymax=V_range+sd_V_range, x=label, color=nb, group=da),
                position=position_dodge(0.5), width=0.2)+
  
  labs(shape="Dispersal ability", color="Niche breadth", x="", y="Niche breadth")+
  scale_color_colorblind()+
  facet_wrap(~VAR, scale="free_y", nrow=3)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=-90, hjust=0, vjust=0))
p
ggsave("../Figures/Niche_Traits/FN_end.png", width=8, height=8)

p<-ggplot(d[species_evo_level==0])+
  #geom_hline(data=df_fn, aes(yintercept = yintercept), linetype=2, alpha=0.4)+
  geom_line(aes(y=V_range, x=(year-1200)/10, color=label))+
  labs(color="Evolution type",  y="Niche breadth", x="X k year before present")+
  facet_grid(VAR~nb+da, scale="free")+
  theme_bw()
p
ggsave("../Figures/Niche_Traits/FN_all.png", width=12, height=8)

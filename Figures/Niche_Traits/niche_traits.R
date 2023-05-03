library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
d<-readRDS("../Data/niche_traits/niche_traits_fn_se_without_outliers_3SD.rda")

d$evo_type<-format_evoType(d$species_evo_type)
d$label<-format_evoType_amplitude(d$evo_type, d$directional_speed, order=1)
d$label<-format_evoType_amplitude(d$evo_type, d$directional_speed, order=1)
d$VAR<-d$V_L
unique(d$V_L)
d[VAR=="Debiased_Maximum_Monthly_Temperature"]$VAR<-"Temperature"
d[VAR=="Debiased_Minimum_Monthly_Temperature"]$VAR<-"Temperature"
d[VAR=="Debiased_Maximum_Monthly_Precipitation"]$VAR<-"Precipitation"
d<-d[, .(V_range=mean(V_range), sd_V_range=mean(sd_V_range)),
     by=list(year, evo_type, nb, da, species_evo_type, directional_speed,
             species_evo_level, label, VAR)]
df_fn<-data.table(yintercept=c(5, 10, 40, 60), VAR=c("Precipitation", "Precipitation",
                                                     "Temperature", "Temperature"))
d_gg<-d[((directional_speed %in% c(0) & species_evo_type==1) |
           (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
           (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)))]
p<-ggplot(d_gg[species_evo_level==0 & year==1200])+
  geom_hline(data=df_fn, aes(yintercept = yintercept), linetype=2, alpha=0.4)+
  geom_point(aes(y=V_range, x=label, color=nb, shape=da, group=da), position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=V_range-sd_V_range, ymax=V_range+sd_V_range, x=label, color=nb, group=da),
                position=position_dodge(0.5), width=0.2)+
  
  labs(shape="Dispersal ability", color="Niche breadth", x="", y="Fundamental niche breadth")+
  scale_color_colorblind()+
  facet_wrap(~VAR, scale="free_y", nrow=3)+
  theme_bw()+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
  #theme(axis.text.x=element_text(angle=-90, hjust=0, vjust=0))
p
ggsave("../Figures/Niche_Traits/FN_end.png", width=12, height=6)

p<-ggplot(d_gg[species_evo_level==0])+
  #geom_hline(data=df_fn, aes(yintercept = yintercept), linetype=2, alpha=0.4)+
  geom_line(aes(y=V_range, x=(year-1200)/10, color=evo_type, linetype=factor(directional_speed)))+
  labs(color="Evolution type",  linetype="Evolution rate", y="Fundamental niche breadth", x="Xk year before present")+
  facet_grid(VAR~nb+da, scale="free")+
  theme_bw()+
  scale_color_colorblind()
p
ggsave("../Figures/Niche_Traits/FN_all.png", width=12, height=8)

#Realized niche
d<-readRDS("../Data/niche_traits/niche_traits_rn_se_without_outliers_3SD.rda")

d$evo_type<-format_evoType(d$species_evo_type)
d$label<-format_evoType_amplitude(d$evo_type, d$directional_speed, order=1)
d$label<-format_evoType_amplitude(d$evo_type, d$directional_speed, order=1)
d$VAR<-d$var
d[var=="TEMP"]$VAR<-"Temperature"
d[var=="PREC"]$VAR<-"Precipitation"
df_fn<-data.table(yintercept=c(5, 10, 40, 60), VAR=c("Precipitation", "Precipitation",
                                                     "Temperature", "Temperature"))
d_gg<-d[((directional_speed %in% c(0) & species_evo_type==1) |
           (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
           (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)))]
p<-ggplot(d_gg[species_evo_level==0 & year==0])+
  geom_hline(data=df_fn, aes(yintercept = yintercept), linetype=2, alpha=0.4)+
  geom_point(aes(y=V_range, x=label, color=nb, shape=da, group=da), position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=V_range-sd_V_range, ymax=V_range+sd_V_range, x=label, color=nb, group=da),
                position=position_dodge(0.5), width=0.2)+
  
  labs(shape="Dispersal ability", color="Niche breadth", x="", y="Realized niche breadth")+
  scale_color_colorblind()+
  facet_wrap(~VAR, scale="free_y", nrow=2)+
  theme_bw()+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
#theme(axis.text.x=element_text(angle=-90, hjust=0, vjust=0))
p
ggsave("../Figures/Niche_Traits/RN_end.png", width=12, height=6)

p<-ggplot(d_gg[species_evo_level==0])+
  #geom_hline(data=df_fn, aes(yintercept = yintercept), linetype=2, alpha=0.4)+
  geom_line(aes(y=V_range, x=year * -0.1, color=evo_type, linetype=factor(directional_speed)))+
  labs(color="Evolution type",  linetype="Evolution rate", y="Realized niche breadth", x="Xk year before present")+
  facet_grid(VAR~nb+da, scale="free")+
  theme_bw()+
  scale_color_colorblind()
p
ggsave("../Figures/Niche_Traits/RN_all.png", width=12, height=8)



library(data.table)
library(ggplot2)
library(ggtext)
library(ggh4x)
library(gridtext)
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

d_gg<-formatLabels(d_gg)
p<-ggplot(d_gg[species_evo_level==0 & year==1200])+
  geom_hline(data=df_fn, aes(yintercept = yintercept), linetype=2, alpha=0.4)+
  geom_point(aes(y=V_range, x=label_x, color=nb, shape=da, group=da), position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=V_range-sd_V_range, ymax=V_range+sd_V_range, x=label_x, color=nb, group=da),
                position=position_dodge(0.5), width=0.2)+
  scale_color_manual(values=c("#4477AA", "#EE6677"), breaks=c("BROAD", "NARROW"))+
  labs(shape="Dispersal ability", color="Niche breadth", x="", y="Niche breadth")+
  facet_wrap(~VAR, scale="free_y", nrow=3)+
  theme_bw()+
  guides(x = guide_axis_nested(delim = "&"))+
  theme(ggh4x.axis.nestline.x = element_line(linewidth = 1.1),
        ggh4x.axis.nesttext.x = element_text(size = 10, face ="bold"))

#theme(axis.text.x=element_text(angle=-90, hjust=0, vjust=0))
p
ggsave("../Figures/Figure5.NB.Change/FN_end.png", width=13.5, height=6)

p<-ggplot(d_gg[species_evo_level==0])+
  #geom_hline(data=df_fn, aes(yintercept = yintercept), linetype=2, alpha=0.4)+
  geom_line(aes(y=V_range, x=(year-1200)/10, color=evo_type, linetype=factor(directional_speed)))+
  labs(color="Evolution type",  linetype="Evolution rate", y="Fundamental niche breadth", x="Xk year before present")+
  facet_grid(VAR~nb+da, scale="free")+
  theme_bw()+
  scale_color_colorblind()
p
ggsave("../Figures/Figure5.NB.Change/FN_all.png", width=12, height=8)

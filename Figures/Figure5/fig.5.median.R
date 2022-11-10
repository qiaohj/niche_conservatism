library(data.table)
library(ggplot2)
library(ggthemes)
library(RSQLite)
library(DBI)
library(ggpubr)
library(sf)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")

source("commons/functions.r")
lat_se<-readRDS("../Data/lat_gradient_nb_da_last_year.rda")
lat_se$evo_type<-format_evoType(lat_se$species_evo_type)
lat_se$label<-format_evoType_amplitude(lat_se$evo_type, lat_se$directional_speed, order=1)
y<-1198
setorderv(lat_se, "mid")

lat_se<-readRDS("../Data/lat_gradient_all_last_year.rda")
lat_band_df<-readRDS("../Data/lat_band_n_land.rda")
lat_se_n_land<-merge(lat_se, lat_band_df, by=c("from", "to", "mid"))
lat_se_n_land$MEAN_Species<-lat_se_n_land$N_Species/lat_se_n_land$n_land


lat_se_n_land$evo_type<-format_evoType(lat_se_n_land$species_evo_type)
lat_se_n_land$label<-format_evoType_amplitude(lat_se_n_land$evo_type, lat_se_n_land$directional_speed, order=1)
y<-1198
setorderv(lat_se_n_land, "mid")
lat_se_n_land$group<-lat_se_n_land$evo_type
lat_se_n_land[grepl("random", evo_type)]$group<-"random"
lat_se_n_land_conservatism<-lat_se_n_land[group=="conservatism"]
lat_se_n_land_ratio<-lat_se_n_land[group %in% c("shift-directional", 
                                                "expansion-directional",
                                                "expansion-omnidirectional")]
lat_se_n_land_conservatism$group<-"shift-directional"
lat_se_n_land_ratio<-rbindlist(list(lat_se_n_land_ratio, lat_se_n_land_conservatism))

lat_se_n_land_conservatism$group<-"expansion-directional"
lat_se_n_land_ratio<-rbindlist(list(lat_se_n_land_ratio, lat_se_n_land_conservatism))

lat_se_n_land_conservatism$group<-"expansion-omnidirectional"
lat_se_n_land_ratio<-rbindlist(list(lat_se_n_land_ratio, lat_se_n_land_conservatism))

lat_se_n_land_random<-lat_se_n_land[group=="random"]
lat_se_n_land_random<-rbindlist(list(lat_se_n_land_random, lat_se_n_land_conservatism))

lat_se_n_land_ratio$directional_speed<-as.character(lat_se_n_land_ratio$directional_speed)
lat_se_n_land_ratio[directional_speed=="0"]$directional_speed<-"0"
unique(lat_se_n_land_ratio$directional_speed)
lat_se_n_land_ratio$directional_speed<-factor(lat_se_n_land_ratio$directional_speed,
                                              c("0", "0.01", "0.1", "0.5"))

p1<-ggplot(lat_se_n_land_ratio[species_evo_level==0 & year==0 & between(mid, -50, 65)])+
  
  #geom_point(aes(x=N_Species, y=mid, color=label))+
  geom_path(aes(x=MEDIAN_N_Species, y=mid, color=factor(directional_speed)), position="identity")+
  
  theme_bw()+
  labs(x="Number of species", y="Latitudinal band", color="Evolution ratio",
       fill="Evolution ratio")+
  facet_wrap(~group)+
  scale_color_manual(values=c("black", colorBlindGrey8[2:4]))+
  scale_x_continuous(breaks=c(40, 80, 120), labels=c(40, 80, 120))+
  theme(legend.position = c(0.25, 0.15),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0.01, 0, 0, 0.01), "null"),
        legend.background = element_rect(fill=bg))
p1

lat_se_n_land_random$group<-"random"
p2<-ggplot(lat_se_n_land_random[species_evo_level==0 & year==0 & between(mid, -50, 65)])+
  
  #geom_point(aes(x=N_Species, y=mid, color=label))+
  geom_path(aes(x=MEDIAN_N_Species, y=mid, color=evo_type), 
            position="identity")+
  geom_errorbarh(aes(xmin=QUANTILE_25_N_Species, xmax=QUANTILE_75_N_Species, y=mid, color=evo_type))+
  theme_bw()+
  labs(x="", y="Latitudinal band", color="Evolution type",
       fill="Evolution type")+
  facet_wrap(~group)+
  scale_x_continuous(breaks=c(40, 80, 120), labels=c(40, 80, 120))+
  scale_color_manual(values=c("black", colorBlindGrey8[5:7]))+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = c(0.65, 0.15),
        legend.background = element_rect(fill=bg),
        plot.margin = unit(c(0.01, 0.01, 0, 0.01), "null"))

ggarrange(p1, p2, widths=c(3,1))
p<-ggarrange(p1, p2, widths=c(3,1))
p<-annotate_figure(p, bottom = "Number of species")
p

ggsave(p, filename="../Figures/Figure5/fig.5.median.png",
       width=12, height=6, bg="white")

library(data.table)
library(ggplot2)
library(ggthemes)
library(RSQLite)
library(DBI)
library(sf)
library(ggpubr)
setDTthreads(20)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
if (F){
  outliers<-readRDS("../Data/outliers.rda")
  seeds<-read_sf("../Shape/ISEA3H8/seeds.shp")
  plot(seeds$geometry)
  plot(seeds[which(seeds$global_id %in% outliers),]$geometry, col="red", add=T)
}

n_splist_df<-readRDS("../Data/lat_gradient_bootstrapping_se.rda")
lat_se_n_land<-n_splist_df
setorderv(lat_se_n_land, "mid")
lat_se_n_land$evo_type<-format_evoType(lat_se_n_land$species_evo_type)
lat_se_n_land$label<-format_evoType_amplitude(lat_se_n_land$evo_type, lat_se_n_land$directional_speed, order=-1)


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
lat_band_n_land<-readRDS("../Data/lat_band_n_land.rda")
lat_se_n_land_ratio<-merge(lat_se_n_land_ratio, lat_band_n_land, by=c("from", "to", "mid"))
lat_se_n_land_ratio$N_Species_MEAN<-lat_se_n_land_ratio$N_Species/lat_se_n_land_ratio$n_land
p1<-ggplot(lat_se_n_land_ratio[species_evo_level==0 & year==0 & is_outlier==F])+
  
  #geom_point(aes(x=N_Species, y=mid, color=label))+
  geom_path(aes(x=N_Species, y=mid, color=factor(directional_speed)), position="identity")+
  #geom_errorbarh(aes(xmin=N_Species-N_Species_SD, xmax=N_Species+N_Species_SD, y=mid))+
  theme_bw()+
  labs(x="Number of species", y="Latitudinal band", color="Evolution ratio",
       fill="Evolution ratio")+
  facet_wrap(~group)+
  scale_color_manual(values=c("black", colorBlindGrey8[2:4]))+
  #scale_x_continuous(breaks=c(500, 1000, 1500, 2000), labels=c(500, 1000, 1500, 2000))+
  xlim(0, 18000)+
  theme(legend.position = c(0.25, 0.15),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0.01, 0, 0, 0.01), "null"),
        legend.background = element_rect(fill=bg))
p1

lat_se_n_land_random$group<-"random"
p2<-ggplot(lat_se_n_land_random[species_evo_level==0 & year==0 & is_outlier==F])+
  
  #geom_point(aes(x=N_Species, y=mid, color=label))+
  geom_path(aes(x=N_Species, y=mid, color=evo_type), 
            position="identity")+
  
  theme_bw()+
  labs(x="", y="Latitudinal band", color="Evolution type",
       fill="Evolution type")+
  facet_wrap(~group)+
  #scale_x_continuous(breaks=c(500, 1000, 1500, 2000), labels=c(500, 1000, 1500, 2000))+
  xlim(0, 18000)+
  scale_color_manual(values=c("black", colorBlindGrey8[5:7]))+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = c(0.65, 0.15),
        legend.background = element_rect(fill=bg),
        plot.margin = unit(c(0.01, 0.01, 0, 0.01), "null"))

p2
ggarrange(p1, p2, widths=c(3,1))
p<-ggarrange(p1, p2, widths=c(3,1))
#p
p<-annotate_figure(p, bottom = "Number of species")

ggsave(p, filename="../Figures/Figure5/fig.5_boot.png",
       width=12, height=6, bg="white")


#with outlier
n_splist_df<-n_splist_df[,.(N_Species=sum(N_Species)),
                         by=list(year, species_evo_type, directional_speed, species_evo_level,
                                 from, to, mid)]
lat_se_n_land<-n_splist_df
setorderv(lat_se_n_land, "mid")
lat_se_n_land$evo_type<-format_evoType(lat_se_n_land$species_evo_type)
lat_se_n_land$label<-format_evoType_amplitude(lat_se_n_land$evo_type, lat_se_n_land$directional_speed, order=-1)


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


p1<-ggplot(lat_se_n_land_ratio[species_evo_level==0 & year==0])+
  
  #geom_point(aes(x=N_Species, y=mid, color=label))+
  geom_path(aes(x=N_Species, y=mid, color=factor(directional_speed)), position="identity")+
  theme_bw()+
  labs(x="Number of species", y="Latitudinal band", color="Evolution ratio",
       fill="Evolution ratio")+
  facet_wrap(~group)+
  scale_color_manual(values=c("black", colorBlindGrey8[2:4]))+
  #scale_x_continuous(breaks=c(500, 1000, 1500, 2000), labels=c(500, 1000, 1500, 2000))+
  xlim(0, 25000)+
  theme(legend.position = c(0.25, 0.15),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0.01, 0, 0, 0.01), "null"),
        legend.background = element_rect(fill=bg))
p1

lat_se_n_land_random$group<-"random"
p2<-ggplot(lat_se_n_land_random[species_evo_level==0 & year==0])+
  
  #geom_point(aes(x=N_Species, y=mid, color=label))+
  geom_path(aes(x=N_Species, y=mid, color=evo_type), 
            position="identity")+
  
  theme_bw()+
  labs(x="", y="Latitudinal band", color="Evolution type",
       fill="Evolution type")+
  facet_wrap(~group)+
  #scale_x_continuous(breaks=c(500, 1000, 1500, 2000), labels=c(500, 1000, 1500, 2000))+
  xlim(0, 25000)+
  scale_color_manual(values=c("black", colorBlindGrey8[5:7]))+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = c(0.65, 0.15),
        legend.background = element_rect(fill=bg),
        plot.margin = unit(c(0.01, 0.01, 0, 0.01), "null"))

p2
ggarrange(p1, p2, widths=c(3,1))
p<-ggarrange(p1, p2, widths=c(3,1))
#p
p<-annotate_figure(p, bottom = "Number of species")

ggsave(p, filename="../Figures/Figure5/fig.5_boot_with_outliers.png",
       width=12, height=6, bg="white")


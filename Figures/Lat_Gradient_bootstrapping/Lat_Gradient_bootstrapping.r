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

n_splist_df<-readRDS("../Data/diversity/diversity_bootstrap.rda")
mask<-readRDS("../Data/mask_lonlat.rda")
n_splist_df<-merge(n_splist_df, mask, by="global_id")
n_splist_df$lat_band<-round(n_splist_df$lat/5) * 5
setorderv(n_splist_df, "lat_band")
n_splist_df$evo_type<-format_evoType(n_splist_df$species_evo_type)
n_splist_df$label<-format_evoType_amplitude(n_splist_df$evo_type, n_splist_df$directional_speed, order=-1)


n_splist_df$group<-n_splist_df$evo_type

n_splist_df[grepl("random", evo_type)]$group<-"random"
n_splist_df_conservatism<-n_splist_df[group=="conservatism"]
n_splist_df_ratio<-n_splist_df[group %in% c("shift-directional", 
                                                "expansion-directional",
                                                "expansion-omnidirectional")]
n_splist_df_conservatism$group<-"shift-directional"
n_splist_df_ratio<-rbindlist(list(n_splist_df_ratio, n_splist_df_conservatism))

n_splist_df_conservatism$group<-"expansion-directional"
n_splist_df_ratio<-rbindlist(list(n_splist_df_ratio, n_splist_df_conservatism))

n_splist_df_conservatism$group<-"expansion-omnidirectional"
n_splist_df_ratio<-rbindlist(list(n_splist_df_ratio, n_splist_df_conservatism))

n_splist_df_random<-n_splist_df[group=="random"]
n_splist_df_random<-rbindlist(list(n_splist_df_random, n_splist_df_conservatism))

n_splist_df_ratio$directional_speed<-as.character(n_splist_df_ratio$directional_speed)
n_splist_df_ratio[directional_speed=="0"]$directional_speed<-"0"
unique(n_splist_df_ratio$directional_speed)
n_splist_df_ratio$directional_speed<-factor(n_splist_df_ratio$directional_speed,
                                              c("0", "0.01", "0.1", "0.5"))
n_splist_df_ratio<-n_splist_df_ratio[((directional_speed %in% c(0) & species_evo_type==1) |
    (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
    (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)))]
n_splist_df_ratio_lat<-n_splist_df_ratio[, .(mean_N_SPECIES=mean(mean_N_SPECIES)),
                                         by=list(species_evo_type, directional_speed, group, lat_band, evo_type, label)]
p1<-ggplot(n_splist_df_ratio_lat)+
  
  #geom_point(aes(x=N_Species, y=mid, color=label))+
  geom_path(aes(x=mean_N_SPECIES, y=lat_band, color=factor(directional_speed)), position="identity")+
  #geom_errorbarh(aes(xmin=N_Species-N_Species_SD, xmax=N_Species+N_Species_SD, y=mid))+
  theme_bw()+
  labs(x="Number of species", y="Latitudinal band", color="Evolution ratio",
       fill="Evolution ratio")+
  facet_wrap(~group)+
  scale_color_manual(values=c("black", colorBlindGrey8[2:4]))+
  #scale_x_continuous(breaks=c(500, 1000, 1500, 2000), labels=c(500, 1000, 1500, 2000))+
  xlim(0, 2500)+
  theme(legend.position = c(0.25, 0.15),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0.01, 0, 0, 0.01), "null"),
        legend.background = element_rect(fill=bg))
p1

n_splist_df_random$group<-"random"
n_splist_df_random<-n_splist_df_random[((directional_speed %in% c(0) & species_evo_type==1) |
                                        (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                                        (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)))]
n_splist_df_random_lat<-n_splist_df_random[, .(mean_N_SPECIES=mean(mean_N_SPECIES)),
                                         by=list(species_evo_type, directional_speed, group, lat_band, evo_type, label)]

p2<-ggplot(n_splist_df_random_lat)+
  
  #geom_point(aes(x=N_Species, y=mid, color=label))+
  geom_path(aes(x=mean_N_SPECIES, y=lat_band, color=evo_type), 
            position="identity")+
  
  theme_bw()+
  labs(x="", y="Latitudinal band", color="Evolution type",
       fill="Evolution type")+
  facet_wrap(~group)+
  #scale_x_continuous(breaks=c(500, 1000, 1500, 2000), labels=c(500, 1000, 1500, 2000))+
  xlim(0, 2500)+
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

ggsave(p, filename="../Figures/Lat_gradient/Lat_gradient_boot.png",
       width=12, height=6, bg="white")




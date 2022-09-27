library(data.table)
library(ggplot2)
library(ggthemes)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
if (F){
  d<-readRDS("../Data/distribution_traits.rda")
  d
  d_se<-d[, .(N_CELLS=mean(N_CELLS),
              SD_N_CELLS=sd(N_CELLS)),
          by=list(year, species_evo_type, directional_speed, nb, da, species_evo_level)]
  saveRDS(d_se, "../Data/distribution_traits_se.rda")
  
  lat_bands<-seq(-87.5, 87.5, by=5)
  lat_band_label<-seq(-85, 85, by=5)
  lat_band_df<-data.table(from=lat_bands[1:(length(lat_bands)-1)], 
                          to=lat_bands[2:(length(lat_bands))],
                          mid=lat_band_label)
  d_sub<-list()
  i=18
  for (i in c(1:nrow(lat_band_df))){
    print(paste(i, nrow(lat_band_df)))
    lat_band_item<-lat_band_df[i]
    d_item<-d[between(MAX_LAT, lat_band_item$from, lat_band_item$to) |
                between(MIN_LAT, lat_band_item$from, lat_band_item$to) |
                (lat_band_item$from>=MIN_LAT & lat_band_item$from<=MAX_LAT) |
                (lat_band_item$to>=MIN_LAT & lat_band_item$to<=MAX_LAT)]
    if (nrow(d_item)==0){
      next()
    }
    n_sp<-d_item[,.(N_Species=length(unique(sp_id)),
                    N_CELLS=sum(N_CELLS)),
                 by=list(year, species_evo_type, directional_speed,
                         nb, da, species_evo_level, global_id)]
    n_sp$from<-lat_band_item$from
    n_sp$to<-lat_band_item$to
    n_sp$mid<-lat_band_item$mid
    d_sub[[length(d_sub)+1]]<-n_sp
  }
  d_sub<-rbindlist(d_sub)
  saveRDS(d_sub, "../Data/lat_gradient.rda")
  
  n_species<-d[, .(N_Species=length(unique(sp_id))),
               by=list(year, species_evo_type, directional_speed,
                       nb, da, species_evo_level, global_id)]
  
  last<-n_species[year==0 & species_evo_level==0]
  
  
  iqr<-IQR(last$N_Species)
  sd<-sd(last$N_Species)
  mean<-mean(last$N_Species)
  last$is_outlier<-F
  last[N_Species>(mean+3*sd)]$is_outlier<-T
  table(last$is_outlier)
  ggplot(last)+geom_histogram(aes(x=N_Species))+
    scale_x_log10()
  
}
source("commons/functions.r")
d_se<-readRDS("../Data/distribution_traits_se.rda")
d_se$evo_type<-format_evoType(d_se$species_evo_type)
d_se$label<-format_evoType_amplitude(d_se$evo_type, d_se$directional_speed, order=-1)
d[species_evo_level==0 & FROM_YEAR==1198]
p<-ggplot(d_se[species_evo_level==0])+
  geom_ribbon(aes(x=year * -0.1, ymin=N_CELLS-SD_N_CELLS, ymax=N_CELLS+SD_N_CELLS,
                  fill=label), alpha=0.2)+
  geom_line(aes(x=year * -0.1, y=N_CELLS, color=label))+
  facet_grid(nb~da, scale="free")+
  theme_bw()+
  labs(x="X k years before present", y="Number of cells", color="Evolution type",
       fill="Evolution type")
  #scale_color_colorblind()+
  #scale_fill_colorblind()
  #scale_y_log10()
p
ggsave(p, filename="../Figures/Distribution_Traits/n_cell.png", width=12, height=6)


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
       shape="Dispersal ability")+
  scale_color_colorblind()
#scale_fill_colorblind()
#scale_y_log10()
p
ggsave(p, filename="../Figures/Distribution_Traits/n_cell_end.png", width=12, height=6)


lat_se<-readRDS("../Data/lat_gradient.rda")
lat_se$evo_type<-format_evoType(lat_se$species_evo_type)
lat_se$label<-format_evoType_amplitude(lat_se$evo_type, lat_se$directional_speed, order=-1)
y<-1198
p<-ggplot(lat_se[species_evo_level==0 & year==y])+
  
  geom_point(aes(x=N_Species, y=mid, color=label))+
  facet_grid(nb~da, scale="free")+
  theme_bw()+
  labs(x="X k years before present", y="Number of cells", color="Evolution type",
       fill="Evolution type")
#scale_color_colorblind()+
#scale_fill_colorblind()
#scale_y_log10()
p
ggsave(p, filename="../Figures/Distribution_Traits/lat_gradient.png", width=12, height=6)



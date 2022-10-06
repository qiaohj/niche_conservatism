library(data.table)
library(ggplot2)
library(ggthemes)
library(RSQLite)
library(DBI)
library(sf)
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
  
  last2<-n_species[year==0 & species_evo_level==1]
  iqr<-IQR(last2$N_Species)
  sd<-sd(last2$N_Species)
  mean<-mean(last2$N_Species)
  last2$is_outlier<-F
  last2[N_Species>(mean+3*sd)]$is_outlier<-T
  table(last2$is_outlier)
  ggplot(last2)+geom_histogram(aes(x=N_Species))+
    scale_x_log10()
  
  outliers<-rbindlist(list(last, last2))
  outliers$year<-NULL
  outliers$N_Species<-NULL
  
  d_sub_with_outliers<-merge(d_sub, outliers, 
                             by=c("species_evo_type", "directional_speed",
                                  "nb", "da", "species_evo_level",
                                  "global_id","is_outlier"), all=T)
  
  
  saveRDS(d_sub_with_outliers, "../Data/lat_gradient_raw.rda")
  
  d_se<-d_sub_with_outliers[, .(N_Species=sum(N_Species), N_CELLS=sum(N_CELLS)),
                            by=list(year, species_evo_type, directional_speed,
                                    nb, da, species_evo_level,
                                    is_outlier, from, to, mid)]
  saveRDS(d_se, "../Data/lat_gradient_nb_da.rda")
  
  d_se<-d_sub_with_outliers[, .(N_Species=sum(N_Species), N_CELLS=sum(N_CELLS)),
                            by=list(year, species_evo_type, directional_speed,
                                    species_evo_level,
                                    is_outlier, from, to, mid)]
  saveRDS(d_se, "../Data/lat_gradient_outlier.rda")
  
  d_se<-d_sub_with_outliers[, .(N_Species=sum(N_Species), N_CELLS=sum(N_CELLS)),
                            by=list(year, species_evo_type, directional_speed,
                                    species_evo_level,
                                    from, to, mid)]
  saveRDS(d_se, "../Data/lat_gradient_all.rda")
  
  
  
  setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
  source("commons/functions.r")
  setDTthreads(1)
  
  base_db<-"../Configuration/env_Hadley3D.sqlite"
  envdb <- dbConnect(RSQLite::SQLite(), base_db)
  v_min_temp<-dbReadTable(envdb, "Debiased_Minimum_Monthly_Temperature")
  dbDisconnect(envdb)
  v_min_temp<-data.table(v_min_temp)
  continent_id<-v_min_temp[year==0]
  full_grids<-read_sf("../Shape/isea3h8/isea3h8p.shp")
  full_grids$global_id<-as.numeric(full_grids$global_id)
  continent<-full_grids[which(full_grids$global_id %in% continent_id$global_id),]
  plot(continent$geometry)
  coords<-st_coordinates(continent)
  continent$lon<-coords[,1]
  continent$lat<-coords[,2]
  write_sf(continent, "../Shape/isea3h8/continent.shp")
  continent_df<-data.table(continent)
  continent_df$geometry<-NULL
  lat_bands<-seq(-87.5, 87.5, by=5)
  lat_band_label<-seq(-85, 85, by=5)
  lat_band_df<-data.table(from=lat_bands[1:(length(lat_bands)-1)], 
                          to=lat_bands[2:(length(lat_bands))],
                          mid=lat_band_label)
  d_sub<-list()
  i=18
  lat_band_df$n_land<-0
  for (i in c(1:nrow(lat_band_df))){
    item<-continent_df[between(lat, lat_band_df[i]$from, lat_band_df[i]$to)]
    lat_band_df[i]$n_land<-nrow(item)
  }
  saveRDS(lat_band_df, "../Data/lat_band_n_land.rda")
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


lat_se<-readRDS("../Data/lat_gradient_nb_da.rda")
lat_se$evo_type<-format_evoType(lat_se$species_evo_type)
lat_se$label<-format_evoType_amplitude(lat_se$evo_type, lat_se$directional_speed, order=1)
y<-1198
setorderv(lat_se, "mid")
p<-ggplot(lat_se[species_evo_level==0 & year==0])+
  
  #geom_point(aes(x=N_Species, y=mid, color=label))+
  geom_path(aes(x=N_Species, y=mid, color=label), position="identity")+
  facet_grid(nb~da+is_outlier, scale="free")+
  theme_bw()+
  labs(x="Number of species", y="Latitudinal band", color="Evolution type",
       fill="Evolution type")
#scale_color_colorblind()+
#scale_fill_colorblind()
#scale_y_log10()
p
ggsave(p, filename="../Figures/Distribution_Traits/lat_gradient_details.png", width=12, height=6)

lat_se<-readRDS("../Data/lat_gradient_outlier.rda")

lat_se$evo_type<-format_evoType(lat_se$species_evo_type)
lat_se$label<-format_evoType_amplitude(lat_se$evo_type, lat_se$directional_speed, order=1)
y<-1198
setorderv(lat_se, "mid")
p<-ggplot(lat_se[species_evo_level==0 & year==0])+
  
  #geom_point(aes(x=N_Species, y=mid, color=label))+
  geom_path(aes(x=N_Species, y=mid, color=label), position="identity")+
  facet_wrap(~is_outlier, scale="free")+
  theme_bw()+
  labs(x="Number of species", y="Latitudinal band", color="Evolution type",
       fill="Evolution type")
#scale_color_colorblind()+
#scale_fill_colorblind()
#scale_y_log10()
p
ggsave(p, filename="../Figures/Distribution_Traits/lat_gradient_outlier.png", width=8, height=4)

lat_se<-readRDS("../Data/lat_gradient_all.rda")
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
lat_se_n_land_ratio[directional_speed=="0"]$directional_speed<-"conservatism"
lat_se_n_land_ratio$directional_speed<-factor(lat_se_n_land_ratio$directional_speed,
                                              c("conservatism", "0.01", "0.1", "0.5"))
p1<-ggplot(lat_se_n_land_ratio[species_evo_level==0 & year==0 & between(mid, -50, 65)])+
  
  #geom_point(aes(x=N_Species, y=mid, color=label))+
  geom_path(aes(x=MEAN_Species, y=mid, color=factor(directional_speed)), position="identity")+
  
  theme_bw()+
  labs(x="Number of species", y="Latitudinal band", color="Evolution type",
       fill="Evolution type")+
  facet_wrap(~group)+
  scale_color_colorblind()
p1

lat_se_n_land_random$group<-"random"
p2<-ggplot(lat_se_n_land_random[species_evo_level==0 & year==0 & between(mid, -50, 65)])+
  
  #geom_point(aes(x=N_Species, y=mid, color=label))+
  geom_path(aes(x=MEAN_Species, y=mid, color=evo_type), 
            position="identity")+
  
  theme_bw()+
  labs(x="Number of species", y="Latitudinal band", color="Evolution type",
       fill="Evolution type")+
  facet_wrap(~group)+
  scale_color_colorblind()
p2

p<-ggarrange(p1, p2, widths=c(2,1))
p
ggsave(p, filename="../Figures/Distribution_Traits/lat_gradient_full.png",
       width=12, height=4)

lat_se<-readRDS("../Data/lat_gradient_outlier.rda")
lat_se$evo_type<-format_evoType(lat_se$species_evo_type)
lat_se$label<-format_evoType_amplitude(lat_se$evo_type, lat_se$directional_speed, order=1)

lat_band_df<-readRDS("../Data/lat_band_n_land.rda")
lat_se_n_land<-merge(lat_se, lat_band_df, by=c("from", "to", "mid"))
lat_se_n_land$MEAN_Species<-lat_se_n_land$N_Species/lat_se_n_land$n_land
xmin_outlier<-min(lat_se_n_land[species_evo_level==0 & between(mid, -20, 65) & is_outlier==T]$MEAN_Species)
xmax_outlier<-max(lat_se_n_land[species_evo_level==0 & between(mid, -20, 65) & is_outlier==T]$MEAN_Species)

xmin_nooutlier<-min(lat_se_n_land[species_evo_level==0 & between(mid, -20, 65) & is_outlier==F]$MEAN_Species)
xmax_nooutlier<-max(lat_se_n_land[species_evo_level==0 & between(mid, -20, 65) & is_outlier==F]$MEAN_Species)

ggplot(lat_se_n_land[species_evo_level==0 & year==0])+
  geom_point(aes(x=N_Species, y=mid, color=label))+
  facet_wrap(~is_outlier, scale="free")

yyy=1198
for (yyy in c(0:1198)){
  print(yyy)

    
  p_outlier<-ggplot(lat_se_n_land[species_evo_level==0 & year==yyy & 
                                    between(mid, -50, 65) &
                                    is_outlier==T])+
    
    #geom_point(aes(x=N_Species, y=mid, color=label))+
    geom_path(aes(x=MEAN_Species, y=mid, color=label), position="identity")+
    ggtitle(sprintf("%.1f kyb (high speciation only)", yyy/10))+
    theme_bw()+
    labs(x="Number of species per cell", y="Latitudinal band", color="Evolution type",
         fill="Evolution type")+
    xlim(0, ceiling(xmax_outlier))
  #scale_color_colorblind()+
  #scale_fill_colorblind()
  #scale_y_log10()
  #p_outlier
  p_nooutlier<-ggplot(lat_se_n_land[species_evo_level==0 & year==yyy & 
                                    between(mid, -50, 65) &
                                    is_outlier==F])+
    
    #geom_point(aes(x=N_Species, y=mid, color=label))+
    geom_path(aes(x=MEAN_Species, y=mid, color=label), position="identity")+
    ggtitle(sprintf("%.1f kyb (low speciation only)", yyy/10))+
    theme_bw()+
    labs(x="Number of species per cell", y="Latitudinal band", color="Evolution type",
         fill="Evolution type")+
    xlim(0, ceiling(xmax_nooutlier))
  
  ggsave(p, 
         filename=sprintf("../Figures/Distribution_Traits/by_year_outlier/lat_gradient_outlier/per_cell_-%d.png", yyy), 
         width=8, height=4)
}

#cd /media/huijieqiao/Butterfly/Niche_Conservatism/Figures/Distribution_Traits/by_year/


#ffmpeg -r 20 -start_number -1198 -i lat_gradient_per_cell_%d.png -y ../lat_mean_sp.mp4




lat_se<-readRDS("../Data/lat_gradient_all.rda")
lat_se_n_land<-merge(lat_se, lat_band_df, by=c("from", "to", "mid"))
lat_se_n_land$MEAN_Species<-lat_se_n_land$N_Species/lat_se_n_land$n_land
xmin<-min(lat_se_n_land[species_evo_level==0 & between(mid, -50, 65)]$MEAN_Species)
xmax<-max(lat_se_n_land[species_evo_level==0 & between(mid, -50, 65)]$MEAN_Species)
yyy=1198
for (yyy in c(0:1198)){
  print(yyy)
  p<-ggplot(lat_se_n_land[species_evo_level==0 & year==yyy & between(mid, -50, 65)])+
    
    #geom_point(aes(x=N_Species, y=mid, color=label))+
    geom_path(aes(x=MEAN_Species, y=mid, color=label), position="identity")+
    ggtitle(sprintf("%.1f kyb", yyy/10))+
    theme_bw()+
    labs(x="Number of species per cell", y="Latitudinal band", color="Evolution type",
         fill="Evolution type")+
    xlim(0, ceiling(xmax))
  #scale_color_colorblind()+
  #scale_fill_colorblind()
  #scale_y_log10()
  p
  ggsave(p, 
         filename=sprintf("../Figures/Distribution_Traits/by_year/lat_gradient_per_cell_-%d.png", yyy), 
         width=8, height=4)
}

#cd /media/huijieqiao/Butterfly/Niche_Conservatism/Figures/Distribution_Traits/by_year/


#ffmpeg -r 20 -start_number -1198 -i lat_gradient_per_cell_%d.png -y ../lat_mean_sp.mp4
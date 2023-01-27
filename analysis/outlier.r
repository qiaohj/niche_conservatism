library(data.table)
library(raster)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggpubr)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")

n_species<-readRDS("../Data/N_speciation_extinction.rda")
n_sp_last<-n_species[year==0 & species_evo_level==0]
hist(n_sp_last$N_SPECIES)

nrow(n_sp_last[N_SPECIES>=2000])
colnames(n_sp_last)
cols<-c("nb", "da", "directional_speed", "species_evo_type", "species_evo_level")
coms<-unique(n_sp_last[, ..cols])
i=1
all_threshold<-list()
n_sp_last$is_outlier<-F
for (i in c(1:nrow(coms))){
  com<-coms[i]
  item<-n_sp_last[which(nb==com$nb & da==com$da &
                          directional_speed== com$directional_speed &
                          species_evo_type==com$species_evo_type)]
  IQR_n_sp<-IQR(item$N_SPECIES)
  mean_n_sp<-mean(item$N_SPECIES)
  sd_n_sp<-sd(item$N_SPECIES)
  q3<-quantile(item$N_SPECIES, 0.75)
  #max_n_sp<-mean_n_sp + 3 * sd_n_sp
  max_n_sp<-q3 + 1.5 * IQR_n_sp
  #quantile(item$N_SPECIES, c(0.5, 0.75))
           
  com$IQR_n_sp<-IQR_n_sp
  com$mean_n_sp<-mean_n_sp
  com$sd_n_sp<-sd_n_sp
  com$max_n_sp<-max_n_sp
  outliers<-item[N_SPECIES>max_n_sp]
  n_sp_last[which(nb==com$nb & da==com$da &
                    directional_speed== com$directional_speed &
                    species_evo_type==com$species_evo_type &
                    global_id %in% outliers$global_id)]$is_outlier<-T
  com$n_outliers<-length(unique(outliers$global_id))
  all_threshold[[length(all_threshold)+1]]<-com
}

table(n_sp_last$is_outlier)
all_threshold<-rbindlist(all_threshold)

global_ids<-unique(n_sp_last[is_outlier==T & nb=="BROAD" & species_evo_type %in% c(1:4) &
                               directional_speed %in% c(0, 0.1, 0.01)]$global_id)

#global_idsx<-readRDS("../Data/outliers/outliers_IQR.rda")
seeds<-readRDS("../Data/seeds.rda")

sub_seeds<-seeds[global_id %in% global_ids]
no_american_ids<-sub_seeds[lon>-20]$global_id
global_ids<-global_ids[global_ids %in% no_american_ids]

global_ids2<-unique(readRDS("../Data/outliers/outliers_keyareas.rda"))
global_ids<-global_ids[!(global_ids %in% global_ids2)]
#saveRDS(global_ids, "../Data/outliers/outliers_IQR.rda")

saveRDS(global_ids, "../Data/outliers/outliers_IQR.rda")
saveRDS(all_threshold, "../Data/outliers/N_outliers_IQR.rda")
saveRDS(n_sp_last, "../Data/outliers/outliers_details_IQR.rda")

if (F){
  
}

all_threshold[nb=="BROAD" & species_evo_type %in% c(1:4) &
                directional_speed %in% c(0, 0.1, 0.01)]
seeds<-readRDS("../Data/seeds.rda")

sub_seeds<-seeds[global_id %in% global_ids]

world <- ne_countries(scale = "small", returnclass = "sf")
polygon<-readRDS("../Figures/Example/polygon.rda")

crs_asia<-"+proj=laea +lat_0=30 +lon_0=90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
crs_america<-"+proj=laea +lat_0=30 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

item_y<-merge(polygon, sub_seeds, by.x="Name", by.y="global_id")

p_asia<-ggplot(item_y) +
  geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
  geom_sf(, fill="red")+
  #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
  coord_sf(crs = st_crs(crs_asia))+
  labs(colour="SOutliers")+
  xlim(-12e6, 12e6)+
  ylim(-12e6, 12e6)+
  theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.title = element_blank(),
        legend.position = "bottom")+
  guides(color = guide_colourbar(barwidth = 20, barheight = NULL,
                                 title.position = "left", title.hjust = 1)) 

p_asia

#legend<-get_legend(p_asia)
#p_asia<-p_asia+theme(legend.position = "none")

p_america<-ggplot(item_y) +
  geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
  geom_sf(fill="red")+
  
  #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
  coord_sf(crs = st_crs(crs_america))+
  labs(colour="Outliers")+
  xlim(-12e6, 12e6)+
  ylim(-12e6, 12e6)+
  theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.title = element_blank(),
        legend.position = "bottom")+
  guides(color = guide_colourbar(barwidth = 20, barheight = NULL,
                                 title.position = "left", title.hjust = 1)) 
p_america
p<-ggarrange(p_asia, p_america, common.legend = TRUE,legend="bottom")

p
#ggsave(p, filename=sprintf("../Figures/Diversity/by_year_%s_%s/%d_%.2f.y-%d.png", 
#                           nb, da, ccc$species_evo_type, ccc$directional_speed, y), 
#       width=12, height=6, bg = "white")
ggsave(p, filename=sprintf("../Figures/outliers/outliers_IQR.png"), 
       width=12, height=6, bg = "white")


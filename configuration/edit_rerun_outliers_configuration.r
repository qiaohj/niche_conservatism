library(dplyr)
library(rgdal)
library(raster)
library(RSQLite)
library(DBI)
library(data.table)
library(ggplot2)
library(rnaturalearth)
library(sf)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")

n_species<-readRDS("../Data/N_speciation_extinction/N_speciation_extinction.rda")
n_sp_last<-n_species[year==0 & species_evo_level==0]
range(n_sp_last$N_SPECIES)

n_sp_last[N_SPECIES==max(n_sp_last$N_SPECIES),]
n_sp_last[global_id==19980]

View(n_species[global_id==24457 & nb=="BROAD" & da=="GOOD" & species_evo_type==2 & directional_speed==0.1])
View(n_species[global_id==9725 & nb=="NARROW" & da=="GOOD" & species_evo_type==1 & directional_speed==0])
world <- ne_countries(scale = "small", returnclass = "sf")
polygon<-readRDS("../Figures/Example/polygon.rda")

crs_asia<-"+proj=laea +lat_0=30 +lon_0=90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
crs_america<-"+proj=laea +lat_0=30 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

item_y<-merge(polygon, n_sp_last[N_SPECIES==max(n_sp_last$N_SPECIES)], by.x="Name", by.y="global_id")
item_y<-merge(polygon, n_sp_last[N_SPECIES>2e3], by.x="Name", by.y="global_id")
item_y<-polygon[which(polygon$Name %in% simulations[which(simulations$is_run==1),]$global_id),]
item_y<-polygon[which(polygon$Name %in% c(420)),]
item_y<-polygon[which(polygon$Name %in% reruns_seeds),]

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


target_dir<-"../"
base_db<-"../Configuration/conf_raw.sqlite"
envdb <- dbConnect(RSQLite::SQLite(), base_db)
simulations<-dbReadTable(envdb, "simulations")
dbDisconnect(envdb)
#evo_type species_evo_type species_evo_level
rerun_seeds<-readRDS("../Data/outliers/outliers_keyareas.rda")
rerun_seeds[rerun_seeds==9725]
dim(simulations[which(simulations$global_id %in% n_sp_last[N_SPECIES>1e4]$global_id &
                    simulations$species_evo_level==0),])
table(simulations[which(simulations$global_id %in% n_sp_last[N_SPECIES>1e4]$global_id &
                        simulations$species_evo_level==0), "is_run"])
table(simulations$is_run)
#simulations[which(!(simulations$global_id %in% c(n_sp_last[N_SPECIES>=1e4]$global_id, 19980))), ]$is_run<-0
simulations[which((simulations$global_id %in% c(rerun_seeds))), ]$is_run<-0

simulations[which(simulations$species_evo_level==1), ]$is_run<-0

length(unique(simulations$global_id))
length(unique(simulations[which(simulations$is_run==1),]$global_id))
table(simulations[which(simulations$is_run==1), "species_evo_type"])

base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
dbWriteTable(mydb, "simulations", simulations, overwrite=T)
dbDisconnect(mydb)

removed_ids<-c(93, 13335, 13664, 20110, 20193, 20192, 20108, 20109, 20110,
               19889, 19888, 19887, 19886, 19967, 19968, 19969, 19970)
continents<-st_read("../Shape/isea3h8/continent.shp")
polygon_sub<-polygon[which((polygon$Name %in% continents$global_id)),]
polygon_sub_removed<-polygon[which((polygon$Name %in% removed_ids)),]

p_asia<-ggplot(polygon_sub) +
  geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
  geom_sf(color="pink", fill="pink")+
  geom_sf(data=polygon_sub_removed, color="blue", fill="blue")+
  
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
ggsave(p_asia, filename="../Figures/Configure/removed_cells.png")


base_db<-"../Configuration/env_Hadley3D_raw.sqlite"
envdb <- dbConnect(RSQLite::SQLite(), base_db)
v_min_temp<-dbReadTable(envdb, "Debiased_Minimum_Monthly_Temperature")
v_max_temp<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Temperature")
v_max_prec<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Precipitation")
mask<-dbReadTable(envdb, "mask")
dbDisconnect(envdb)

v_min_temp_removed<-v_min_temp[which(!(v_min_temp$global_id %in% removed_ids)),]
v_max_temp_removed<-v_max_temp[which(!(v_max_temp$global_id %in% removed_ids)),]
v_max_prec_removed<-v_max_prec[which(!(v_max_prec$global_id %in% removed_ids)),]
mask_removed<-mask[which(!(mask$global_id %in% removed_ids)),]
dim(v_min_temp_removed)
dim(v_min_temp)
dim(mask_removed)
dim(mask)
base_db<-"../Configuration/env_Hadley3D.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
dbWriteTable(mydb, "Debiased_Minimum_Monthly_Temperature", v_min_temp_removed, overwrite=T)
dbWriteTable(mydb, "Debiased_Maximum_Monthly_Temperature", v_max_temp_removed, overwrite=T)
dbWriteTable(mydb, "Debiased_Maximum_Monthly_Precipitation", v_max_prec_removed, overwrite=T)
dbWriteTable(mydb, "mask", mask_removed, overwrite=T)
dbDisconnect(mydb)

dim(v_min_temp_removed)
dim(v_max_temp_removed)
dim(v_max_prec_removed)


#Removed the simulations that we needn't to rerun
base_db<-"../Configuration/conf.sqlite"
envdb <- dbConnect(RSQLite::SQLite(), base_db)
simulations<-dbReadTable(envdb, "simulations")
dbDisconnect(envdb)
reruns_seeds<-unique(simulations[which(simulations$is_run==1),]$global_id)
base_new<-"../Results"
folders<-list.files(base_new)
r<-folders[1]
rm_template<-"rm -rf %s/%s"
cmds<-c()
for (r in folders){
  id<-as.numeric(strsplit(r, "_")[[1]][1])
  if (!(id %in% reruns_seeds)){
    cmd<-sprintf(rm_template, base_new, r)
    cmds<-c(cmds, cmd)
  }
}
write.csv(cmds, "rm_unrun.sh", quote=F, row.names=F, col.names=F)
#select * from simulations where global_id=23724 and nb='BROAD' and da='POOR' and species_evo_type=2 and directional_speed=1 and species_evo_level=0 


#cd ~/git/ees_3d/Debug
#is_overwrite, is_debug, is_detail
#./ees_3d /media/huijieqiao/Butterfly/Niche_Conservatism/Configuration/env_Hadley3D.sqlite /media/huijieqiao/Butterfly/Niche_Conservatism/Configuration/conf.sqlite /media/huijieqiao/Butterfly/Niche_Conservatism/Results -1 64 0 0 0
#./ees_3d /media/huijieqiao/Butterfly/Niche_Conservatism/Configuration/env_Hadley3D.sqlite /media/huijieqiao/Butterfly/Niche_Conservatism/Configuration/conf.sqlite /media/huijieqiao/Butterfly/Niche_Conservatism/Results 6551 64 1 1 1
library(data.table)
library(raster)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggpubr)


setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
setDTthreads(10)
print(sprintf("Number of core(s) is(are) %d.", getDTthreads()))


base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
#mydb <- dbConnect(RSQLite::SQLite(), "/media/huijieqiao/SSD_Fast/conf.sqlite")
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)


i=233


simulations<-simulations[which(simulations$species_evo_level==0),]
simulations<-simulations[which(simulations$is_run==1),]
simulations<-simulations[which(simulations$nb=="BROAD" & 
                                 simulations$species_evo_type %in% c(1:4) &
                                 simulations$directional_speed %in% c(0, 0.1, 0.01)),]

outlier_type<-"keyareas"
#outlier_ids<-readRDS(sprintf("../Data/outliers/outliers_%s.rda", outlier_type))
#simulations<-simulations[which(!(simulations$global_id %in% outlier_ids)),]
table(simulations$species_evo_type)

table(simulations[, c("nb", "da")])

kays<-c(20110, 20192, 19889, 19888, 19887, 19886)
template<-"%d_%s_%s_%d_%s_%d"
#all_df<-simulations[which(simulations$global_id %in% sub_seeds),]
all_df<-simulations
all_df<-all_df[sample(nrow(all_df), nrow(all_df)),]
all_df<-data.table(all_df)
global_ids<-c()
for (i in c(1:nrow(all_df))){
  item<-all_df[i,]
  sp<-sprintf(template, item$global_id, item$da, item$nb, item$species_evo_type, item$directional_speed, item$species_evo_level)
  
  print(paste(i, nrow(all_df), sp))
  if ((item$species_evo_level==1)&(item$nb=="BROAD")){
    print("1 broad skip")
    next()
  }
  if ((item$species_evo_level==1)&(item$species_evo_type==1)){
    print("1 1 skip")
    next()
  }
  
  if (item$species_evo_level==1){
    base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results_1"
  }else{
    if ((item$nb=="BROAD")&(item$da=="GOOD")){
      base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"
    }else{
      base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"
    }
  }
  ttt<-sprintf("%s/%s/%s.diversity.rda", base, sp, sp)
  df<-readRDS(ttt)
  
  if (is.null(df)){
    next()
  }
  if (nrow(df[global_id %in% kays])==0){
    global_ids<-c(global_ids, item$global_id)
  }
  
}
saveRDS(global_ids, "../Data/outliers/outliers_keyareas.rda")

global_ids<-unique(readRDS("../Data/outliers/outliers_keyareas.rda"))
seeds<-readRDS("../Data/seeds.rda")

sub_seeds<-seeds[!(global_id %in% global_ids)]

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

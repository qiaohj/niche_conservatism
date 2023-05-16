library(data.table)
library(sf)
library(RSQLite)
library(DBI)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
setDTthreads(1)
print(sprintf("Number of core(s) is(are) %d.", getDTthreads()))
if (F){
  base_db<-"../Configuration/env_Hadley3D.sqlite"
  envdb <- dbConnect(RSQLite::SQLite(), base_db)
  v_min_temp<-dbReadTable(envdb, "Debiased_Minimum_Monthly_Temperature")
  dbDisconnect(envdb)
  v_min_temp<-data.table(v_min_temp)
  polygon<-readRDS("../Figures/Example/polygon.rda")
  polygon<-polygon[which(polygon$Name %in% unique(v_min_temp$global_id)),]
  plot(polygon$geometry)
  saveRDS(polygon, "../Figures/Example/polygon_continent.rda")
}
if (F){
  sf_use_s2(FALSE)
  polygon<-st_read("../Shape/isea3h8/continent.shp")
  vessel <- sf::st_read(dsn = "../Shape/IUCN/BIRDS/BOTW.gdb", layer = "All_Species")
  polygon<-st_transform(polygon, crs=st_crs(vessel))
  i=1
  species<-unique(vessel$SCINAME)
  polygon$richness<-0
  
  for (i in c(1:length(species))){
    print(paste(i, length(species)))
    sp<-species[i]
    item<-vessel[which(vessel$SCINAME ==sp),]
    item<-item[which(st_geometry_type(item)=="MULTIPOLYGON"),]
    if (nrow(item)==0){
      next()
    }
    index<-st_intersects(item, polygon)
    index<-unique(unlist(index))
    polygon[index,]$richness<-polygon[index,]$richness+1
  }
  st_write(polygon, "../Shape/IUCN_Richness/Birds/richness.shp")
  
  polygon<-st_read("../Shape/isea3h8/continent.shp")
  
  mammals<-st_read("../Shape/IUCN/MAMMALS_TERRESTRIAL_ONLY_20220228/MAMMALS_TERRESTRIAL_ONLY.shp")
  polygon<-st_transform(polygon, crs=st_crs(mammals))
  i=1
  species<-unique(mammals$binomial)
  polygon$richness<-0
  
  for (i in c(1:length(species))){
    print(paste(i, length(species)))
    sp<-species[i]
    item<-mammals[which(mammals$binomial ==sp),]
    index<-st_intersects(item, polygon)
    index<-unique(unlist(index))
    polygon[index,]$richness<-polygon[index,]$richness+1
  }
  st_write(polygon, "../Shape/IUCN_Richness/Mammals/richness.shp")
}
mask<-readRDS("../Data/mask_lonlat.rda")
mammals<-st_read("../Shape/IUCN_Richness/Mammals/richness.shp")
birds<-st_read("../Shape/IUCN_Richness/Birds/richness.shp")
simulations<-readRDS("../Data/diversity/diversity_bootstrap.rda")
simulations<-merge(simulations, mask, by="global_id")

simulations_nb<-readRDS("../Data/diversity/diversity_bootstrap_nb.rda")
simulations_nb<-merge(simulations_nb, mask, by="global_id")
simulations_nb_da<-readRDS("../Data/diversity/diversity_bootstrap_nb_da.rda")
simulations_nb_da<-merge(simulations_nb_da, mask, by="global_id")

mammals_sim<-merge(mammals, simulations, by.x="global_id", by.y="global_id")
mammals_sim<-data.table(mammals_sim)
mammals_sim_cor<-mammals_sim[(richness>0) & (mean_N_SPECIES>0), 
                             .(cor=cor(richness, mean_N_SPECIES)), by=list(group, directional_speed)]

birds_sim<-merge(birds, simulations, by.x="global_id", by.y="global_id")
birds_sim<-data.table(birds_sim)
birds_sim_cor<-birds_sim[(richness>0) & (mean_N_SPECIES>0), 
                         .(cor=cor(richness, mean_N_SPECIES)), by=list(group, directional_speed)]

mammals_sim_cor$taxon<-"Mammals"
birds_sim_cor$taxon<-"Birds"
sim_cor<-rbind(mammals_sim_cor, birds_sim_cor)
write.csv(sim_cor, "../Data/sim_cor.csv", row.names = F)


mammals$geometry<-NULL
mammals$group<-"Mammals"
mammals$mean_N_SPECIES<-mammals$richness

birds$geometry<-NULL
birds$group<-"Birds"
birds$mean_N_SPECIES<-birds$richness

fullset<-rbindlist(list(mammals, birds, simulations, simulations_nb, simulations_nb_da), fill=T)
fullset[is.na(species_evo_type)]$species_evo_type<-0
fullset[is.na(directional_speed)]$directional_speed<-0
fullset[is.na(nb)]$nb<-""
fullset[is.na(da)]$da<-""
fullset$lat_band<-floor(fullset$lat/5) * 5
fullset$evo_type<-format_evoType(fullset$species_evo_type)
fullset[evo_type=="unknown"]$evo_type<-fullset[evo_type=="unknown"]$group
fullset$label<-sprintf("%s (%s)", fullset$evo_type, as.character(fullset$directional_speed))


fullset_se<-fullset[, .(N_SPECIES=mean(mean_N_SPECIES)),
                    by=list(species_evo_type, directional_speed, group,
                            lat_band)]

fullset_se$evo_type<-format_evoType(fullset_se$species_evo_type)
fullset_se[evo_type=="unknown"]$evo_type<-fullset_se[evo_type=="unknown"]$group
fullset_se$label<-sprintf("%s (%s)", fullset_se$evo_type, as.character(fullset_se$directional_speed))

fullset_se_f<-fullset_se[((directional_speed %in% c(0) & species_evo_type==1) |
                            (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                            (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)) |
                            group %in% c("Mammals", "Birds")) ]

fullset_all<-fullset[, .(N_SPECIES=mean(mean_N_SPECIES)),
                     by=list(species_evo_type, directional_speed, group,evo_type,label,
                             global_id)]


fullset_f<-fullset_all[((directional_speed %in% c(0) & species_evo_type==1) |
                          (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                          (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)) |
                          group %in% c("Mammals", "Birds")) ]
comb<-unique(fullset_se_f$label)
i=1
j=2
set_cor<-list()
for (i in c(1:length(comb))){
  item1<-fullset_se_f[label==comb[i]]
  item1_cell<-fullset_f[label==comb[i]]
  
  for (j in c(1:length(comb))){
    item2<-fullset_se_f[label==comb[j]]
    item2_cell<-fullset_f[label==comb[j]]
    item<-merge(item1, item2, by="lat_band", all=T)
    item[is.na(N_SPECIES.x)]$N_SPECIES.x<-0
    item[is.na(N_SPECIES.y)]$N_SPECIES.y<-0
    cor<-cor(item$N_SPECIES.x, item$N_SPECIES.y)
    
    
    item_cell<-merge(item1_cell, item2_cell, by="global_id", all=T)
    item_cell[is.na(N_SPECIES.x)]$N_SPECIES.x<-0
    item_cell[is.na(N_SPECIES.y)]$N_SPECIES.y<-0
    cor2<-cor(item_cell$N_SPECIES.x, item_cell$N_SPECIES.y)
    item_set<-data.table(group1=comb[i], group2=comb[j], cor_lat=cor, cor_cell=cor2)
    set_cor[[length(set_cor)+1]]<-item_set
  }
}
set_cor<-rbindlist(set_cor)
set_cor_2<-set_cor
set_cor_2<-set_cor[group1 %in% c("Birds (0)", "Mammals (0)")]
set_cor_2<-set_cor_2[!(group2 %in% c("Birds (0)", "Mammals (0)"))]
set_cor_2$cor_s_lat<-sprintf("%.2f", round(set_cor_2$cor_lat * 100)/100)
set_cor_2$cor_s_cell<-sprintf("%.2f", round(set_cor_2$cor_cell * 100)/100)
lvls<- c("Mammals (0)", "Birds (0)", "conservatism (0)",
         "shift-directional (0.1)", "shift-directional (0.5)",        
         "expansion-directional (0.1)", "expansion-directional (0.5)", 
         "expansion-omnidirectional (0.1)", "expansion-omnidirectional (0.5)", 
         "random-central (0.01)", "random-symmetrical (0.01)", "random-asymmetrical (0.01)")
set_cor_2$group1<-factor(set_cor_2$group1, levels=lvls)
set_cor_2$group2<-factor(set_cor_2$group2, levels=lvls)
as.numeric(set_cor_2$group1)

set_cor_gg1<-set_cor_2[as.numeric(group1)>as.numeric(group2)]
set_cor_gg1$type<-"lat"
set_cor_gg1$cor<-set_cor_gg1$cor_lat
set_cor_gg2<-set_cor_2[as.numeric(group1)<=as.numeric(group2)]
set_cor_gg2$type<-"cell"
set_cor_gg2$cor<-set_cor_gg2$cor_cell

set_cor_gg<-rbindlist(list(set_cor_gg1, set_cor_gg2))
set_cor_gg$cor_s<-sprintf("%.2f", round(set_cor_gg$cor * 100)/100)
set_cor_gg[group1==group2]$type<-""
p<-ggplot(data=set_cor_gg, aes(group1, group2, fill= cor, color=type)) + 
  geom_tile()+geom_text(aes(label=cor_s))+
  scale_fill_gradientn(colours = c("#0072B2", "white", "#CC6666"))+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p
ggsave(p, filename="../Figures/Lat_gradient/full_cor.png", width=10, height=10)

set_cor_2<-set_cor[group1 %in% c("Birds (0)", "Mammals (0)")]
set_cor_2<-set_cor_2[!(group2 %in% c("Birds (0)", "Mammals (0)"))]
set_cor_2$cor_s<-sprintf("%.2f", round(set_cor_2$cor_lat * 100)/100)

p<-ggplot(data=set_cor_2, aes(group1, group2, fill= cor_lat)) + 
  geom_tile()+geom_text(aes(label=cor_s))+
  scale_fill_gradientn(colours = c("#0072B2", "white", "#CC6666"))+
  theme_bw()+
  theme(axis.title = element_blank())
p
ggsave(p, filename="../Figures/Lat_gradient/Lat_gradient_cor.png", width=4, height=3)

set_cor_2$cor_s<-sprintf("%.2f", round(set_cor_2$cor_cell * 100)/100)
p<-ggplot(data=set_cor_2, aes(group1, group2, fill= cor_cell)) + 
  geom_tile()+geom_text(aes(label=cor_s))+
  scale_fill_gradientn(colours = c("#0072B2", "white", "#CC6666"))+
  theme_bw()+
  theme(axis.title = element_blank())
p
ggsave(p, filename="../Figures/Lat_gradient/cell_cor.png", width=4, height=3)

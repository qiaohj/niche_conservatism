library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(ggspatial)
library(data.table)
library(RSQLite)
library(DBI)
library(ape)
library(phangorn)
library(phytools)
library(geiger)
library(stringr)
library(plotKML)
library(ggtree)
library(phylobase)
library(ggpubr)
library(heatmaply)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
if (F){
  seeds<-readRDS("../Data/seeds.rda")
  seeds_p<-st_as_sf(seeds, coords = c("lon", "lat"), crs = 4326)
  continent<-read_sf("../Shape/continents/continent.shp")
  st_crs(continent)<-4326
  index<-st_contains(continent, seeds_p)
  i=1
  seeds$continent<-""
  for (i in c(1:length(index))){
    seeds[index[[i]]]$continent<-continent[i, ]$CONTINENT
  }
  #plot(continent$geometry)
  #plot(seeds, add=T, col=as.numeric(as.factor(seeds$continent)), pch=as.numeric(seeds$outlier)+1)
  outliers<-readRDS("../Data/outliers/outliers_3SD.rda")
  seeds$outlier<-seeds$global_id %in% unique(outliers$global_id)
  seeds<-seeds[outlier==F & continent!=""]
  rep_from<-1
  rep_to<-1e4
  evo_types<-data.table(species_evo_type=c(1, 2, 2, 3, 3, 4, 4, 5, 6, 7),
                        directional_speed=c(0, 0.1, 0.5, 0.1, 0.5, 0.1, 0.5, 0.01, 0.01, 0.01))
  evo_types_1<-evo_types
  evo_types_1$nb<-"BROAD"
  evo_types_1$da<-"GOOD"
  
  evo_types_2<-evo_types
  evo_types_2$nb<-"BROAD"
  evo_types_2$da<-"POOR"
  
  evo_types_3<-evo_types
  evo_types_3$nb<-"NARROW"
  evo_types_3$da<-"GOOD"
  
  evo_types_4<-evo_types
  evo_types_4$nb<-"NARROW"
  evo_types_4$da<-"POOR"
  
  evo_types<-rbindlist(list(evo_types_1, evo_types_2, evo_types_3, evo_types_4))
  seeds_all<-seeds[sample(c(1:nrow(seeds)), 4000, replace=T)]
  seeds_rand<-list()
  for (i in c(101:rep_to)){
    print(i)
    persentage<-c(0, runif(nrow(evo_types), 0, 1))
    persentage<-persentage/sum(persentage)
    persentage<-data.table(persentage=persentage, cum_persentage=persentage)
    for (j in c(2:nrow(persentage))){
      persentage[j]$cum_persentage<-persentage[j-1]$cum_persentage+persentage[j]$persentage
    }
    #hist(persentage)
    item<-seeds_all
    rand<-data.table(v=runif(nrow(seeds_all), 0, 1), index=0)
    for (j in c(2:nrow(persentage))){
      rand[between(v, persentage[j-1]$cum_persentage, persentage[j]$cum_persentage)]$index<-j-1
    }
    item$species_evo_type<-evo_types[rand$index]$species_evo_type
    item$directional_speed<-evo_types[rand$index]$directional_speed
    item$nb<-evo_types[rand$index]$nb
    item$da<-evo_types[rand$index]$da
    item$rep<-i
    seeds_rand[[length(seeds_rand)+1]]<-item
  }
  seeds_rand_df<-rbindlist(seeds_rand)
  saveRDS(seeds_rand_df, "../Data/diversity_random/seed_random_nb_da.rda")
}

if (F){
  diversity<-readRDS("../Data/diversity/diversity_full_without_outlier_last_year.rda")
  diversity[, .(N=length(unique(seed_id))), by=list(nb, da, species_evo_type, directional_speed)]
  seeds_rand_nb_da<-readRDS("../Data/diversity_random/seed_random_nb_da.rda")
  label_div<-sprintf("%d, %d, %s, %s, %s", diversity$seed_id, diversity$species_evo_type, 
                     as.character(diversity$directional_speed), diversity$nb, diversity$da)
  diversity_rep<-list()
  i=1
  len<-length(unique(seeds_rand_nb_da$rep))
  for (i in unique(seeds_rand_nb_da$rep)){
    target<-sprintf("../Data/diversity_random_items/%d_nb_da.rda", i)
    if (file.exists(target)){
      next()
    }
    saveRDS(NULL, target)
    print(paste(i, len))
    seeds_item<-seeds_rand_nb_da[rep==i]
    label<-sprintf("%d, %d, %s, %s, %s", seeds_item$global_id, seeds_item$species_evo_type, 
                   as.character(seeds_item$directional_speed), seeds_item$nb, seeds_item$da)
    
    diversity_item<-diversity[label_div %in% label]
    diversity_se<-diversity_item[, .(N_SPECIES=sum(N_SPECIES)),
                                 by=list(global_id)]
    if (F){
      dd<-seeds_item[, .(N=.N), by=list(species_evo_type, directional_speed)]
      dd$N_P<-round(dd$N/sum(dd$N) * 10000)/100
      setorderv(dd, c("species_evo_type", "directional_speed"))
      title<-paste(sprintf("%s (%s), %.2f%%", dd$species_evo_type, 
                           as.character(dd$directional_speed),
                           dd$N_P), collapse='; ')
      polygon<-readRDS("../Figures/Example/polygon.rda")
      world <- ne_countries(scale = "small", returnclass = "sf")
      
      
      crs_asia<-"+proj=laea +lat_0=30 +lon_0=90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
      crs_america<-"+proj=laea +lat_0=30 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
      p2_1<-create_fig(diversity_se, title)
      ggsave(p2_1, filename=sprintf("../Figures/diversity_random_items/%d_nb_da.png", i), width=14, height=8, bg="white")
    }
    saveRDS(diversity_se, target)
    
  }
}
if (F){
  diversity<-readRDS("../Data/diversity/diversity_full_without_outlier_last_year.rda")
  diversity_se<-diversity[, .(N_SPECIES=sum(N_SPECIES)),
                               by=list(global_id)]
  saveRDS(diversity_se, "../Data/diversity/diversity_all_last_year.rda")
  polygon<-readRDS("../Figures/Example/polygon.rda")
  world <- ne_countries(scale = "small", returnclass = "sf")
  
  
  crs_asia<-"+proj=laea +lat_0=30 +lon_0=90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
  crs_america<-"+proj=laea +lat_0=30 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
  p2_1<-create_fig(diversity_se, "All")
  p1
  ggsave(p2_1, filename="../Figures/diversity_random/all.png", width=14, height=8, bg="white")
}

if (F){
  birds<-read_sf("../Shape/IUCN_Richness/Birds/richness.shp")
  birds$geometry<-NULL
  birds<-data.table(birds)
  birds$lat<-NULL
  birds$lon<-NULL
  colnames(birds)[2]<-"IUCN_richness_bird"
  
  mammals<-read_sf("../Shape/IUCN_Richness/Mammals/richness.shp")
  mammals$geometry<-NULL
  mammals<-data.table(mammals)
  mammals$lat<-NULL
  mammals$lon<-NULL
  colnames(mammals)[2]<-"IUCN_richness_mammal"
  
  iucn<-merge(mammals, birds, by="global_id")
  seeds_rand_nb_da<-readRDS("../Data/diversity_random/seed_random_nb_da.rda")
  mask<-readRDS("../Data/mask_lonlat.rda")
  mask_p<-st_as_sf(mask, coords = c("lon", "lat"), crs = 4326)
  continent<-read_sf("../Shape/continents/continent.shp")
  st_crs(continent)<-4326
  index<-st_contains(continent, mask_p)
  i=1
  mask$continent<-""
  for (i in c(1:length(index))){
    mask[index[[i]]]$continent<-continent[i, ]$CONTINENT
  }
  table(mask$continent)
  i=1
  cor_list<-list()
  for (i in c(unique(seeds_rand_nb_da$rep))){
    target<-sprintf("../Data/diversity_random_items/%d_nb_da.rda", i)
    if (!file.exists(target)){
      next()
    }
    print(i)
    item<-readRDS(target)
    item<-merge(item, mask, by="global_id")
    item<-merge(item, iucn, by="global_id")
    item<-item[continent!=""]
    item_cor<-item[, .(cor_bird=cor(N_SPECIES, IUCN_richness_bird),
                       cor_mammal=cor(N_SPECIES, IUCN_richness_mammal)),
                   by=list(continent)]
    item_cor$cor_bird_global<-cor(item$N_SPECIES, item$IUCN_richness_bird)
    item_cor$cor_mammal_global<-cor(item$N_SPECIES, item$IUCN_richness_mammal)
    item_cor$rep<-i
    cor_list[[length(cor_list)+1]]<-item_cor
  }
  cor_list<-rbindlist(cor_list)
  item1<-cor_list[, c("continent", "cor_bird", "rep")]
  colnames(item1)[2]<-"cor"
  item1$type<-"cor_bird"
  
  item2<-cor_list[, c("continent", "cor_mammal", "rep")]
  colnames(item2)[2]<-"cor"
  item2$type<-"cor_mammal"
  
  item3<-cor_list[, c("continent", "cor_bird_global", "rep")]
  colnames(item3)[2]<-"cor"
  item3$type<-"cor_bird_global"
  item3$continent<-"global"
  item3<-unique(item3)
  
  item4<-cor_list[, c("continent", "cor_mammal_global", "rep")]
  colnames(item4)[2]<-"cor"
  item4$type<-"cor_mammal_global"
  item4$continent<-"global"
  item4<-unique(item4)
  
 
  
  gg_df<-rbindlist(list(item1, item2, item3, item4))
  
  diversity_all<-readRDS("../Data/diversity/diversity_all_last_year.rda")
  diversity_all<-merge(diversity_all, iucn, by="global_id") 
  diversity_all<-merge(diversity_all, mask, by="global_id")
  diversity_all<-diversity_all[continent!=""]
  
  cor_all<-diversity_all[, .(cor_bird_all=cor(N_SPECIES, IUCN_richness_bird),
                             cor_mammal_all=cor(N_SPECIES, IUCN_richness_mammal)),
                         by=list(continent)]
  item_all_1<-cor_all[, c("continent", "cor_bird_all")]
  colnames(item_all_1)[2]<-"cor"
  item_all_1$type<-"cor_bird"
  item_all_2<-cor_all[, c("continent", "cor_mammal_all")]
  colnames(item_all_2)[2]<-"cor"
  item_all_2$type<-"cor_mammal"
  
  item_all_3<-data.table(continent="global", cor=c(
    cor(diversity_all$N_SPECIES, diversity_all$IUCN_richness_bird),
    cor(diversity_all$N_SPECIES, diversity_all$IUCN_richness_mammal)
  ),
  type=c("cor_bird_global", "cor_mammal_global"))
  gg_df_all<-rbindlist(list(item_all_1, item_all_2, item_all_3))
  gg_df_all$rep<-1
  
  ggplot(gg_df)+geom_boxplot(aes(x=continent, y=cor, color=type))+
    geom_point(data=gg_df_all, aes(x=continent, y=cor, color=type), shape=2)
  ]range(gg_df$cor, na.rm=T)
  hist(gg_df$cor)
  gg_df[,.(max_cor=max(cor)), by=list(continent, type)]
  
  diversity_boost<-readRDS("../Data/diversity/diversity_bootstrap.rda")
  diversity_boost<-merge(diversity_boost, iucn, by="global_id")
  diversity_boost<-merge(diversity_boost, mask, by="global_id")
  diversity_boost<-diversity_boost[continent!=""]
  cor_boost<-diversity_boost[continent!="Oceania", 
                             .(cor_boost_bird=cor(mean_N_SPECIES, IUCN_richness_bird),
                                 cor_boost_mammal=cor(mean_N_SPECIES, IUCN_richness_mammal)),
                             by=list(group, continent, species_evo_type, directional_speed)]
  cor_boost_global<-diversity_boost[continent!="Oceania", 
                                    .(cor_boost_bird=cor(mean_N_SPECIES, IUCN_richness_bird),
                                        cor_boost_mammal=cor(mean_N_SPECIES, IUCN_richness_mammal)),
                                    by=list(group, species_evo_type, directional_speed)]
  
  item1<-cor_boost[, c("continent", "cor_boost_bird", "species_evo_type", "directional_speed")]
  colnames(item1)[2]<-"cor"
  item1$type<-"cor_bird"
  
  item2<-cor_boost[, c("continent", "cor_boost_mammal", "species_evo_type", "directional_speed")]
  colnames(item2)[2]<-"cor"
  item2$type<-"cor_mammal"
  
  item3<-cor_boost_global[, c("cor_boost_bird", "species_evo_type", "directional_speed")]
  colnames(item3)[1]<-"cor"
  item3$type<-"cor_bird_global"
  item3$continent<-"global"
  item3<-unique(item3)
  
  item4<-cor_boost_global[, c("cor_boost_mammal", "species_evo_type", "directional_speed")]
  colnames(item4)[1]<-"cor"
  item4$type<-"cor_mammal_global"
  item4$continent<-"global"
  item4<-unique(item4)
  
  
  
  gg_df_boost<-rbindlist(list(item1, item2, item3, item4), use.names=TRUE)
  gg_df_boost$rep<-1
  
  seeds_rand<-readRDS("../Data/diversity_random/seed_random.rda")
  mask<-readRDS("../Data/mask_lonlat.rda")
  mask_p<-st_as_sf(mask, coords = c("lon", "lat"), crs = 4326)
  continent<-read_sf("../Shape/continents/continent.shp")
  st_crs(continent)<-4326
  index<-st_contains(continent, mask_p)
  i=1
  mask$continent<-""
  for (i in c(1:length(index))){
    mask[index[[i]]]$continent<-continent[i, ]$CONTINENT
  }
  table(mask$continent)
  i=1
  cor_list<-list()
  for (i in c(unique(seeds_rand$rep))){
    target<-sprintf("../Data/diversity_random_items/%d.rda", i)
    if (!file.exists(target)){
      next()
    }
    print(i)
    item<-readRDS(target)
    item<-merge(item, mask, by="global_id")
    item<-merge(item, iucn, by="global_id")
    item<-item[continent!=""]
    item_cor<-item[, .(cor_bird=cor(N_SPECIES, IUCN_richness_bird),
                       cor_mammal=cor(N_SPECIES, IUCN_richness_mammal)),
                   by=list(continent)]
    item_cor$cor_bird_global<-cor(item$N_SPECIES, item$IUCN_richness_bird)
    item_cor$cor_mammal_global<-cor(item$N_SPECIES, item$IUCN_richness_mammal)
    item_cor$rep<-i
    cor_list[[length(cor_list)+1]]<-item_cor
  }
  cor_list<-rbindlist(cor_list)
  item1<-cor_list[, c("continent", "cor_bird", "rep")]
  colnames(item1)[2]<-"cor"
  item1$type<-"cor_bird"
  
  item2<-cor_list[, c("continent", "cor_mammal", "rep")]
  colnames(item2)[2]<-"cor"
  item2$type<-"cor_mammal"
  
  item3<-cor_list[, c("continent", "cor_bird_global", "rep")]
  colnames(item3)[2]<-"cor"
  item3$type<-"cor_bird_global"
  item3$continent<-"global"
  item3<-unique(item3)
  
  item4<-cor_list[, c("continent", "cor_mammal_global", "rep")]
  colnames(item4)[2]<-"cor"
  item4$type<-"cor_mammal_global"
  item4$continent<-"global"
  item4<-unique(item4)
  
  
  
  gg_df_full<-rbindlist(list(item1, item2, item3, item4))
  
  gg_df_full[,.(cor=max(cor)), by=list(type, continent)]
  
  gg_df_full$sample<-"by evo type"
  gg_df$sample<-"by evo type, nb and da"
  gg_df_boost$sample<-"boostrap"
  gg_df_all$sample<-"all per evo type"
  
  gg_all<-rbindlist(list(gg_df_full, gg_df, gg_df_boost, gg_df_all), fill=TRUE)
  gg_all<-gg_all[continent!="Oceania"]
  gg_all[is.na(species_evo_type)]$species_evo_type<-0
  gg_all[is.na(directional_speed)]$directional_speed<-0
  gg_all<-gg_all[((directional_speed %in% c(0) & species_evo_type %in% c(0, 1)) |
      (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
      (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)))]
  
  gg_all_max<-gg_all[, .(max_cor=max(cor)),
                     by=list(continent, type, sample)]
  gg_all_with_max<-merge(gg_all, gg_all_max, by=c("continent", "type",
                                                  "sample"))
  gg_all_max_only<-gg_all_with_max[cor==max_cor]
  gg_all_max_only[type=="cor_bird_global"]$type<-"cor_bird"
  gg_all_max_only[type=="cor_mammal_global"]$type<-"cor_mammal"
  saveRDS(gg_all_with_max, "../Figures/LM/gg_all_with_max.rda")
  saveRDS(gg_all_max_only, "../Figures/LM/gg_all_max_only.rda")
  
  gg_all_max_only$continent_f<-factor(gg_all_max_only$continent, levels=c("Africa", "Asia", "Australia", 
                                                              "Europe", "North America", "South America", 
                                                              "global"))
  gg_all_max_only[continent=="global"]
  gg_all_mean<-gg_all_max_only[,.(cor=mean(cor)), by=list(continent_f, sample)]
  ggplot(gg_all_max_only)+geom_point(aes(x=continent_f, y=cor, color=sample, shape=type))
  
  x<-seeds_rand_nb_da[rep==3559]
  x[,.(N=.N/4000), by=list(species_evo_type, directional_speed, nb, da)]
}

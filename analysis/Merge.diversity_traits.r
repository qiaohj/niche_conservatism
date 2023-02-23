library(ggplot2)
library(data.table)
library(RSQLite)
library(DBI)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
setDTthreads(20)
print(sprintf("Number of core(s) is(are) %d.", getDTthreads()))


base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
#mydb <- dbConnect(RSQLite::SQLite(), "/media/huijieqiao/SSD_Fast/conf.sqlite")
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)


i=233


nb<-"BROAD"
da<-"GOOD"
simulations<-simulations[which(((simulations$nb==nb & simulations$da==da))),]
simulations<-simulations[which(simulations$species_evo_level==0),]
#simulations<-simulations[which(simulations$species_evo_type==1),]
#simulations<-simulations[which(simulations$is_run==1),]

table(simulations$species_evo_type)

table(simulations[, c("nb", "da")])


template<-"%d_%s_%s_%d_%s_%d"
for (t in c(1:7)){
  #all_df<-simulations[which(simulations$global_id %in% sub_seeds),]
  all_df<-simulations[which(simulations$species_evo_type==t),]
  all_df<-all_df[sample(nrow(all_df), nrow(all_df)),]
  all_df<-data.table(all_df)
  df_all<-list()
  for (i in c(1:nrow(all_df))){
    item<-all_df[i,]
    sp<-sprintf(template, item$global_id, item$da, item$nb, item$species_evo_type, 
                item$directional_speed, item$species_evo_level)
    
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
        base<-"../Results"
        base2<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"
      }else{
        base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"
      }
    }
    ttt<-sprintf("%s/%s/%s.diversity.rda", base, sp, sp)
    if (!file.exists(ttt)){
      ttt<-sprintf("%s/%s/%s.diversity.rda", base2, sp, sp)
    }
    df<-readRDS(ttt)
    
    if (is.null(df)){
      asdf
      next()
    }
    df$seed_id<-item$initial_seeds
    df$da<-item$da
    df$nb<-item$nb
    df$evo_type<-item$evo_type
    df$species_evo_type<-item$species_evo_type
    df$directional_speed<-item$directional_speed 
    df$species_evo_level<-item$species_evo_level
    
    df_all[[length(df_all)+1]]<-df
  }
  
  if (T){
    df_all_df<-rbindlist(df_all)
    diversity_se<-df_all_df[, .(N_SPECIES=sum(N_SPECIES), N_INDIVIDUAL=sum(N_INDIVIDUAL)),
                            by=list(year, global_id, da, nb, evo_type, 
                                    species_evo_type, directional_speed,
                                    species_evo_level)]
    saveRDS(diversity_se, 
            sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Data/diversity_items/%s_%s_%d.rda", nb, da, t))
    
    
    
  }
}
if (F){
  
  diversity_se_all<-list()
  for (i in seq(1, length(df_all), by=1200)){
    print(i)
    i=12801
    df_all_df<-rbindlist(df_all[c(i:(i+1199))])
    diversity_se<-df_all_df[, .(N_SPECIES=sum(N_SPECIES), N_INDIVIDUAL=sum(N_INDIVIDUAL)),
                            by=list(year, global_id, da, nb, evo_type, 
                                    species_evo_type, directional_speed,
                                    species_evo_level)]
    diversity_se$i<-i
    diversity_se_all[[length(diversity_se_all)+1]]<-diversity_se
  }
  
  diversity_se_all_df<-rbindlist(diversity_se_all[c(1:8)])
  diversity_se_all_df2<-rbindlist(diversity_se_all[c(9:11)])
  
  diversity_se_all_df_se<-diversity_se_all_df[, .(N_SPECIES=sum(N_SPECIES), N_INDIVIDUAL=sum(N_INDIVIDUAL)),
                                              by=list(year, global_id, da, nb, evo_type, 
                                                      species_evo_type, directional_speed,
                                                      species_evo_level)]
  
  
  diversity_se_all_df_se2<-diversity_se_all_df2[, .(N_SPECIES=sum(N_SPECIES), N_INDIVIDUAL=sum(N_INDIVIDUAL)),
                                                by=list(year, global_id, da, nb, evo_type, 
                                                        species_evo_type, directional_speed,
                                                        species_evo_level)]
  
  diversity_se_all_df_se_xx<-rbindlist(list(diversity_se_all_df_se, diversity_se_all_df_se2))
  
  diversity_se_all_df_se_xx2<-diversity_se_all_df_se_xx[, .(N_SPECIES=sum(N_SPECIES), N_INDIVIDUAL=sum(N_INDIVIDUAL)),
                                                        by=list(year, global_id, da, nb, evo_type, 
                                                                species_evo_type, directional_speed,
                                                                species_evo_level)]
  diversity_se_all_df_se1<-readRDS(sprintf("../Data/diversity_items_%s_%s.rda", nb, da))
  
  diversity_se_all_df_se2<-readRDS(sprintf("../Data/diversity_items_%s_%s2.rda", nb, da))
  
}
if (F){
  diversity_new<-readRDS("/media/huijieqiao/QNAS/Niche_Conservatism/Data/diversity_items/BROAD_GOOD_1.rda")
  diversity_old<-readRDS("/media/huijieqiao/QNAS/Niche_Conservatism/Data/diversity_items/old/BROAD_GOOD.rda")
  diversity_old<-diversity_old[species_evo_type==1]
  diversity<-merge(diversity_new, diversity_old, 
                   by=c("year", "global_id", "da", "nb", "evo_type",
                        "species_evo_type", "directional_speed", "species_evo_level"),
                   all=T)
  diversity[is.na(N_SPECIES.x)]$N_SPECIES.x<-0
  diversity[is.na(N_SPECIES.y)]$N_SPECIES.y<-0
  diversity$decrease<-diversity$N_SPECIES.y - diversity$N_SPECIES.x
  history(diversity$decrease)
  ggplot(diversity[year==0])+geom_histogram(aes(decrease), bins=100)
  
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
  
  polygon<-readRDS("../Figures/Example/polygon.rda")
  world <- ne_countries(scale = "small", returnclass = "sf")
  
  
  crs_asia<-"+proj=laea +lat_0=30 +lon_0=90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
  crs_america<-"+proj=laea +lat_0=30 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
  
  
  item_y<-diversity_new[year==0]
  item_y<-merge(polygon, item_y, by.x="Name", by.y="global_id")
  
  threshold<-round(mean(item_y$N_SPECIES)+3*sd(item_y$N_SPECIES))
  mycol <- cool_warm(threshold + 1)
  
  max_n_sp<-max(item_y$N_SPECIES)
  min_n_sp<-min(item_y$N_SPECIES)
  if (threshold>max_n_sp){
    midpoint<-round(max_n_sp/2)
    breakss<-c(min_n_sp, midpoint, max_n_sp)
    labelss<-c(min_n_sp, "", max_n_sp)
  }else{
    midpoint<-round(threshold/2)
    breakss<-c(min_n_sp, midpoint, threshold)
    labelss<-c(min_n_sp, "", sprintf(">%d, up to %d", threshold, max_n_sp))
  }
  
  p_asia<-ggplot(item_y, aes(colour=N_SPECIES)) +
    geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
    geom_sf()+
    scale_color_gradient2(low  = mycol[1], high=mycol[length(mycol)],
                          mid = "#DDDDDD", midpoint=midpoint,
                          breaks=breakss, 
                          labels=labelss)+
    
    #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
    coord_sf(crs = st_crs(crs_asia))+
    labs(colour="Species richness")+
    xlim(-12e6, 12e6)+
    ylim(-12e6, 12e6)+
    theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.title = element_blank(),
          legend.position = "bottom")+
    guides(color = guide_colourbar(barwidth = 20, barheight = NULL,
                                   title.position = "left", title.hjust = 1)) 
  #legend<-get_legend(p_asia)
  #p_asia<-p_asia+theme(legend.position = "none")
  
  p_america<-ggplot(item_y, aes(colour=N_SPECIES)) +
    geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
    geom_sf()+
    scale_color_gradient2(low  = mycol[1], high=mycol[length(mycol)],
                          mid = "#DDDDDD", midpoint=midpoint,
                          breaks=breakss, 
                          labels=labelss)+
    
    #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
    coord_sf(crs = st_crs(crs_america))+
    labs(colour="Species richness")+
    xlim(-12e6, 12e6)+
    ylim(-12e6, 12e6)+
    theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.title = element_blank(),
          legend.position = "bottom")+
    guides(color = guide_colourbar(barwidth = 20, barheight = NULL,
                                   title.position = "left", title.hjust = 1)) 
  
  p<-ggarrange(p_asia, p_america, common.legend = TRUE,legend="bottom")
  
  p<-annotate_figure(p, top = sprintf("%.1f kyb", y/10))
}


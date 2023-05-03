library(data.table)
library(ggplot2)
library(ggthemes)
library(RSQLite)
library(DBI)
library(sf)
setDTthreads(20)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
df<-readRDS("../Data/lat_gradient/lat_gradient_raw_3SD.rda")



if (F){
  mask_lonlat<-readRDS("../Data/mask_lonlat.rda")
  base_db<-"../Configuration/conf.sqlite"
  mydb <- dbConnect(RSQLite::SQLite(), base_db)
  simulations<-dbReadTable(mydb, "simulations")
  dbDisconnect(mydb)
  simulations<-data.table(simulations)
  outliers<-readRDS("../Data/outliers/outliers_3SD.rda")
  seeds<-mask_lonlat[global_id %in% simulations$global_id]
  lat_bands<-seq(-87.5, 87.5, by=5)
  lat_band_label<-seq(-85, 85, by=5)
  lat_band_df<-data.table(from=lat_bands[1:(length(lat_bands)-1)], 
                          to=lat_bands[2:(length(lat_bands))],
                          mid=lat_band_label)
  i=1
  seeds$from<--9999
  seeds$to<--9999
  seeds$mid<--9999
  for (i in c(1:nrow(lat_band_df))){
    print(paste(i, nrow(lat_band_df)))
    lat_band_item<-lat_band_df[i]
    seeds[between(lat, lat_band_item$from, lat_band_item$to)]$from<-lat_band_item$from
    seeds[between(lat, lat_band_item$from, lat_band_item$to)]$to<-lat_band_item$to
    seeds[between(lat, lat_band_item$from, lat_band_item$to)]$mid<-lat_band_item$mid
    
  }
  saveRDS(seeds, "../Data/seeds.rda")
  hist(seeds$mid)
  seeds_count<-seeds[, .(N=.N), by=mid]
  
  seeds_1<-seeds[mid %in% seeds_count[N>=10]$mid]
  seeds_2<-seeds[mid %in% seeds_count[N<10]$mid]
  sampled_seeds<-list()
  for (i in 1:100){
    sampled_seeds_1<-seeds_1[,.SD[sample(.N, 10)],by = mid]
    sampled_seeds_2<-seeds_2[,.SD[sample(.N, 10, replace=T)],by = mid]
    sampled_seeds_item<-rbindlist(list(sampled_seeds_1, sampled_seeds_2))
    sampled_seeds_item$rep<-i
    sampled_seeds[[i]]<-sampled_seeds_item
  }
  sampled_seeds<-rbindlist(sampled_seeds)
  hist(sampled_seeds$mid)
  hist(table(sampled_seeds$global_id))
  saveRDS(sampled_seeds, "../Data/sampled_seeds.rda")
}
sampled_seeds<-readRDS("../Data/sampled_seeds.rda")

outliers<-readRDS("../Data/outliers.rda")
df[!(global_id %in% outliers)]$is_outlier<-F

setindexv(df, "global_id")
i=10
n_splist<-list()
for (i in c(1:max(sampled_seeds$rep))){
#for (i in c(1:10)){
  print(i)
  seed_item<-sampled_seeds[rep==i]
  df_item<-df[global_id %in% seed_item$global_id]
  n_sp<-df_item[,.(N_Species=sum(N_Species),
                   N_CELLS=sum(N_CELLS)),
                by=list(year, species_evo_type, directional_speed,
                        species_evo_level, from, to, mid, is_outlier)]
  n_sp$rep<-i
  n_splist[[i]]<-n_sp
  if (F){
    
    setorderv(n_sp, "mid")
    unique(n_sp$species_evo_level)
    p1<-ggplot(n_sp[species_evo_level==0 & year==0 & is_outlier==F])+
      
      #geom_point(aes(x=N_Species, y=mid, color=label))+
      geom_path(aes(x=N_Species, y=mid, color=factor(directional_speed)), position="identity")+
      
      theme_bw()+
      labs(x="Number of species", y="Latitudinal band", color="Evolution ratio",
           fill="Evolution ratio")+
      facet_grid(species_evo_type~directional_speed)+
      scale_color_manual(values=c("black", colorBlindGrey8[2:4]))
      #scale_x_continuous(breaks=c(40, 80, 120), labels=c(40, 80, 120))
      #theme(legend.position = c(0.25, 0.15),
      #      axis.title.x = element_blank(),
      #      plot.margin = unit(c(0.01, 0, 0, 0.01), "null"),
      #      legend.background = element_rect(fill=bg))
    p1
    
  }
}

n_splist_df<-rbindlist(n_splist)
saveRDS(n_splist_df, "../Data/lat_gradient_bootstrapping.rda")
n_splist_df_se<-n_splist_df[, .(N_Species=mean(N_Species),
                                N_Species_SD=sd(N_Species),
                                N_CELLS=mean(N_CELLS),
                                N_CELLS_SD=sd(N_CELLS)),
                            by=list(year, species_evo_type, directional_speed, species_evo_level,
                                    from, to, mid, is_outlier)]
saveRDS(n_splist_df_se, "../Data/lat_gradient_bootstrapping_se.rda")

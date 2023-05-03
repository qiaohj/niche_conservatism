library(ggplot2)
library(data.table)
library(RSQLite)
library(DBI)
library(ape)
library(phangorn)
library(phytools)
library(geiger)
library(stringr)
library(tidyverse)
library(plotKML)
library(ggtree)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
setDTthreads(1)
print(sprintf("Number of core(s) is(are) %d.", getDTthreads()))
if (F){
  sub_seeds<-c(27790,25837,24109,54786,19759,13651,17569,18517,4497,4847,9898,11847,40440,23724)
  #species_evo_type
  #1: niche conservatism
  #2: niche shift (directional)
  #3: niche expansion (directional)
  #4: niche expansion (omnidirectional)
  #5: niche shift (random in box center)
  #6: niche shift (random symmetrical change in box limit)
  #7: niche shift (random asymmetrical change in box limit)
  species_evo_types<-data.frame(species_evo_type=c(1,2,2,2,3,3,3,4,4,4,5,6,7),
                                directional_speed=c(0,0.5,0.1,0.01,0.5,0.1,0.01,
                                                    0.5,0.1,0.01,0.01,0.01,0.01))
  nb<-c("BROAD", "NARROW")
  da<-c("GOOD", "POOR")
  conf<-data.table(expand.grid(nb=nb, da=da, global_id=sub_seeds, 
                               species_evo_level=c(0, 1), stringsAsFactors = F))
  
  i=2
  all_df<-NULL
  for (i in c(1:nrow(species_evo_types))){
    item<-species_evo_types[i,]
    ss<-conf
    ss$species_evo_type<-item$species_evo_type
    ss$directional_speed<-item$directional_speed
    all_df<-bind(all_df, ss)
  }
  all_df<-all_df[!((species_evo_level==1)&(species_evo_type==1))]
  all_df$directional_speed<-as.character(all_df$directional_speed)
  
  #23724_POOR_BROAD_2_0.01_0
  
  i=2
  #event_df<-NULL
  ttt<-"()SP27790;"
  vert.tree<-read.tree(text=ttt)
}

base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)


i=233
if (F){
  simulations<-simulations[which((simulations$global_id==27464)&
                                   (simulations$nb=="NARROW")&
                                   (simulations$da=="GOOD")&
                                   (simulations$species_evo_level==0)),]
  
  
  #simulations<-simulations[which(simulations$nb=="NARROW"),]
  simulations<-simulations[which((simulations$nb=="NARROW")&
                                   (simulations$da=="GOOD")&
                                   (simulations$species_evo_type %in% c(2))&
                                   (simulations$is_run==1)&
                                   (simulations$species_evo_level==0)),]
  simulations<-simulations[which(!((simulations$da=="GOOD")&(simulations$nb=="BROAD"))),]
}
#simulations<-simulations[which((simulations$nb=="NARROW")),]
simulations<-simulations[which(simulations$species_evo_level==0),]
simulations<-simulations[which(simulations$is_run==1),]
#simulations<-simulations[which(simulations$species_evo_type %in% c(4)),]
  
table(simulations$species_evo_type)

table(simulations[, c("nb", "da")])

if (F){
  mask<-readRDS("../old_data/ENV/mask_df.rda")
  mask<-data.table(mask)
  cols<-c("global_id", "lon", "lat")
  mask<-unique(mask[, ..cols])
  saveRDS(mask, "../Data/mask_lonlat.rda")
}
template<-"%d_%s_%s_%d_%s_%d"
mask<-readRDS("../Data/mask_lonlat.rda")
#all_df<-simulations[which(simulations$global_id %in% sub_seeds),]
all_df<-simulations
all_df<-all_df[sample(nrow(all_df), nrow(all_df)),]
all_df<-data.table(all_df)
#all_df<-all_df[nb=="BROAD"&da=="GOOD"&species_evo_type==1]
if (F){
  for (i in c(1:nrow(all_df))){
    
    
    item<-all_df[i]
    
    sp<-sprintf(template, item$global_id, item$da, item$nb, item$species_evo_type, 
                item$directional_speed, item$species_evo_level)
    print(paste(i, nrow(all_df), sp))
    ttt<-sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results/%s/%s.N.rda", sp, sp)
    if (!file.exists(ttt)){
      next()
    }
    dfff<-readRDS(ttt)
    if (is.null(dfff)){
      unlink(ttt)
      next()
    }
    ncols<-ncol(dfff)
    if (ncols==6){
      file.rename(ttt, sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results/%s/%s.N.csv", sp, sp))
    }else{
      adsf
    }
  }
}
for (i in c(1:nrow(all_df))){
  
 
  item<-all_df[i]
  
  sp<-sprintf(template, item$global_id, item$da, item$nb, item$species_evo_type, item$directional_speed, item$species_evo_level)
  print(paste(i, nrow(all_df), sp))
  ttt<-sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results/%s/%s.N.csv", sp, sp)
  #ttt<-sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results_1/%s/%s.N.rda", sp, sp)
  ttt2<-sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results/%s/%s.DISTRIBUTION.csv", sp, sp)
  #ttt2<-sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results_1/%s/%s.DISTRIBUTION.rda", sp, sp)
  
  if (file.exists(ttt2)){
    size<-file.size(ttt2)
    if (size>100){
      #print(sprintf("rm -rf %s", ttt))
      next()
    }
    #print("skip")
    
  }
  #next()
  log<-sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results/%s/%s.sqlite", sp, sp)
  #log<-sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results/%s/%s.sqlite", sp, sp)
  
  if (!file.exists(log)){
    next()
  }
  
  sp.log<-sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results/%s/%s.sp.log", sp, sp)
  #sp.log<-sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results_1/%s/%s.sp.log", sp, sp)
  saveRDS(NULL, ttt2)
  
  mydb <- dbConnect(RSQLite::SQLite(), log)
  trees<-dbReadTable(mydb, "trees")
  dbDisconnect(mydb)
  
  text.string<-trees[1,2]
  text.string<-gsub("\\]", "#", gsub("\\[", "#", text.string))
  if (!grepl("\\(", text.string)){
    text.string<-sprintf("(a:0)%s", text.string)
  }
  vert.tree<-read.tree(text=text.string)
  #plot(vert.tree)
  #nodelabels()
  nodes<-data.table(label=c(vert.tree$node.label, vert.tree$tip.label),
                    type=c(rep("node", length(vert.tree$node.label)),
                           rep("leaf", length(vert.tree$tip.label)))
  )
  nodes<-nodes[label!="a"]
  if (nrow(nodes)==1){
    nodes$type<-"leaf"
  }
  nodes[, c("PX","PY") := data.table(str_split_fixed(label,"@",2))]
  nodes[, c("from","to") := data.table(str_split_fixed(PY,"-",2))]
  nodes$from<-as.numeric(nodes$from)
  nodes$to<-as.numeric(nodes$to)
  nodes$event<-"SPECIATION"
  nodes[(type=="leaf")&(to==0), "event"]<-"NONE"
  nodes[(type=="leaf")&(to!=0), "event"]<-"EXTINCTION"
  #df<-data.table(read.table(sprintf("../Results/%s/%s.log", sp, sp), head=F, sep=",", stringsAsFactors = F))
  #df<-data.table(read.table(sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results_1/%s/%s.log", sp, sp), 
  #                          head=F, sep=",", stringsAsFactors = F))
  df<-fread(sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results/%s/%s.log", sp, sp))
  if (nrow(df)==0){
    next()
  }
  
  colnames(df)<-c("year", "global_id", "group_id", "n", "sp_id", "suitable")
  df<-df[suitable==1]
  if (nrow(df)==0){
    next()
  }
  j=500
  
  df_with_lon_lat<-merge(df, mask, by="global_id")
  distribution_df<-df_with_lon_lat[, .(N_CELLS=.N,
                          MAX_LON=max(lon),
                          MEAN_LON=mean(lon),
                          MIN_LON=min(lon),
                          MAX_LAT=max(lat),
                          MAX_ABS_LAT=max(abs(lat)),
                          MEAN_LAT=mean(lat),
                          MIN_LAT=min(lat),
                          MIN_ABS_LAT=min(abs(lat)),
                          FROM_YEAR=max(year)), 
                          by=c("year", "sp_id")]
  
  
  event_df<-df[, .(N_SPECIES=length(unique(sp_id)),
                   N_SPECIATION=0,
                   N_EXTINCTION=0,
                   N_SPECIATION_YEAR=0,
                   N_EXTINCTION_YEAR=0), 
               by="year"]
  years<-data.table(year=c(0:1198))
  event_df<-merge(event_df, years, by="year", all=T)
  event_df[is.na(N_SPECIES)]$N_SPECIES<-0
  event_df[is.na(N_SPECIATION)]$N_SPECIATION<-0
  event_df[is.na(N_EXTINCTION)]$N_EXTINCTION<-0
  event_df[is.na(N_SPECIATION_YEAR)]$N_SPECIATION_YEAR<-0
  event_df[is.na(N_EXTINCTION_YEAR)]$N_EXTINCTION_YEAR<-0
  
  
  
  for (j in c(1198:0)){
    #sub_df<-df[(year==j),]
    sub_node<-nodes[to>=j]
    event_df[year==j]$N_SPECIATION<-nrow(sub_node[event=="SPECIATION"])
    event_df[year==j]$N_EXTINCTION<-nrow(sub_node[event=="EXTINCTION"])
    
    sub_node<-nodes[to==j]
    event_df[year==j]$N_SPECIATION_YEAR<-nrow(sub_node[event=="SPECIATION"])
    event_df[year==j]$N_EXTINCTION_YEAR<-nrow(sub_node[event=="EXTINCTION"])
    
  }
  saveRDS(event_df, ttt)
  saveRDS(distribution_df, ttt2)
  #saveRDS(distribution_df, sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results_1/%s/%s.DISTRIBUTION.rda", sp, sp))
  
  #g4<-ggtree(vert.tree, root.position = vert.tree$root.edge)+theme_tree2()+xlim(0, 1200)
  #g4
}
#saveRDS(event_df, "../Data/Sample_N.rda")
event_df<-readRDS("../Data/Sample_N.rda")

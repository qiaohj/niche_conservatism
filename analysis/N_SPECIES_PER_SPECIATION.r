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
#library(plotKML)
library(ggtree)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
setDTthreads(1)
print(sprintf("Number of core(s) is(are) %d.", getDTthreads()))

base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)


i=233

#simulations<-simulations[which(((simulations$nb=="BROAD")&(simulations$da=="GOOD"))),]
#simulations<-simulations[which(simulations$species_evo_level==0),]
simulations<-simulations[which(simulations$is_run==1),]
#simulations<-simulations[which(simulations$species_evo_type %in% c(4)),]

table(simulations$species_evo_type)

table(simulations[, c("nb", "da")])


template<-"%d_%s_%s_%d_%s_%d"
all_df<-simulations
all_df<-all_df[sample(nrow(all_df), nrow(all_df)),]
#item<-all_df[nb=="BROAD"&da=="GOOD"&global_id==1361&species_evo_type==2&directional_speed==0.1]
#item<-all_df[nb=="BROAD"&da=="GOOD"&global_id==1567&species_evo_type==2&directional_speed==0.1]
i=68
all_df<-all_df[which(all_df$species_evo_level==0),]
table(all_df$species_evo_type)
table(all_df$species_evo_level)
df_all<-list()
for (i in c(1:nrow(all_df))){
  
  
  item<-all_df[i,]
  
  sp<-sprintf(template, item$global_id, item$da, item$nb, item$species_evo_type, 
              item$directional_speed, item$species_evo_level)
  print(paste(i, nrow(all_df), sp))
  
  
  base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"
  
  log<-sprintf("%s/%s/%s.sqlite", base, sp, sp)
  
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
  nodes_speciation<-nodes[event=="SPECIATION"]
  if (nrow(nodes_speciation)==0){
    next()
  }
  N_SPECIATION<-nrow(nodes_speciation)
  all_new_Species<-0
  for (j in c(1:N_SPECIATION)){
    p_to<-nodes_speciation[j]$to
    sp<-nodes[from==p_to]
    all_new_Species<-all_new_Species+nrow(sp)
  }
  item_df<-data.table(species_evo_type=item$species_evo_type,
                      directional_speed=item$directional_speed,
                      nb=item$nb,
                      da=item$da,
                      global_id=item$global_id,
                      N_SPECIATION=N_SPECIATION,
                      N_NEW_SPECIES=all_new_Species,
                      SPECIES_PER_SPECIATION=all_new_Species/N_SPECIATION)
  df_all[[i]]<-item_df
}
df_all_dt<-rbindlist(df_all)
saveRDS(df_all_dt, "../Data/N_speciation_extinction/N_species_per_speciation.rda")

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
i=1

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
      base<-"../Results"
    }else{
      base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"
    }
  }
  
  ttt<-sprintf("%s/%s/%s.N.rda", base, sp, sp)
  
  if (file.exists(ttt)){
    print("exist skip")
    next()
  }
  
  log<-sprintf("%s/%s/%s.sqlite", base, sp, sp)
  
  if (!file.exists(log)){
    print("no log, skip")
    next()
  }
  
  saveRDS(NULL, ttt)
  
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
  
  event_df<-data.table(year=c(0:1199),
                   N_SPECIES=0,
                   N_SPECIATION=0,
                   N_EXTINCTION=0,
                   N_SPECIATION_YEAR=0,
                   N_EXTINCTION_YEAR=0)
  
  
  event_df[year==1199]$N_SPECIES<-1
  for (j in c(1198:0)){
    #sub_df<-df[(year==j),]
    event_df[year==j]$N_SPECIES<-nrow(nodes[from>=j&to<=j])
    event_df[year==j]
    sub_node<-nodes[to>=j]
    event_df[year==j]$N_SPECIATION<-nrow(sub_node[event=="SPECIATION"])
    event_df[year==j]$N_EXTINCTION<-nrow(sub_node[event=="EXTINCTION"])
    
    sub_node<-nodes[to==j]
    event_df[year==j]$N_SPECIATION_YEAR<-nrow(sub_node[event=="SPECIATION"])
    event_df[year==j]$N_EXTINCTION_YEAR<-nrow(sub_node[event=="EXTINCTION"])
    
  }
  saveRDS(event_df, ttt)
}


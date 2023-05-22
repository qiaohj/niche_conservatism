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
i=11000
all_df<-all_df[which(all_df$species_evo_level==0),]
table(all_df$species_evo_type)
table(all_df$species_evo_level)
for (i in c(1:nrow(all_df))){
  
  
  item<-all_df[i,]
  
  sp<-sprintf(template, item$global_id, item$da, item$nb, item$species_evo_type, 
              item$directional_speed, item$species_evo_level)
  print(paste(i, nrow(all_df), sp))
  
  
  base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"
  
  ttt<-sprintf("%s/%s/%s.N.speciation.extinction_rolling_windows.rda", base, sp, sp)
  
  if (file.exists(ttt)){
    print("skip")
    #next()
    size<-file.size(ttt)
    if (size>100){
      
      #print(sprintf("rm -rf %s", ttt))
      next()
    }
    print("redo")
    #next()
  }
  saveRDS(NULL, ttt)
  
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
  nodes$from<-nodes$from*-1
  nodes$to<-nodes$to*-1
  event_df<-data.table(from=seq(from=-1100, to=0, by=10)-100,
                       to=seq(from=-1100, to=0, by=10),
                       N_SPECIES=0,
                       N_SPECIATION=0,
                       N_EXTINCTION=0)
  
  
  event_df[from==-1200]$N_SPECIES<-1
  
  
  j=-680
  for (j in seq(from=-1100, to=0, by=10)){
    from_y<-j-100
    to_y<-j
    #sub_df<-df[(year==j),]
    subnodes<-nodes[between(from, from_y, to_y) | between(to, from_y, to_y) |
                     (from_y>=from & from_y<=to) | (to_y>=from & to_y<=to)]
    
    
    event_df[from==from_y]$N_SPECIES<-nrow(subnodes)
    
    subnodes<-nodes[between(to, from_y, to_y)]
    event_df[from==from_y]$N_SPECIATION<-nrow(subnodes[event=="SPECIATION"])
    event_df[from==from_y]$N_EXTINCTION<-nrow(subnodes[event=="EXTINCTION"])
    
    
    
  }
  event_df$N_SPECIES <- event_df$N_SPECIES - event_df$N_SPECIATION
  saveRDS(event_df, ttt)
}


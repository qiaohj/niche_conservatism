library(phytools)
library(TreePar)
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
select.tip.or.node <- function(element, tree) {
  ifelse(element < Ntip(tree)+1,
         tree$tip.label[element],
         tree$node.label[element-Ntip(tree)])
}

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
  
  ttt<-sprintf("%s/%s/%s.net_dr_tree_based.rda", base, sp, sp)
  
  if (file.exists(ttt)){
    print("skip")
    next()
    size<-file.size(ttt)
    if (size>100){
      
      #print(sprintf("rm -rf %s", ttt))
      next()
    }
    print("redo")
    #next()
  }
  
  #saveRDS(NULL, ttt)
  
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
  nodes<-data.table(label=c(vert.tree$tip.label, vert.tree$node.label),
                    type=c(rep("leaf", length(vert.tree$tip.label)),
                           rep("node", length(vert.tree$node.label))
                    )
  )
  nodes$id<-c(1:nrow(nodes))
  
  nodes<-nodes[label!="a"]
  if (nrow(nodes)==1){
    nodes$type<-"leaf"
  }
  nodes[, c("PX","PY") := data.table(str_split_fixed(label,"@",2))]
  nodes[, c("from","to") := data.table(str_split_fixed(PY,"-",2))]
  nodes$from<-as.numeric(nodes$from)
  nodes$to<-as.numeric(nodes$to)
  nodes[is.na(to)]$to<-0
  nodes$event<-"SPECIATION"
  nodes[(type=="leaf")&(to==0), "event"]<-"NONE"
  nodes[(type=="leaf")&(to!=0), "event"]<-"EXTINCTION"
  if (nrow(nodes[event=="EXTINCTION"])>10){
    asdf
  }
  next()
  #plot(vert.tree)
  #nodelabels()
  
  # make tree binary and ultrametric
  binultra <- multi2di(force.ultrametric(vert.tree, method = "extend"))
  birthdeath(binultra)
  fit.bd(binultra)
  # assume a near complete tree, rho[1]=0.95
  rho <- c(0.95,1)
  
  # set windows of 10k, starting 0, ending 120k
  grid <- -10
  start <- 1200
  end <- 0
  
  # Vector of speciation times in the phylogeny. Time is measured 
  # increasing going into the past with the present being time 0. 
  # x can be obtained from a phylogenetic tree using getx(TREE).
  x <- getx(binultra)
  
  # estimate time, lambda, mu
  res <- bd.shifts.optim(x, rho, grid, start, end)[[2]]
  #saveRDS(event_df, ttt)
}


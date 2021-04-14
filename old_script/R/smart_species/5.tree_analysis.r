library("DBI")
library("ape")
library("phangorn")
library("phytools")
library("geiger")
library("stringr")
library("tidyverse")

base<-"/home/huijieqiao/git/ees_3d_data/SMART_SPECIES/Results_TEST"
sims<-list.dirs(base, full.names=F)
sims[5]
sim<-sims[8]
result<-data.frame()
for (sim in sims){
  if (sim==""){
    next()
  }
  if (sim=="10715_GOOD_BROAD_4_1"){
    next()
  }
  str = strsplit(sim, "_")[[1]]
  id = str[1]
  da = str[2]
  nb = str[3]
  evo_type = str[4]
  evo_ratio = str[5]
  print(sim)
  mydb <- dbConnect(RSQLite::SQLite(), sprintf("%s/%s/%s.sqlite", base, sim, sim))  
  trees<-dbReadTable(mydb, "trees")
  dbDisconnect(mydb) 
  
  text.string<-trees[1,2]
  text.string<-gsub("\\]", "#", gsub("\\[", "#", text.string))
  if (!grepl("\\(", text.string)){
    text.string<-paste("()", text.string, sep="")
  }
  vert.tree<-read.newick(text=text.string)
  n_node<-length(vert.tree$node.label)
  n_leaf<-length(vert.tree$tip.label)
  item<-data.frame(id=id, da=da, nb=nb, evo_type=evo_type, evo_ratio=evo_ratio, n_node=n_node, n_leaf=n_leaf)
  if (nrow(result)==0){
    #result<-item
  }else{
    #result<-rbind(result, item)
  }
  if (n_leaf>0){
    plotTree(vert.tree, ftype="i")
    nodelabels(vert.tree$node.label)
  }
  x<-readline(prompt=sprintf("N Node:%d, N Leave:%d, (X=exit): ", n_node, n_leaf))
  if (tolower(x)=="x"){
    break()
  }
}


if (F){
  library("ape")
  library("phytools")
  t<-"SP10715 @ 1200-0:1200;"
  t<-"()SP10715 @ 1200-0:1200;"
  vert.tree<-read.newick(text=t)
}

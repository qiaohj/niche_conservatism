library("dplyr")
library("DBI")
library("phytools")
library("tidyr")
library("data.table")
library("tibble")
library("raster")
library("sp")
library("ggplot2")



base<-"/home/huijieqiao/git/ees_3d_data/niche_conservatism"
db_base<-"/home/huijieqiao/git/ees_3d_data/SMART_SPECIES"
base2<-"/media/huijieqiao/Butterfly/SMART_SPECIES"


#the rda file was generated by global_id_lonlat.r
mask_df<-readRDS(sprintf("%s/Data/ENV/mask_df.rda", base))

simulations<-NULL
for (i in c(1,2,7)){
  print(i)
  mydb <- dbConnect(RSQLite::SQLite(), sprintf("%s/conf_%s.sqlite", db_base, i))  
  simulation<-dbReadTable(mydb, "simulations")
  dbDisconnect(mydb) 
  if (is.null(simulations)){
    simulations<-simulation
  }else{
    simulations<-bind_rows(simulation, simulations)
  }
}
handle_label<-function(label){
  if (length(label)==4){
    label<-c(label, label[4])
  }
  label[c(1,4,5)]
}
simulations<-simulations %>% filter(nb!="BROAD")
simulations<-simulations %>% filter(is_run==1)

simulations<-simulations[sample(nrow(simulations), nrow(simulations)),]
for (i in c(1:nrow(simulations))){
  print(paste(i, nrow(simulations)))
  s<-simulations[i,]
  log<-sprintf("%s/RESULTS/%s/%s.log", base2, s$label, s$label)
  target<-sprintf("%s.env.rda", log)
  log_db<-sprintf("%s/RESULTS/%s/%s.sqlite", base2, s$label, s$label)
  mydb <- dbConnect(RSQLite::SQLite(), log_db)  
  trees<-dbReadTable(mydb, "trees")
  dbDisconnect(mydb)
  
  text.string<-trees[which(trees$TYPE=="NEWICK"), "CONTENT"]
  if (length(text.string)==0){
    text.string<-sprintf("SP%d @ 1200-1199:1;", s$global_id)
  }
  text.string<-gsub("\\]", "#", gsub("\\[", "#", text.string))
  if (!grepl("\\(", text.string)){
    text.string<-paste("()", text.string, sep="")
  }
  tree<-read.newick(text=text.string)
  
  #plotTree(tree)
  #nodelabels(text=tree$node.label)
  
  lll<-strsplit(tree$node.label, "[-\\@\\ ]")
  node_labels<-data.frame(stringsAsFactors = F,
                          matrix(unlist(purrr::map(lll, .f = ~ handle_label(.x))), ncol=3, byrow=T)
  )
  
  
  colnames(node_labels)<-c("sp_label", "from", "to")
  node_labels$from<-as.numeric(node_labels$from)
  node_labels$to<-as.numeric(node_labels$to)
  node_labels$type<-"NODE"
  node_labels$index<-Ntip(tree) +as.numeric(rownames(node_labels))
  
  
  if (length(tree$tip.label)>0){
    lll<-strsplit(tree$tip.label, "[-\\@\\ ]")
    tip_labels<-data.frame(stringsAsFactors = F,
                           matrix(unlist(purrr::map(lll, .f = ~ handle_label(.x))), ncol=3, byrow=T)
    )
    colnames(tip_labels)<-c("sp_label", "from", "to")
    tip_labels$from<-as.numeric(tip_labels$from)
    tip_labels$to<-as.numeric(tip_labels$to)
    tip_labels$type<-"LEAF"
    tip_labels$index<-as.numeric(rownames(tip_labels))
    node_labels<-bind_rows(tip_labels, node_labels)
  }else{
    node_labels$type<-"LEAF"
  }
  log_df<-readRDS(target)
  if (min(node_labels$to)!=min(log_df$Y)){
    aasdf
  }
  
  
}

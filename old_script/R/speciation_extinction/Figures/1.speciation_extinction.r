library("dplyr")
library("DBI")
library("phytools")
library("tidyr")
library("data.table")
library("tibble")

base<-"/home/huijieqiao/git/ees_3d_data/SMART_SPECIES"
base2<-"/media/huijieqiao/Butterfly/SMART_SPECIES"
simulations<-NULL
for (i in c(1,2,7)){
  print(i)
  mydb <- dbConnect(RSQLite::SQLite(), sprintf("%s/conf_%s.sqlite", base, i))  
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
cmd<-c()
i=15234
stat<-NULL
detail<-NULL
for (i in c(1:nrow(simulations))){
  print(paste(i, nrow(simulations)))
  s<-simulations[i,]
  log<-sprintf("%s/RESULTS/%s/%s.log", base2, s$label, s$label)
  
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
  n_speciation<-length(tree$node.label)-1
  
  lll<-strsplit(tree$node.label, "[-\\@\\ ]")
  node_labels<-data.frame(stringsAsFactors = F,
                         matrix(unlist(purrr::map(lll, .f = ~ handle_label(.x))), ncol=3, byrow=T)
  )
  
  colnames(node_labels)<-c("sp_label", "from", "to")
  node_labels$from<-as.numeric(node_labels$from)
  node_labels$to<-as.numeric(node_labels$to)
  node_labels$type<-"NODE"
  
  tip_labels<-NULL
  if (length(tree$tip.label)>0){
    lll<-strsplit(tree$tip.label, "[-\\@\\ ]")
    tip_labels<-data.frame(stringsAsFactors = F,
                           matrix(unlist(purrr::map(lll, .f = ~ handle_label(.x))), ncol=3, byrow=T)
    )
    colnames(tip_labels)<-c("sp_label", "from", "to")
    tip_labels$from<-as.numeric(tip_labels$from)
    tip_labels$to<-as.numeric(tip_labels$to)
    tip_labels$type<-"LEAF"
    last_labels<-tip_labels$to
  }else{
    last_labels<-node_labels$to
  }
  n_leaves<-length(last_labels)
  n_extinct<-length(last_labels[last_labels!="0"])
  s<-s[, c("global_id", "da", "nb", "evo_type", "niche_envolution_individual_ratio")]
  s$n_speciation<-n_speciation
  s$n_leaves<-n_leaves
  s$n_extinct<-n_extinct
  if (is.null(tip_labels)){
    all_nodes<-node_labels
  }else{
    all_nodes<-dplyr::bind_rows(node_labels, tip_labels)
  }
  
  
  
  all_nodes$global_id<-s$global_id
  all_nodes$da<-s$da
  all_nodes$nb<-s$nb
  all_nodes$evo_type<-s$evo_type
  all_nodes$evo_ratio<-s$niche_envolution_individual_ratio
  
  if (is.null(stat)){
    stat<-s
    detail<-all_nodes
  }else{
    stat<-bind_rows(stat, s)
    detail<-bind_rows(detail, all_nodes)
  }
}
colnames(stat)[5]<-"evo_ratio"
stat[which(stat$evo_ratio==0.01), "evo_ratio"]<-0.005
stat[which(stat$evo_ratio==0.1), "evo_ratio"]<-0.05
detail[which(detail$evo_ratio==0.01), "evo_ratio"]<-0.005
detail[which(detail$evo_ratio==0.1), "evo_ratio"]<-0.05

saveRDS(stat, sprintf("%s/Tables/Speciation_Extinction/stat.rda", base))
saveRDS(detail, sprintf("%s/Tables/Speciation_Extinction/detail.rda", base))



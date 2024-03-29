base<-"/home/huijieqiao/git/ees_3d_data/niche_conservatism"
db_base<-"/home/huijieqiao/git/ees_3d_data/SMART_SPECIES"
base2<-"/media/huijieqiao/Butterfly/SMART_SPECIES"
args = commandArgs(trailingOnly=TRUE)
start=as.numeric(args[1])
end=as.numeric(args[2])

t_f<-sprintf("%s/Data/items_with_NA/stat_%d_%d.rda", base, start, end)
skip<-T
if (file.exists(t_f)){
  skip<-T
  if (F){
    test_df<-readRDS(t_f)
    if (is.na(test_df)){
      skip<-F
    }else{
      if (length(unique(test_df$global_id))<5){
        skip<-F
      }
    }
  }
  
}else{
  skip<-F
}
if (skip){
  skipasdfasdf
}
saveRDS(NA, sprintf("%s/Data/items_with_NA/stat_%d_%d.rda", base, start, end))

library("dplyr")
library("DBI")
library("phytools")
library("tidyr")
library("data.table")
library("tibble")
library("raster")
library("sp")
library("ggplot2")
library("sgeostat")
library("mapview")
library("concaveman")

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
cmd<-c()


getID<-function(label){
  gsub("SP", "", strsplit(label, " ")[[1]][1])
}
getSPID<-function(tree, node){
  if (is.null(node)){
    return("")
  }
  if (node>Ntip(tree)){
    label_index<-node - Ntip(tree)
    node_type<-"node"
    label<-tree$node.label[label_index]
  }else{
    label_index<-node
    node_type<-"tip"
    label<-tree$tip.label[label_index]
  }
  label<-getID(label)
  parent<-getParent(tree, node)
  if (is.null(parent)){
    return(getID(tree$node.label[node - Ntip(tree)]))
  }else{
    return(sprintf("%s-%s", getSPID(tree, parent),
                   label))
  }
}
#node<-Ntip(tree)+3
#getSPID(tree, Ntip(tree)+3)
#getSPID(tree, Ntip(tree))
#getSPID(tree, 1)
args = commandArgs(trailingOnly=TRUE)
start=as.numeric(args[1])
end=as.numeric(args[2])

if (F){
  if (start==1){
    stat<-NULL
    detail<-NULL
    speciation_df<-NULL
    extinction_df<-NULL
    sp_character<-NULL
  }else{
    stat<-readRDS(sprintf("%s/Data/stat.rda", base))
    detail<-readRDS(sprintf("%s/Data/detail.rda", base))
    sp_character<-readRDS(sprintf("%s/Data/sp_character.rda", base))
    extinction_df<-readRDS(sprintf("%s/Data/extinction_df.rda", base))
    speciation_df<-readRDS(sprintf("%s/Data/speciation_df.rda", base))
    stat<-stat%>%dplyr::filter(global_id!=start)
    detail<-detail%>%dplyr::filter(global_id!=start)
    sp_character<-sp_character%>%dplyr::filter(global_id!=start)
    extinction_df<-extinction_df%>%dplyr::filter(ID!=start)
    speciation_df<-speciation_df%>%dplyr::filter(global_id!=start)
  }
}
if (F){
  stat[which(is.na(stat$evo_ratio)),]
  stat$label<-paste(stat$global_id, stat$da, stat$nb, stat$evo_type, stat$evo_ratio, sep="_")
  for (i in c(1:nrow(simulations))){
    s<-simulations[i,]
    if (s$label %in% stat$label){
      print(paste(i, "yes"))
    }else{
      asdf
    }
  }
}
stat<-NULL
detail<-NULL
speciation_df<-NULL
extinction_df<-NULL
sp_character<-NULL
i=18313 #three node
i=2701 #for debug
for (i in c(start:end)){
  print(paste(i, start, end))
  s<-simulations[i,]
  nb<-strsplit(s$nb_v, "\\|")[[1]]
  nb_temp<-as.numeric(strsplit(nb[1], ",")[[1]])
  nb_prec<-as.numeric(strsplit(nb[3], ",")[[1]])
  log<-sprintf("%s/RESULTS/%s/%s.log.env.rda", base2, s$label, s$label)
  log_df<-readRDS(log)
  
  log_db<-sprintf("%s/RESULTS/%s/%s.sqlite", base2, s$label, s$label)
  mydb <- dbConnect(RSQLite::SQLite(), log_db)  
  trees<-dbReadTable(mydb, "trees")
  dbDisconnect(mydb)
  
  text.string<-trees[which(trees$TYPE=="NEWICK"), "CONTENT"]
  if (length(text.string)==0){
    text.string<-sprintf("SP%d @ %d-%d:%d;", s$global_id, max(log_df$Y), min(log_df$Y),
                         max(log_df$Y)- min(log_df$Y))
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
  
  node_labels<-node_labels %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(SP=getSPID(tree, index),
           PARENT=getSPID(tree, getParent(tree, index)))
  if (min(node_labels$to)!=min(log_df$Y)){
    aasdf
  }
  
  n_speciation<-nrow(node_labels%>%dplyr::filter(type=="NODE"))
  n_extinction<-nrow(node_labels%>%dplyr::filter((type=="LEAF")&(to!=0)))
  
  s<-s[, c("global_id", "da", "nb", "evo_type", "niche_envolution_individual_ratio")]
  s$n_speciation<-n_speciation
  s$n_extinction<-n_extinction
  
  node_labels$SEED_ID<-s$global_id
  node_labels$DA<-s$da
  node_labels$NB<-s$nb
  node_labels$EVO_TYPE<-s$evo_type
  node_labels$EVO_RATIO<-s$niche_envolution_individual_ratio
  
  if (is.null(stat)){
    stat<-s
    detail<-node_labels
  }else{
    stat<-bind_rows(stat, s)
    detail<-bind_rows(detail, node_labels)
  }
  #Area, Niche Breadth, Lat, Lon Range
  dis_suitable<-log_df%>%dplyr::filter(SUITABLE==1)
  if (nrow(dis_suitable)>0){
    dis_n<-dis_suitable%>%dplyr::group_by(Y, SP_ID)%>%
      dplyr::summarise(AREA=n(),
                       MIN_TEMP=min(MIN_TEMP, na.rm = T),
                       MAX_MIN_TEMP=max(MIN_TEMP, na.rm = T),
                       MIN_MAX_TEMP=min(MAX_TEMP, na.rm = T),
                       MAX_TEMP=max(MAX_TEMP, na.rm = T),
                       MIN_PREC=min(MAX_PREC, na.rm = T),
                       MAX_PREC=max(MAX_PREC, na.rm = T),
                       MIN_LAT=min(lat, na.rm = T),
                       MAX_LAT=max(lat, na.rm = T),
                       MIN_LON=min(lon, na.rm = T),
                       MAX_LON=max(lon, na.rm = T))
    dis_n$RANG_TEMP<-dis_n$MAX_TEMP-dis_n$MIN_TEMP
    dis_n$RANG_PREC<-dis_n$MAX_PREC-dis_n$MIN_PREC
    dis_n$RANG_LON<-dis_n$MAX_LON-dis_n$MIN_LON
    dis_n$RANG_LAT<-dis_n$MAX_LAT-dis_n$MIN_LAT
    dis_n$SEED_ID<-s$global_id
    dis_n$DA<-s$da
    dis_n$NB<-s$nb
    dis_n$EVO_TYPE<-s$evo_type
    dis_n$EVO_RATIO<-s$niche_envolution_individual_ratio
    if (is.null(sp_character)){
      sp_character<-dis_n
    }else{
      sp_character<-bind_rows(sp_character, dis_n)
    }
  }
  keys<-dis_suitable %>%ungroup()%>%
    group_by(Y)%>%group_keys()
  dis_suitable_year <- dis_suitable %>%ungroup()%>%
    group_by(Y)%>%group_split()%>%setNames(pull(keys))
  
  j=390
  for (j in c(1:nrow(node_labels))){
    
    node<-node_labels[j,]
    
    #Speciation
    if (node$type=="NODE"){
      
      item<-node_labels%>%dplyr::filter(PARENT==node$SP)
      dis<-dis_suitable_year[[as.character(node$to)]]%>%dplyr::filter(SP_ID %in% item$SP)
      SP_IDS<-unique(dis$SP_ID)
      print(paste(i, start, end, j, nrow(node_labels), "SPECIATION. N_SPs:", length(SP_IDS)))
      if (length(SP_IDS)<2){
        print("SPECIES NUMBER ERROR")
        next()
      }
      range_lon<-range(dis$lon)
      range_lat<-range(dis$lat)
      potential_ba<-mask_df%>%dplyr::filter(between(lon, range_lon[1], range_lon[2])&
                                               between(lat, range_lat[1], range_lat[2])&
                                               ((Y==node$to)|(is.na(Y))))
      potential_ba$Y<-node$to
      potential_ba$in_sp<-0
      for (sp_id in SP_IDS){
        sp1<-data.frame(dis%>%dplyr::filter(SP_ID==sp_id))
        
        concave1<-concaveman(as.matrix(sp1[, c("lon", "lat")]))
        potential_ba$in_sp<-potential_ba$in_sp+point.in.polygon(potential_ba$lon, potential_ba$lat, 
                                                                concave1[,1], concave1[,2])
      }
      concave<-concaveman(as.matrix(dis[, c("lon", "lat")]))
      potential_ba$in_all_sp<-point.in.polygon(potential_ba$lon, potential_ba$lat, 
                                           concave[,1], concave[,2])
      
      potential_ba<-potential_ba%>%dplyr::filter((in_all_sp>0)&(in_sp==0))
      
      if (nrow(potential_ba)==0){
        next()
      }
      
      potential_ba$MIN_TEMP_IN<-between(potential_ba$MIN_TEMP, nb_temp[1], nb_temp[2])
      potential_ba$MAX_TEMP_IN<-between(potential_ba$MAX_TEMP, nb_temp[1], nb_temp[2])
      potential_ba$MAX_PREC_IN<-between(potential_ba$MAX_PREC, nb_temp[1], nb_temp[2])
      potential_ba$SEED_ID<-s$global_id
      potential_ba$DA<-s$da
      potential_ba$NB<-s$nb
      potential_ba$EVO_TYPE<-s$evo_type
      potential_ba$EVO_RATIO<-s$niche_envolution_individual_ratio
      if(F){
        #ggplot(data=dis, aes(x=lon, y=lat))+
        #  geom_point(data=potential_ba%>%filter(in_sp>0), color="green")+
        #  geom_point(data=potential_ba%>%filter(in_sp1>0), color="red")+
        #  geom_point(data=potential_ba%>%filter(in_sp2>0), color="blue")
        ggplot(data=dis, aes(x=lon, y=lat))+geom_point(aes(color=factor(SP_ID)))+
          geom_point(data=potential_ba, color="blue")
          
      }
      if (is.null(speciation_df)){
        speciation_df<-potential_ba
      }else{
        speciation_df<-bind_rows(speciation_df, potential_ba)
      }
    }
    #Extinction
    if ((node$type=="LEAF")&(node$to!=0)){
      print(paste(i, start, end, j, nrow(node_labels), "EXTINCTION"))
      dis<-log_df%>%dplyr::filter((Y==node$to)&(SP_ID==node$SP))
      dis$MIN_TEMP_IN<-between(dis$MIN_TEMP, nb_temp[1], nb_temp[2])
      dis$MAX_TEMP_IN<-between(dis$MAX_TEMP, nb_temp[1], nb_temp[2])
      dis$MAX_PREC_IN<-between(dis$MAX_PREC, nb_temp[1], nb_temp[2])
      
      dis$SEED_ID<-s$global_id
      dis$DA<-s$da
      dis$NB<-s$nb
      dis$EVO_TYPE<-s$evo_type
      dis$EVO_RATIO<-s$niche_envolution_individual_ratio
      if (is.null(extinction_df)){
        extinction_df<-dis
      }else{
        extinction_df<-bind_rows(extinction_df, dis)
      }
    }
  }
  
  #saveRDS(stat, sprintf("%s/Data/stat.rda", base))
  #saveRDS(detail, sprintf("%s/Data/detail.rda", base))
  #saveRDS(sp_character, sprintf("%s/Data/sp_character.rda", base))
  #saveRDS(extinction_df, sprintf("%s/Data/extinction_df.rda", base))
  #saveRDS(speciation_df, sprintf("%s/Data/speciation_df.rda", base))
  
  
}

colnames(stat)[5]<-"evo_ratio"
if (F){
  
}

saveRDS(stat, sprintf("%s/Data/items_with_NA/stat_%d_%d.rda", base, start, end))
saveRDS(detail, sprintf("%s/Data/items_with_NA/detail_%d_%d.rda", base, start, end))
saveRDS(sp_character, sprintf("%s/Data/items_with_NA/sp_character_%d_%d.rda", base, start, end))
saveRDS(extinction_df, sprintf("%s/Data/items_with_NA/extinction_df_%d_%d.rda", base, start, end))
saveRDS(speciation_df, sprintf("%s/Data/items_with_NA/speciation_df_%d_%d.rda", base, start, end))

if (F){
  cc<-c()
  for (f in seq(from=1, to=20000, by=1000)){
    cc<-c(cc, sprintf("Rscript get_data_by_id_range.r %d %d", f, f+999))
  }
  
  df<-readRDS("/home/huijieqiao/git/ees_3d_data/niche_conservatism/Data/items_with_NA/speciation_df_1_100.rda")
  head(df[which(is.na(df$MAX_PREC)),])
}

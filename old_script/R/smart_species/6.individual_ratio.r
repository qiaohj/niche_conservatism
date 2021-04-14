library("dplyr")
library("DBI")
setwd("~/git/ees_3d/R/smart_species")
args = commandArgs(trailingOnly=TRUE)
iii=8
iii = args[1]
base<-"/home/huijieqiao/git/ees_3d_data/SMART_SPECIES"

if (F){
  target<-sprintf("%s/Tables/individual_ratio.rda", base)
  result<-readRDS(target)
  #item2<-result%>%distinct(DA, NB, EVO_TYPE, GLOBAL_ID, EVO_RATIO)%>%dplyr::group_by(EVO_TYPE)%>%dplyr::count()
  item<-unique(result[, c("DA", "NB", "EVO_TYPE", "GLOBAL_ID", "EVO_RATIO")])
  head(item)
  item$label<-paste( item$EVO_TYPE, item$EVO_RATIO)
  print(table(item$EVO_TYPE))
  j=1
  for (j in unique(item$EVO_TYPE)){
    print(j)
    sub<-result %>% filter(EVO_TYPE==j)
    target<-sprintf("%s/Tables/individual_ratio_%s.rda", base, j)
    saveRDS(sub, target)
  }
  
}
mydb <- dbConnect(RSQLite::SQLite(), sprintf("%s/conf_%s.sqlite", base, iii))  
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb) 

#mydb <- dbConnect(RSQLite::SQLite(), sprintf("%s/conf_combine.sqlite", base))  
#simulations2<-dbReadTable(mydb, "simulations")
#simulations<-bind_rows(simulations2, simulations)
#dbDisconnect(mydb) 

simulations<-simulations %>% filter(nb!="BROAD")
simulations<-simulations %>% filter(is_run==1)

simulations<-simulations[sample(nrow(simulations)),]

target<-sprintf("%s/Tables/individual_ratio_%s.rda", base, iii)
if (file.exists(target)){
  result<-readRDS(target)
}else{
  result<-data.frame()
}

#test<-result %>% filter(EVO_TYPE==3)
#dim(test)
#result<-result %>% filter(EVO_TYPE!=3)
print(result)
finished<-unique(result$LABLE)
base2<-"/media/huijieqiao/Butterfly/SMART_SPECIES"
base3<-"/mnt/sshftps"
for (i in c(1:nrow(simulations))){
  s<-simulations[i,]
  if (s$label %in% finished){
    next()
  }
  finished<-c(finished, s$label)
  log1<-sprintf("%s/RESULTS/%s/%s.log", base2, s$label, s$label)
  log2<-sprintf("%s/RESULTS/%s/%s.log", base2, s$label, s$label)
  log3<-sprintf("%s/RESULTS/%s/%s.log", base2, s$label, s$label)
  #log3<-sprintf("%s/%s/%s.log", base3, s$label, s$label)
  log<-NULL
  if (file.exists(log1)){
    log<-log1
  }
  if (file.exists(log2)){
    log<-log2
  }
  if (file.exists(log3)){
    log<-log3
  }
  if (is.null(log)){
    next()
  }
  print(paste(i, nrow(simulations), s$label))
  log_db<-read.table(log, head=F, sep=",", stringsAsFactors = F)
  colnames(log_db)<-c("Y", "ID", "GROUP", "N", "SP_ID", "SUITABLE")
  
  log_db$SP_ID<-as.character(log_db$SP_ID)
  log_db<-as_tibble(log_db)
  
  individual_ratio<-log_db %>% dplyr::group_by(Y, SUITABLE) %>% dplyr::summarise(N_IND=sum(N))
  #sp_N<-log_db %>% dplyr::summarise(N_SP=length(unique(SP_ID)), .groups=Y)
  sp_N<-log_db %>% dplyr::group_by(Y)%>%dplyr::summarise(N_SP=length(unique(SP_ID)))
  cell_N<-log_db %>% dplyr::group_by(Y, SUITABLE) %>% dplyr::count()
  
  unique_cell_N<-log_db%>%dplyr::group_by(Y, SUITABLE)%>%distinct(ID)%>%dplyr::count()
  colnames(cell_N)[3]<-"N_CELL"
  colnames(unique_cell_N)[3]<-"N_UNIQUE_CELL"
  item<-inner_join(individual_ratio, sp_N, by="Y")
  item<-inner_join(item, cell_N, by=c("Y", "SUITABLE"))
  item<-inner_join(item, unique_cell_N, by=c("Y", "SUITABLE"))
  item$LABLE<-s$label
  item$NB<-s$nb
  item$DA<-s$da
  ff<-strsplit(s$label, "_")[[1]]
  item$EVO_TYPE<-as.numeric(ff[4])
  item$GLOBAL_ID<-s$global_id
  item$EVO_RATIO<-s$niche_envolution_individual_ratio
  if (nrow(result)==0){
    result<-item
  }else{
    result<-bind_rows(result, item)
  }
  if (F){
    if ((i %% 1000)==1){
      print("write data frame")
      saveRDS(result, target)
    }
  }
}
saveRDS(result, target)

item<-unique(result[, c("DA", "NB", "EVO_TYPE", "GLOBAL_ID", "EVO_RATIO")])
head(item)
item$label<-paste( item$EVO_TYPE, item$EVO_RATIO)
print(table(item$EVO_TYPE))

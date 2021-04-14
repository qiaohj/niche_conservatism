library("dplyr")
library("DBI")
print(mem.maxVSize())
print(mem.maxNSize())
print("----------------")
print(Sys.getenv('R_MAX_VSIZE'))
print(Sys.getenv('R_MAX_NSIZE'))

setwd("~/git/ees_3d/R/smart_species")
args = commandArgs(trailingOnly=TRUE)
iii=8
iii = args[1]
start = as.numeric(args[2])
if (is.na(start)){
  start<-1
}
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
if (F){
  env <- dbConnect(RSQLite::SQLite(), sprintf("%s/ISEA3H8/SQLITE/env_Hadley3D.sqlite", base))  
  min_temp<-dbReadTable(env, "Debiased_Minimum_Monthly_Temperature")
  colnames(min_temp)<-c("ID", "MIN_TEMP", "Y")
  max_temp<-dbReadTable(env, "Debiased_Maximum_Monthly_Temperature")
  colnames(max_temp)<-c("ID", "MAX_TEMP", "Y")
  max_prec<-dbReadTable(env, "Debiased_Maximum_Monthly_Precipitation")
  colnames(max_prec)<-c("ID", "MAX_PREC", "Y")
  dbDisconnect(env) 
  saveRDS(min_temp, sprintf("%s/Tables/ENV/min_temp.rda", base))
  saveRDS(max_temp, sprintf("%s/Tables/ENV/max_temp.rda", base))
  saveRDS(max_prec, sprintf("%s/Tables/ENV/max_prec.rda", base))
}
min_temp<-readRDS(sprintf("%s/Tables/ENV/min_temp.rda", base))
max_temp<-readRDS(sprintf("%s/Tables/ENV/max_temp.rda", base))
max_prec<-readRDS(sprintf("%s/Tables/ENV/max_prec.rda", base))

#mydb <- dbConnect(RSQLite::SQLite(), sprintf("%s/conf_combine.sqlite", base))  
#simulations2<-dbReadTable(mydb, "simulations")
#simulations<-bind_rows(simulations2, simulations)
#dbDisconnect(mydb) 

simulations<-simulations %>% filter(nb!="BROAD")
simulations<-simulations %>% filter(is_run==1)

simulations<-simulations[sample(nrow(simulations)),]

target<-sprintf("%s/Tables/individual_ratio_nb_%s.rda", base, iii)
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
for (i in c(start:nrow(simulations))){
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
  nb_rda<-sprintf("%s/RESULTS/%s/%s_nb.rda", base2, s$label, s$label)
  nb_log_rda<-sprintf("%s/RESULTS/%s/%s_log_nb.rda", base2, s$label, s$label)
  if (file.exists(nb_rda)){
    next()
    print(nb_rda)
    sp_niche<-readRDS(nb_rda)
    
  }else{
    sp_niche<-NULL
  }
    if (!is.null(sp_niche)){
      next()
    }
  saveRDS(sp_niche, nb_rda)
    #print(system.time({
    print("Reading data")
    print(log)
    log_db<-read.table(log, head=F, sep=",", stringsAsFactors = F)
    
    colnames(log_db)<-c("Y", "ID", "GROUP", "N", "SP_ID", "SUITABLE")
    log_db$SP_ID<-as.character(log_db$SP_ID)
    log_db<-as_tibble(log_db)
    log_db<-log_db%>%filter(SUITABLE==1)
    if (nrow(log_db)==0){
      next()
    }
    #}))
    #print(system.time({
    print(sprintf("Merging data, %d records", nrow(log_db)))
    
    min_temp_item<-min_temp %>%filter(ID %in% log_db$ID)
    max_temp_item<-max_temp %>%filter(ID %in% log_db$ID)
    max_prec_item<-max_prec %>%filter(ID %in% log_db$ID)
    
    log_db<-dplyr::left_join(log_db, min_temp_item, by=c("Y", "ID"))
    log_db<-dplyr::left_join(log_db, max_temp_item, by=c("Y", "ID"))
    log_db<-dplyr::left_join(log_db, max_prec_item, by=c("Y", "ID"))
    saveRDS(log_db, nb_log_rda)
    sp_niche<-log_db%>%dplyr::group_by(Y, SP_ID)%>%
      dplyr::summarise(TEMP_LOW=min(MIN_TEMP),
                       MIN_TEMP_HIGH=max(MIN_TEMP),
                       MAX_TEMP_LOW=min(MAX_TEMP),
                       TEMP_HIGH=max(MAX_TEMP),
                       PREC_LOW=min(MAX_PREC),
                       PREC_HIGH=max(MAX_PREC))
    #}))
    #print(system.time({
    print("Calculating Metrics")
    sp_niche$TEMP_RANGE<-sp_niche$TEMP_HIGH-sp_niche$TEMP_LOW
    sp_niche$PREC_RANGE<-sp_niche$PREC_HIGH-sp_niche$PREC_LOW
    #hist(sp_niche$TEMP_RANGE)
    #CI(sp_niche[which(sp_niche$Y==0),]$TEMP_LOW, ci=0.95)[3]
    #sd(sp_niche[which(sp_niche$Y==0),]$TEMP_LOW)
    
    sp_niche$LABLE<-s$label
    sp_niche$NB<-s$nb
    sp_niche$DA<-s$da
    ff<-strsplit(s$label, "_")[[1]]
    sp_niche$EVO_TYPE<-as.numeric(ff[4])
    sp_niche$GLOBAL_ID<-s$global_id
    sp_niche$EVO_RATIO<-s$niche_envolution_individual_ratio
    #}))
    #print(system.time({
    print("Saving")
    saveRDS(sp_niche, nb_rda)
    #}))
    log_db<-NULL
    min_temp_item<-NULL
    max_temp_item<-NULL
    max_prec_item<-NULL
    gcinfo(verbose = FALSE) #-- don't show it anymore
    print(gc(TRUE))
    
  }


if (F){
  if (nrow(result)==0){
    result<-sp_niche
  }else{
    result<-bind_rows(result, sp_niche)
  }
  saveRDS(result, target)
}


library("dplyr")
library("DBI")
base<-"/home/huijieqiao/git/ees_3d_data/SMART_SPECIES"
base2<-"/media/huijieqiao/Butterfly/SMART_SPECIES"
simulations<-NULL
for (i in c(1:8)){
#for (i in c(4:4)){
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
simulations<-simulations %>% filter(nb!="BROAD")
simulations<-simulations %>% filter(is_run==1)
cmd<-c()
for (i in c(1:nrow(simulations))){
  print(paste(i, nrow(simulations)))
  s<-simulations[i,]
  if (!file.exists(sprintf("%s/RESULTS/%s", base2, s$label))){
    next()
  }
  log1<-sprintf("%s/RESULTS/%s/%s.log", base2, s$label, s$label)
  if (!file.exists(log1)){
    cmd<-c(cmd, sprintf("rm -rf %s/RESULTS/%s", base2, s$label))
    next()
  }
  if (F){
    if (s$evo_type!=3){
      log_db<-sprintf("%s/RESULTS/%s/%s.sqlite", base2, s$label, s$label)
      mydb <- dbConnect(RSQLite::SQLite(), log_db)  
      trees<-dbReadTable(mydb, "trees")
      if (nrow(trees)==0){
        cmd<-c(cmd, sprintf("rm -rf %s/RESULTS/%s", base2, s$label))
      }
      dbDisconnect(mydb) 
    }
  }
}

write.table(cmd, sprintf("%s/rm_unfinished.sh", base2), quote = F, row.names=F, col.names = F)

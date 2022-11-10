library(ggplot2)
library(data.table)
library(RSQLite)
library(DBI)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
setDTthreads(1)
print(sprintf("Number of core(s) is(are) %d.", getDTthreads()))

base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)


simulations<-simulations[which(simulations$species_evo_level==0),]
simulations<-simulations[which(simulations$is_run==1),]
#simulations<-simulations[which(simulations$species_evo_type %in% c(4)),]

table(simulations$species_evo_type)
table(simulations$directional_speed)
table(simulations[, c("nb", "da")])

all_df<-data.table(simulations)
i=1
runtimes<-list()
for (i in c(1:nrow(all_df))){
  print(i)
  item<-all_df[i]
  log<-sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results/%s/%s.sqlite", item$label, item$label)  
  
  logdb <- dbConnect(RSQLite::SQLite(), log)
  runtime<-data.table(dbReadTable(logdb, "runtime"))
  dbDisconnect(logdb)
  runtime$global_id<-item$global_id
  runtime$label<-item$label
  runtime$da<-item$da
  runtime$nb<-item$nb
  runtime$evo_type<-item$evo_type
  runtime$species_evo_type<-item$species_evo_type
  runtime$directional_speed<-item$directional_speed
  runtime$species_evo_level<-item$species_evo_level
  runtimes[[length(runtimes)+1]]<-runtime
  
}
runtimess<-rbindlist(runtimes)
runtimess$seconds<-runtimess$end - runtimess$start
runtimess$hours<-runtimess$seconds/3600

range(runtimess$hours)
#in MB
range(runtimess$memory)
saveRDS(runtimess, "../Data/runtimes.rda")

runtimess<-readRDS("../Data/runtimes.rda")
sum(runtimess$hours)


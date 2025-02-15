library(ggplot2)
library(data.table)
library(RSQLite)
library(DBI)
library(sf)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
setDTthreads(1)
print(sprintf("Number of core(s) is(are) %d.", getDTthreads()))


base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
#mydb <- dbConnect(RSQLite::SQLite(), "/media/huijieqiao/SSD_Fast/conf.sqlite")
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)
table(simulations$is_run)
table(simulations$species_evo_type)

simulations<-simulations[which(simulations$is_run==1),]

simulations<-simulations[which(simulations$species_evo_type==1),]
seeds<-readRDS("../Data/seeds.rda")
continent<-read_sf("/media/huijieqiao/Butterfly/Niche_Conservatism/Shape/continents/continent.shp")

st_crs(continent)<-st_crs("+proj=longlat +datum=WGS84")
seeds_points <- st_as_sf(
  seeds,
  coords = c("lon", "lat"),
  crs = st_crs("+proj=longlat +datum=WGS84")
)
ggplot(continent)+geom_sf(aes(fill=factor(id)))

america<-st_contains(continent, seeds_points)[c(2, 5)]

america<-c(america[[1]], america[[2]])


american_seeds<-seeds[america,]

ggplot(continent)+geom_sf(aes(fill=factor(id)))+
  geom_point(data=american_seeds, aes(x=lon, y=lat))

simulations<-simulations[which(simulations$global_id %in% american_seeds$global_id),]

for (i in c(1:nrow(simulations))){
  print(paste(i, nrow(simulations)))
  item<-simulations[i,]
  folder<-sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results/%s", item$label)
  target<-sprintf("../Results/America/%s", item$label)
  if (dir.exists(target)){
    next()
  }
  file.copy(folder, "../Results/America", recursive = TRUE)
}

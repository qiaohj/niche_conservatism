library("dggridR")
library("dplyr")
library("DBI")
library("phytools")
library("tidyr")
library("data.table")
library("tibble")
library("raster")
library("sp")
library("ggplot2")


#Construct a global grid with cells approximately 1000 miles across
dggs          <- dgconstruct(projection = "ISEA",
                             aperture = 3,
                             topology = "HEXAGON",
                             res = 8)
base<-"/home/huijieqiao/git/ees_3d_data/niche_conservatism"
db_base<-"/home/huijieqiao/git/ees_3d_data/SMART_SPECIES"
base2<-"/media/huijieqiao/Butterfly/SMART_SPECIES"

simulations<-NULL
for (i in c(1,2,7)){
  #for (i in c(7)){
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
simulations<-simulations %>% filter(nb!="BROAD")
simulations<-simulations %>% filter(is_run==1)
seeds<-unique(simulations$global_id)
grid<- dgcellstogrid(dggs,seeds,frame=TRUE,wrapcells=TRUE)

#Get polygons for each country of the world
countries <- map_data("world")

#Plot everything on a flat map
p<- ggplot() + 
  geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="grey", alpha=0.4)   +
  geom_polygon(data=grid,      aes(x=long, y=lat, group=group), fill="red")    +
  #geom_path   (data=grid,      aes(x=long, y=lat, group=group), alpha=0.4, color="white") +
  scale_fill_gradient(low="blue", high="red")
p
ggsave(p, file=sprintf("%s/Figures/seed_flat.pdf", base))
#Replot on a spherical projection
#Beijing
p<-p+coord_map("ortho", orientation = c(39.916668, 116.383331, 0))+
  xlab('')+ylab('')+
  theme(axis.ticks.x=element_blank())+
  theme(axis.ticks.y=element_blank())+
  theme(axis.text.x=element_blank())+
  theme(axis.text.y=element_blank())+
  ggtitle('Seeds, Beijing View')
ggsave(p, file=sprintf("%s/Figures/seed_3d_Beijing.pdf", base))

#London
p<-p+coord_map("ortho", orientation = c(51.509865, -0.118092, 0))+
  xlab('')+ylab('')+
  theme(axis.ticks.x=element_blank())+
  theme(axis.ticks.y=element_blank())+
  theme(axis.text.x=element_blank())+
  theme(axis.text.y=element_blank())+
  ggtitle('Seeds, London View')
ggsave(p, file=sprintf("%s/Figures/seed_3d_London.pdf", base))

#Lawrence

p<-p+coord_map("ortho", orientation = c(38.9560, -95.2520, 0))+
  xlab('')+ylab('')+
  theme(axis.ticks.x=element_blank())+
  theme(axis.ticks.y=element_blank())+
  theme(axis.text.x=element_blank())+
  theme(axis.text.y=element_blank())+
  ggtitle('Seeds, Lawrence View')
ggsave(p, file=sprintf("%s/Figures/seed_3d_Lawrence.pdf", base))



if (F){
  
  
  #Load included test data set
  data(dgquakes)
  
  #Get the corresponding grid cells for each earthquake epicenter (lat-long pair)
  dgquakes$cell <- dgGEO_to_SEQNUM(dggs,dgquakes$lon,dgquakes$lat)$seqnum
  
  #Converting SEQNUM to GEO gives the center coordinates of the cells
  cellcenters   <- dgSEQNUM_to_GEO(dggs,dgquakes$cell)
  
  #Get the number of earthquakes in each cell
  quakecounts   <- dgquakes %>% group_by(cell) %>% summarise(count=n())
  
  #Get the grid cell boundaries for cells which had quakes
  grid          <- dgcellstogrid(dggs,quakecounts$cell,frame=TRUE,wrapcells=TRUE)
  
  #Update the grid cells' properties to include the number of earthquakes
  #in each cell
  grid          <- merge(grid,quakecounts,by.x="cell",by.y="cell")
  
  #Make adjustments so the output is more visually interesting
  grid$count    <- log(grid$count)
  cutoff        <- quantile(grid$count,0.9)
  grid          <- grid %>% mutate(count=ifelse(count>cutoff,cutoff,count))
  
}
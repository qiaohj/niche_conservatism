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
stat<-readRDS(sprintf("%s/Data/stat.rda", base))
stat%>%dplyr::filter(between(N_SPECIATION, 100, 200)&between(N_EXTINCTION, 10, 20) )
example<-stat%>%dplyr::filter((SEED_ID==6225)&(DA=="GOOD")&(NB=="MODERATE"))

simulation<-simulations%>%dplyr::filter((global_id==6225)&(da=="GOOD")&(nb=="MODERATE")&(evo_type==1))
nb<-strsplit(simulation[1, "nb_v"], "\\|")[[1]]
temp_range<-as.numeric(strsplit(nb[1], "\\,")[[1]])
prec_range<-as.numeric(strsplit(nb[3], "\\,")[[1]])

item<-example[3,]

if (item$EVO_RATIO==0.05){
  item$EVO_RATIO_LABEL<-"0.1"
}
if (item$EVO_RATIO==0.005){
  item$EVO_RATIO_LABEL<-"0.01"
}
if (item$EVO_RATIO==1){
  item$EVO_RATIO_LABEL<-"1"
}
label<-sprintf("%d_%s_%s_%d_%s", item$SEED_ID, item$DA, item$NB, 
               item$EVO_TYPE, item$EVO_RATIO_LABEL)
#df<-readRDS(sprintf("%s/RESULTS/%s/%s_log_nb.rda", base2, label, label))
#df2<-readRDS(sprintf("%s/RESULTS/%s/%s.log.env.rda", base2, label, label))
#Format for 2 and 7: 
#year_i, id, uid, parent_uid, evoType, species->getIDWithParentID().c_str(), nb_str.c_str(), memo.c_str()
#r, env_range, env_list
df<-read.csv(sprintf("%s/RESULTS/%s/%s.log", base2, label, label), head=F, stringsAsFactors = F)

colnames(df)<-c("Y", "GLOBAL_ID", "GROUP", "N", "SP_ID", "SUITABLE")
df$Y<-df$Y*-1
y=-1
countries <- map_data("world")

#for
seed<-c(10.4683841,-66.9605775)
folder<-sprintf("%s/Figures/Dispersal_Example/%s", base, label)
dir.create(folder, showWarnings = F)
dir.create(sprintf("%s/FLAT", folder), showWarnings = F)
dir.create(sprintf("%s/3D", folder), showWarnings = F)

for (y in rev(unique(df$Y))){
  print(y)
  sub_df<-df%>%dplyr::filter(Y==y)
  sub_df<-sub_df%>%dplyr::distinct(Y, GLOBAL_ID, N, SUITABLE)
  sub_df<-sub_df%>%dplyr::group_by(Y, GLOBAL_ID, SUITABLE)%>%dplyr::summarise(N=sum(N))
  grid<-dgcellstogrid(dggs,sub_df$GLOBAL_ID,frame=TRUE,wrapcells=TRUE)
  grid$cell<-as.numeric(grid$cell)
  grid<-inner_join(grid, sub_df,by=c("cell"="GLOBAL_ID"))
  p<-ggplot() + 
    geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="grey", alpha=0.4)
  unsuitable<-grid%>%dplyr::filter(SUITABLE==0)
  if (nrow(unsuitable)>0){
    #p<-p+geom_polygon(data=unsuitable, aes(x=long, y=lat, group=group), fill="grey")+
    #  geom_path(data=unsuitable, aes(x=long, y=lat, group=group), alpha=0.4, color="grey")
  }
  suitable<-grid%>%dplyr::filter(SUITABLE==1)
  if (nrow(suitable)>0){
    p<-p+geom_polygon(data=suitable,aes(x=long, y=lat, group=group, fill=N))+
      geom_path(data=suitable, aes(x=long, y=lat, group=group), alpha=0.4, color="white")
  }
  p<-p+
    scale_fill_gradient(low="blue", high="red")+
    theme_bw()+
    xlab('')+ylab('')+
    theme(legend.position = "none")+
    theme(axis.ticks.x=element_blank())+
    theme(axis.ticks.y=element_blank())+
    theme(axis.text.x=element_blank())+
    theme(axis.text.y=element_blank())+
    ggtitle(sprintf("%s @ %d", label, y))
  ggsave(p, file=sprintf("%s/FLAT/%s.png", folder, gsub("-", "B", y)), width=6.61, height=5.83)
  if (is.null(seed)){
    seed<-c(grid[1, "lat"], grid[1, "long"])
  }
  p<-p+coord_map("ortho", orientation = c(seed, 0))
  ggsave(p, file=sprintf("%s/3D/%s.png", folder, gsub("-", "B", y)), width=6.61, height=5.83)
}

library(animation)
imgs <- paste(folder, "/3D/", "B", c(1199:0), ".png", sep="")
ani.options()
ani.options("interval" = 0.05)
ani.options("ani.res" = 150)
ani.options("ani.width" = 1280)
ani.options("ani.height" = 1280)

saveVideo({
  for(img in imgs){
    print(img)
    im <- magick::image_read(img)
    plot(as.raster(im))
  }  
}, video.name=sprintf("%s/dispersal.mp4", folder))

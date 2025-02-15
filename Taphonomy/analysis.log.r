library(data.table)
library(RSQLite)
library(DBI)
library(ape)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(heatmaply)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")

base<-"../Results/America"

#folder's name
folder<-"11454_GOOD_BROAD_1_0_0"
log<-sprintf("%s/%s/%s.log", base, folder, folder)

#load the log info
log_df<-fread(log)

#setup the column names to make the data.table more understandable
#year= X*100 years before present, 
#global_id = ID of the hexagon, can be used to connect to the hexagon polygon, see example below
#group_id id of the population
#n: number of the individuals
#sp_id: species id
#suitable: is a suitable area, unsuitable area is the species dispersals but is unsuitable hexagon which can be removed.

colnames(log_df)<-c("year", "global_id", "group_id", "n", "sp_id", "suitable")

#remove the unsuitable area
log_df<-log_df[suitable==1]

#Visualization
wc_col<-c("#4477AA", "#EE6677")
crs_america<-"+proj=laea +lat_0=30 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

polygon<-readRDS("../Results/polygon.rda")
world <- ne_countries(scale = "small", returnclass = "sf")
item<-log_df[year==0]
item<-item[, .(N_SP=length(unique(sp_id))), by=list(year, global_id)]
item$global_id<-as.character(item$global_id)
item<-merge(polygon, item, by.y="global_id", by.x="Name")
mycol <- cool_warm(max(item$N_SP) - min(item$N_SP) + 1)

p<-ggplot(item, aes(fill=N_SP, color=N_SP)) +
  geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
  geom_sf()+
  scale_fill_gradientn(colours  = mycol)+
  scale_colour_gradientn(colours  = mycol)+
  #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
  coord_sf(crs = st_crs(crs_america))+
  labs(colour="Number of species", fill="Number of species")+
  xlim(-12e6, 12e6)+
  ylim(-12e6, 12e6)+
  theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.title = element_blank())
p

#phylo tree
mydb <- dbConnect(RSQLite::SQLite(), 
                  sprintf("%s/%s/%s.sqlite", base,
                          folder, folder))
trees<-dbReadTable(mydb, "trees")
dbDisconnect(mydb)

text.string<-trees[1,2]
text.string<-gsub("\\]", "#", gsub("\\[", "#", text.string))
if (!grepl("\\(", text.string)){
  text.string<-sprintf("(a:0)%s", text.string)
}
vert.tree<-read.tree(text=text.string)
#vert.tree$root.edge


plot(vert.tree)
nodelabels()
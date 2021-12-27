library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(ggspatial)
library(data.table)
library(RSQLite)
library(DBI)
library(ape)
library(phangorn)
library(phytools)
library(geiger)
library(stringr)
library(plotKML)
library(ggtree)
require(phylobase)
library(ggpubr)
if (F){
  base_db<-sprintf("%s/Configuration/env_Hadley3D.sqlite", "../")
  envdb <- dbConnect(RSQLite::SQLite(), base_db)
  
  v_max_prec<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Precipitation")
  v_max_temp<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Temperature")
  v_min_temp<-dbReadTable(envdb, "Debiased_Minimum_Monthly_Temperature")
  
  dbDisconnect(envdb)
  
  df<-data.table(read.table("../Data/Example/27464_GOOD_NARROW_2_0.1_0/27464_GOOD_NARROW_2_0.1_0.log",
                            head=F, sep=",", stringsAsFactors = F))
  #df<-data.table(read.table(sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results/%s/%s.log", sp, sp), 
  #                          head=F, sep=",", stringsAsFactors = F))
  colnames(df)<-c("year", "global_id", "v3", "v4", "sp_id", "suitable")
  v_max_prec<-data.table(v_max_prec)
  v_max_temp<-data.table(v_max_temp)
  v_min_temp<-data.table(v_min_temp)
  
  v_max_prec_sub<-v_max_prec[global_id %in% df$global_id]
  v_max_temp_sub<-v_max_temp[global_id %in% df$global_id]
  v_min_temp_sub<-v_min_temp[global_id %in% df$global_id]
  
  v_max_prec_sub_se<-v_max_prec_sub[, .(mean=mean(v), sd=sd(v)),
                                    by=list(year)]
  v_max_temp_sub_se<-v_max_temp_sub[, .(mean=mean(v), sd=sd(v)),
                                    by=list(year)]
  v_min_temp_sub_se<-v_min_temp_sub[, .(mean=mean(v), sd=sd(v)),
                                    by=list(year)]
  p_temp<-ggplot()+
    geom_ribbon(data=v_max_temp_sub_se, aes(x=year * -0.1, ymin=mean-sd, ymax=mean+sd), 
                fill="red", alpha=0.2)+
    geom_ribbon(data=v_max_prec_sub_se, aes(x=year * -0.1, ymin=mean-sd, ymax=mean+sd), 
                fill="black", alpha=0.2)+
    geom_ribbon(data=v_min_temp_sub_se, aes(x=year * -0.1, ymin=mean-sd, ymax=mean+sd), 
                fill="blue", alpha=0.2)+
    geom_line(data=v_max_temp_sub_se, aes(x=year * -0.1, y=mean), color="red")+
    geom_line(data=v_min_temp_sub_se, aes(x=year * -0.1, y=mean), color="blue")+
    geom_line(data=v_max_prec_sub_se, aes(x=year * -0.1, y=mean), color="black")+
    xlab("K years before present")+
    ylab("Temperature (degree) / Precipitation (mm/day)")+
    xlim(-120, 0)+
    theme_bw()
  saveRDS(p_temp, "../Figures/Example/27464_GOOD_NARROW_2_0.1_0/env_curve.rda")
  #df<-df[suitable==1]
  
  
  all<-readRDS("../Data/N_speciation_extinction_NARROW_GOOD_0.rda")
  id<-all[which((year==0)&between(N_SPECIES, 10, 20))]
  id[global_id==27464]
  background<-readRDS("../Data/mask_lonlat.rda")
  background_p<-st_sfc(st_multipoint(as.matrix(target[, ..cols])), 
                       crs=st_crs(ll_str))
  
  target<-background[global_id %in% id]
  target<-target[between(lon, 100, 130)&between(lat, 0, 90)]
  
  lines<-st_read("../Shape/isea3h8/isea3h8.kml")
  polygon<-st_cast(lines, "POLYGON")
  saveRDS(polygon, "../Figures/Example/polygon.rda")
}

p_temp<-readRDS("../Figures/Example/27464_GOOD_NARROW_2_0.1_0/env_curve.rda")
target<-c(27464)
polygon_sub<-polygon[which(polygon$Name %in% target),]
ll_str<-"+proj=longlat +datum=WGS84 +no_defs"
cols<-c("lon", "lat")

world <- ne_countries(scale = "small", returnclass = "sf")
crs_str<-"+proj=laea +lat_0=30 +lon_0=90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

mydb <- dbConnect(RSQLite::SQLite(), "../Data/Example/27464_GOOD_NARROW_2_0.1_0/27464_GOOD_NARROW_2_0.1_0.sqlite")
trees<-dbReadTable(mydb, "trees")
dbDisconnect(mydb)

text.string<-trees[1,2]
text.string<-gsub("\\]", "#", gsub("\\[", "#", text.string))
if (!grepl("\\(", text.string)){
  text.string<-sprintf("(a:0)%s", text.string)
}
vert.tree<-read.tree(text=text.string)
vert.tree$root.edge


#plot(vert.tree)
#nodelabels()
nodes<-data.table(label=c(vert.tree$node.label, vert.tree$tip.label),
                  type=c(rep("node", length(vert.tree$node.label)),
                         rep("leaf", length(vert.tree$tip.label)))
)
nodes<-nodes[label!="a"]
if (nrow(nodes)==1){
  nodes$type<-"leaf"
}
nodes[, c("PX","PY") := data.table(str_split_fixed(label,"@",2))]
nodes[, c("from","to") := data.table(str_split_fixed(PY,"-",2))]
nodes$from<-as.numeric(nodes$from)
nodes$to<-as.numeric(nodes$to)
nodes$event<-"SPECIATION"
nodes[(type=="leaf")&(to==0), "event"]<-"NONE"
nodes[(type=="leaf")&(to!=0), "event"]<-"EXTINCTION"

phylo_tree<-as(vert.tree, "phylo4d")

data_df<-data.frame(ori_name=phylo_tree@label, new_label="", 
                            ancestor=as.numeric(ancestor(phylo_tree)),
                            id=c(1:length(phylo_tree@label)),
                    nodetype=nodeType(phylo_tree),
                    colorIndex=0)
id=1

setColor<-function(id, color_i){
  if (data_df[which(data_df$id==id), "colorIndex"]!=0){
    return()
  }
  data_df[which(data_df$id==id), "colorIndex"]<<-color_i
  if (!is.na(data_df[which(data_df$id==id), "ancestor"])){
    setColor(data_df[which(data_df$id==id), "ancestor"], color_i)
  }
}
color_i<-1
for (id in data_df[which(data_df$nodetype=="tip"), "id"]){
  setColor(id, color_i)
  color_i<-color_i+1
}
colors<-rainbow(max(data_df$colorIndex))
colors<-colors[sample(length(colors), length(colors))]
data_df$color<-colors[data_df$colorIndex]
data_df$simp_label<-gsub("SP", "", str_split_fixed(data_df$ori_name,"@",2)[,1])
data_df[which(data_df$simp_label==target),]$new_label<-as.character(target)
root<-rootNode(phylo_tree)
set_label<-function(node){
  item<-data_df[node, ]
  if (!is.na(item$ancestor)){
    ancestor<-data_df[item$ancestor, ]
    data_df[node, "new_label"]<<-sprintf("%s-%s", 
                                                 ancestor$new_label, item$simp_label)
  }
  children<-children(phylo_tree, node)
  if (length(children)==0){
    return()
  }
  for (i in c(1:length(children))){
    set_label(children[i])
  }
  
}
set_label(root)

data_df<-data_df[order(data_df$id),]
nodeId(phylo_tree, "internal")

rownames(data_df)<-data_df$id
phylo_tree@data<-data_df

labels<-phylo_tree@data$new_label
names(labels)<-names(phylo_tree@label)
phylo_tree@label<-labels

checkPhylo4(phylo_tree) 
ggtree(phylo_tree, aes(color=I(color)))+
  geom_text(aes(label=label), hjust=-.3)
df<-data.table(read.table("../Data/Example/27464_GOOD_NARROW_2_0.1_0/27464_GOOD_NARROW_2_0.1_0.log",
                          head=F, sep=",", stringsAsFactors = F))
colnames(df)<-c("year", "global_id", "v3", "v4", "sp_id", "suitable")

df<-df[suitable==1]

df<-merge(df, data_df[, c("color", "new_label")], by.x="sp_id", by.y="new_label", all=T)
i=900
sp<-"27464_GOOD_NARROW_2_0.1_0"
#niche shift
df_nb<-read.table(sprintf("../Data/Example/%s/%s.sp.log", sp, sp), head=F, sep=",", stringsAsFactors = F)
colnames(df_nb)<-c("year", "sp_id",
                "V1_L", "V1_min", "V1_max",
                "V2_L", "V2_min", "V2_max",
                "V3_L", "V3_min", "V3_max", 
                "TT")
if (ncol(df_nb)==13){
  colnames(df_nb)[13]<-"group_id"
}else{
  df_nb$group_id<-1
}

unique(df$V2_L)

df_all<-list()
for (i in c(1:3)){
  item<-df_nb[,c("year", "sp_id", "group_id",
                 sprintf("V%d_L", i), sprintf("V%d_min", i), 
                 sprintf("V%d_max", i))]
  colnames(item)<-c("year", "sp_id", "group_id",
                    "V_L", "V_min", "V_max")
  df_all[[i]]<-item
}
df_all<-rbindlist(df_all)
df_all$V_mean<-(df_all$V_max+df_all$V_min)/2
df_all$V_range<-df_all$V_max-df_all$V_min
df_all<-merge(df_all, data_df[, c("color", "new_label")], 
              by.x="sp_id", by.y="new_label", all=T)
df_all$Label<-ifelse(df_all$V_L=="Debiased_Maximum_Monthly_Precipitation",
                     "Precipitation", 
                     "Max temperature")
df_all[V_L=="Debiased_Minimum_Monthly_Temperature"]$Label<-
  "Min temperature"
g1<-ggplot(df_all)+
  #geom_line(aes(x=year/10-120, y=V_min, color=I(color)))+
  geom_line(aes(x=year/10-120, y=V_min, color=I(color)))+
  theme(legend.position = "none")+xlim(-120, 0)+
  facet_wrap(~Label, scale="free", nrow=3)+
  theme_bw()+ylab("lower limit")+xlab("K years before present")
g1

for (i in c(max(df$year):min(df$year))){
  print(i)
  item<-df[year==i]
  polygon_sub<-polygon[which(polygon$Name %in% item$global_id),]
  polygon_sub<-merge(polygon_sub, item, by.x = "Name", by.y="global_id")
  p<-ggplot() +
    geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
    geom_sf(data=polygon_sub, aes(color=I(color), fill=I(color))) + 
    coord_sf(crs = st_crs(crs_str))+
    theme(panel.grid.major = element_line(color = "#d4d4d4", 
                                          linetype = "dashed", size = 0.5), 
          panel.background = element_rect(fill = "#FFFFFF"))+
    xlim(-12000000, 12000000)+
    ylim(-12000000, 12000000)+
    ggtitle(sprintf("%.1fk YBP", i/10))+
    theme(legend.position = "none")
  p
  p2<-ggtree(phylo_tree, aes(color=I(color)))+geom_rootedge()+
    geom_vline(xintercept = vert.tree$root.edge*-1 + (1200-i), linetype=2)+
    xlim(-170, 1200)
  p2
  p3<-p_temp+geom_vline(xintercept = i*-0.1, linetype=2)
  p3
  
  g1<-ggplot(df_all)+
    #geom_line(aes(x=year/10-120, y=V_min, color=I(color)))+
    geom_line(aes(x=year/10-120, y=V_min, color=I(color)))+
    theme(legend.position = "none")+xlim(-120, 0)+
    facet_wrap(~Label, scale="free", nrow=3, strip.position="right")+
    theme_bw()+ylab("lower limit")+xlab("K years before present")+
    geom_vline(xintercept = i*-0.1, linetype=2)
  
  pp<-ggarrange(p, ggarrange(p2,p3,g1, nrow=3, heights=c(1,1,2)), 
                nrow=1, widths=c(1, 1))
  pp
  ggsave(pp, filename=sprintf("../Figures/Example/27464_GOOD_NARROW_2_0.1_0/by_year/%d.png", i), width=15, height=8)
}

cd /media/huijieqiao/Butterfly/Niche_Conservatism/Figures/Example/27464_GOOD_NARROW_2_0.1_0/by_year
ffmpeg -r 5 -start_number -1198 -i y%d.png -y ../27464_GOOD_NARROW_2_0.1_0.mp4



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
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/tree_fun.R")
source("commons/functions.r")
polygon<-readRDS("../Figures/Movie2.Example/polygon.rda")
target<-27464
ll_str<-"+proj=longlat +datum=WGS84 +no_defs"
cols<-c("lon", "lat")

world <- ne_countries(scale = "small", returnclass = "sf")
crs_str<-"+proj=laea +lat_0=30 +lon_0=90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

species_evo_types<-data.table(species_evo_type=c(1,2,2,3,3,4,4,5,6,7),
                              directional_speed=c(0,0.5,0.1,0.5,0.1,
                                                  0.5,0.1,0.01,0.01,0.01))
species_evo_types<-formatLabels(species_evo_types)
if (F){
  df_list<-list()
  tree_list<-list()
  iii=9
  for (iii in c(1:nrow(species_evo_types))){
    print(paste(iii))
    sp_item<-species_evo_types[iii]
    sp_file<-sprintf("%d_GOOD_NARROW_%d_%s_0", target,
                     sp_item$species_evo_type, 
                     as.character(sp_item$directional_speed))
    mydb <- dbConnect(RSQLite::SQLite(), 
                      sprintf("../Data/Example/%s/%s.sqlite",
                              sp_file, sp_file))
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
    tree_list[[paste(sp_item$species_evo_type, sp_item$directional_speed, target)]]<-phylo_tree
    df<-data.table(read.table(sprintf("../Data/Example/%s/%s.log", sp_file, sp_file),
                              head=F, sep=",", stringsAsFactors = F))
    colnames(df)<-c("year", "global_id", "v3", "v4", "sp_id", "suitable")
    
    df<-df[suitable==1]
    
    df<-merge(df, data_df[, c("color", "new_label")], by.x="sp_id", by.y="new_label", all=T)
    df_list[[paste(sp_item$label)]]<-df
  }
  saveRDS(df_list, "../Data/Example/dis.rda")
  saveRDS(tree_list, "../Data/Example/tree.rda")
}
df_list<-readRDS("../Data/Example/dis.rda")
tree_list<-readRDS("../Data/Example/tree.rda")
n<-names(df_list)[1]
i=349
types<-c("conservatism", "shift-directional (0.1)", "expansion-directional (0.1)","expansion-omnidirectional (0.1)",
         "shift-directional (0.5)", "expansion-directional (0.5)",  "expansion-omnidirectional (0.5)",
         "random-central", "random-symmetrical", "random-asymmetrical")
for (i in c(max(df$year):min(df$year))){
  plist<-list()
  print(i)
  for (n in types){
    print(n)
    df_item<-df_list[[n]]
    title<-sprintf("%s", trimws(gsub(target, "", n)))
    if (n==types[1]){
      titletheme<-theme(legend.position = "none")
      title<-sprintf("%.1fk YBP %s", i/10, n)
    }else{
      titletheme<-theme(legend.position = "none", plot.title = element_text(size = 8))
    }
    
    
    item<-df_item[year==i]
    polygon_sub<-polygon[which(polygon$Name %in% item$global_id),]
    polygon_sub<-merge(polygon_sub, item, by.x = "Name", by.y="global_id")
    p<-ggplot() +
      geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
      geom_sf(data=polygon_sub, aes(color=I(color), fill=I(color))) + 
      coord_sf(crs = st_crs(crs_str))+
      theme(panel.grid.major = element_line(color = "#d4d4d4", 
                                            linetype = "dashed", linewidth = 0.5), 
            panel.background = element_rect(fill = "#FFFFFF"))+
      xlim(-12000000, 12000000)+
      ylim(-12000000, 12000000)+
      ggtitle(title)+titletheme
    
    p
    plist[[n]]<-p
  }
  
  pp1<-ggarrange(plist[[1]], ggarrange(plotlist=plist[c(2:10)], nrow=3, ncol=3),
                 widths = c(1,1), align = "v")
  ggsave(pp1, filename=sprintf("../Figures/Movie2.Example/all_scenarios/%d.png", i),
         width=10.5, height=6, bg="white")
}

for file in * ; do
mv ./"$file" "y-$file"
done

cd /media/huijieqiao/Butterfly/Niche_Conservatism/Figures/Example/all_scenarios
ffmpeg -r 5 -start_number -1198 -i y%d.png -y ../27464_all_scenarios.mp4

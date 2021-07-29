library(dplyr)
library(ggplot2)
library(RSQLite)
library(DBI)
library(ape)
library(phytools)
library(ggtree)
library(ggpubr)


setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
base_db<-"../Configuration/env_Hadley3D.sqlite"
envdb <- dbConnect(RSQLite::SQLite(), base_db)
v_min_temp<-dbReadTable(envdb, "Debiased_Minimum_Monthly_Temperature")
v_max_temp<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Temperature")
v_max_prec<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Precipitation")
dbDisconnect(envdb)
v_max_temp_se<-v_max_temp%>%dplyr::group_by(year)%>%
  dplyr::summarise(mean_v=mean(v),
                   sd_v=sd(v))

#v_max_temp_se<-v_max_prec%>%dplyr::group_by(year)%>%
#  dplyr::summarise(mean_v=mean(v),
#                   sd_v=sd(v))

#species_evo_type
#1: niche conservatism
#2: niche shift (directional)
#3: niche expansion (directional)
#4: niche expansion (omnidirectional)
#5: niche shift (random in box center)
#6: niche shift (random symmetrical change in box limit)
#7: niche shift (random asymmetrical change in box limit)

sp="19759_GOOD_BROAD_6_0.01_0"
for (sp in c("19759_GOOD_BROAD_2_0.01_0", "19759_GOOD_BROAD_2_0.5_0", "19759_GOOD_BROAD_2_0.1_0",
             "19759_GOOD_BROAD_3_0.01_0", "19759_GOOD_BROAD_3_0.5_0", "19759_GOOD_BROAD_3_0.1_0",
             "19759_GOOD_BROAD_4_0.01_0", "19759_GOOD_BROAD_4_0.5_0", "19759_GOOD_BROAD_4_0.1_0",
             "19759_GOOD_BROAD_5_0.01_0", "19759_GOOD_BROAD_6_0.01_0", "19759_GOOD_BROAD_7_0.01_0",
             "19759_GOOD_BROAD_2_0.01_1", "19759_GOOD_BROAD_2_0.5_1", "19759_GOOD_BROAD_2_0.1_1",
             "19759_GOOD_BROAD_3_0.01_1", "19759_GOOD_BROAD_3_0.5_1", "19759_GOOD_BROAD_3_0.1_1",
             "19759_GOOD_BROAD_4_0.01_1", "19759_GOOD_BROAD_4_0.5_1", "19759_GOOD_BROAD_4_0.1_1",
             "19759_GOOD_BROAD_5_0.01_1", "19759_GOOD_BROAD_6_0.01_1", "19759_GOOD_BROAD_7_0.01_1")){
  print(sp)
  #sp<-"19759_GOOD_BROAD_2_1_0"
  df<-read.table(sprintf("../Results/%s/%s.sp.log", sp, sp), head=F, sep=",", stringsAsFactors = F)
  colnames(df)<-c("year", "SP_ID",
                  "V1_L", "V1_min", "V1_max",
                  "V2_L", "V2_min", "V2_max",
                  "V3_L", "V3_min", "V3_max", 
                  "TT")
  if (ncol(df)==13){
    colnames(df)[13]<-"group_id"
  }else{
    df$group_id<-1
  }
  
  unique(df$V2_L)
  
  v_label<-"Debiased_Maximum_Monthly_Temperature"
  v_label_f<-"max_t"
  df_all<-NULL
  for (i in c(1:3)){
    item<-df[which(df[,sprintf("V%d_L", i)]==v_label),]
    item<-item[, c("year", "SP_ID", "group_id",
                   sprintf("V%d_L", i), sprintf("V%d_min", i), sprintf("V%d_max", i))]
    if (nrow(item)==0){
      next()
    }
    colnames(item)<-c("year", "SP_ID", "group_id",
                      "V_L", "V_min", "V_max")
    if (is.null(df_all)){
      df_all<-item
    }else{
      df_all<-bind_rows(df_all, item)
    }
  }
  df_all$V_mean<-(df_all$V_max+df_all$V_min)/2
  df_all$V_range<-df_all$V_max-df_all$V_min
  df_all_se<-df_all%>%dplyr::group_by(SP_ID, year, group_id)%>%
    dplyr::summarise(mean_V_mean=mean(V_mean),
                     sd_V_mean=sd(V_mean),
                     mean_V_range=mean(V_range),
                     sd_V_range=sd(V_range))
  df_all_se$label<-paste(df_all_se$SP_ID, df_all_se$group_id)
  
  df_all_se<-df_all%>%dplyr::group_by(SP_ID, year)%>%
    dplyr::summarise(mean_V_mean=mean(V_mean),
                     sd_V_mean=sd(V_mean),
                     mean_V_range=mean(V_range),
                     sd_V_range=sd(V_range),
                     max_V_mean=max(V_mean),
                     min_V_mean=min(V_mean))
  df_all_se[is.na(df_all_se)]<-0
  g1<-ggplot(df_all_se)+
    geom_ribbon(aes(x=year, ymin=mean_V_mean-mean_V_range/2,
                    ymax=mean_V_mean+mean_V_range/2, fill=factor(SP_ID)),
                alpha=0.2)+
    geom_line(aes(x=year, y=mean_V_mean, color=factor(SP_ID)))+
    theme(legend.position = "none")+xlim(0, 1200)
  #g1
  df_all_se$SP_ID_f<-factor(df_all_se$SP_ID)
  g2<-ggplot(df_all_se)+
    geom_ribbon(aes(x=year, ymin=min_V_mean,
                    ymax=max_V_mean, fill=SP_ID_f),
                alpha=0.2)+
    geom_line(aes(x=year, y=mean_V_mean, color=SP_ID_f))+
    theme(legend.position = "none")+xlim(0, 1200)
  
  #g2
  v_max_temp_se$year_i<-1200-v_max_temp_se$year
  g3<-ggplot(v_max_temp_se)+
    geom_ribbon(data=v_max_temp_se, aes(x=year_i, ymin=mean_v-sd_v, ymax=mean_v+sd_v), alpha=0.2)+
    geom_line(data=v_max_temp_se, aes(x=year_i, y=mean_v))+
    theme(legend.position = "none")+xlim(0, 1200)
  #g3
  
  logdb<-sprintf("../Results/%s/%s.sqlite", sp, sp)
  mydb <- dbConnect(RSQLite::SQLite(), logdb)
  trees<-dbReadTable(mydb, "trees")
  dbDisconnect(mydb)
  text.string<-trees[1,2]
  text.string<-gsub("\\]", "#", gsub("\\[", "#", text.string))
  vert.tree<-read.tree(text=text.string)
  #plot(vert.tree)
  #nodelabels()
  
  g4<-ggtree(vert.tree, root.position = vert.tree$root.edge)+theme_tree2()+xlim(0, 1200)
  #edge=data.frame(vert.tree$edge, edge_num=1:nrow(vert.tree$edge))
  #colnames(edge)=c("parent", "node", "edge_num")
  #g4 %<+% edge + geom_label(aes(x=branch, label=edge_num))
  
  #g4
  #plotTree(vert.tree, ftype="i")
  #tiplabels(vert.tree$tip.label)
  #nodelabels(vert.tree$node.label)
  
  
  g<-egg::ggarrange(g4, g1, g2, g3, nrow=4, ncol=1)
  #g
  ggsave(g, filename=sprintf("../Figures/niche_shift/%s_%s.png", v_label_f, sp), width=6, height=9)
}

library(ggplot2)
library(dplyr)
library(RSQLite)
library(DBI)
library(ape)
library(phangorn)
library(phytools)
library(geiger)
library(stringr)
library(tidyverse)
library(plotKML)

df<-read.csv("/media/huijieqiao/Butterfly/Niche_Center_E_G/Results/4365_POOR_BROAD_7_0.05_935/4365_POOR_BROAD_7_0.05_935.log", head=F)
colnames(df)<-c("Y", "GLOBAL_ID", "GROUP_ID", "N", "SP_ID", "IS_SUITABLE")
ggplot(df)+geom_boxplot(aes(x=factor(IS_SUITABLE), y=N))

ll<-readRDS("/home/huijieqiao/git/ees_3d_data/niche_conservatism/Data/ENV/mask_df.rda")

df_with_env<-inner_join(df, ll, by=c("GLOBAL_ID"="global_id", "Y"="Y"))

df_with_env_last<-df_with_env%>%dplyr::filter(Y==0)
table(df_with_env$Y)
xlim<-range(df_with_env$lon)
ylim<-range(df_with_env$lat)
xlim_env<-range(df_with_env$MIN_TEMP, df_with_env$MAX_TEMP)
ylim_env<-range(df_with_env$MAX_PREC)

for (year in unique(df_with_env$Y)){
  if (year>=341){
    next()
  }
  print(year)
  df_with_env_last<-df_with_env%>%dplyr::filter(Y==year)
  p<-ggplot(df_with_env_last)+geom_point(aes(x=lon, y=lat, color=N), size=0.2)+
    scale_color_gradient(low = "blue", high = "red")+
    xlim(xlim)+
    ylim(ylim)+
    theme_bw()+ggtitle(year)+
    facet_wrap(~SP_ID, ncol=1)
  #print(p)
  ggsave(p, file=sprintf("/home/huijieqiao/git/ees_3d_data/Niche_Center_G_E/Figures/Examples/G/%d_G.png", year),
         width=5, height=3*length(unique(df_with_env_last$SP_ID)), limitsize = FALSE)
  p<-ggplot(df_with_env_last)+
    geom_point(aes(x=MIN_TEMP, y=MAX_PREC, color=N), size=0.2)+
    geom_point(aes(x=MAX_TEMP, y=MAX_PREC, color=N), size=0.2)+
    scale_color_gradient(low = "blue", high = "red")+
    xlim(xlim_env)+
    ylim(ylim_env)+
    theme_bw()+ggtitle(year)+
    facet_wrap(~SP_ID, ncol=1)
  #print(p)
  ggsave(p, file=sprintf("/home/huijieqiao/git/ees_3d_data/Niche_Center_G_E/Figures/Examples/E/%d_E.png", year),
         width=5, height=3*length(unique(df_with_env_last$SP_ID)), limitsize = FALSE)
  
  #cmd <- readline(prompt="Enter (X=EXIT): ")
  #if (toupper(cmd)=="X"){
  #  break()
  #}
}

logdb<-"/media/huijieqiao/Butterfly/Niche_Center_E_G/Results/4365_POOR_BROAD_7_0.05_935/4365_POOR_BROAD_7_0.05_935.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), logdb)
trees<-dbReadTable(mydb, "trees")
suitable<-dbReadTable(mydb, "suitable")
dbDisconnect(mydb)
text.string<-trees[1,2]
text.string<-gsub("\\]", "#", gsub("\\[", "#", text.string))
vert.tree<-read.tree(text=text.string)

plotTree(vert.tree, ftype="i")
#tiplabels(vert.tree$tip.label)
nodelabels(vert.tree$node.label)


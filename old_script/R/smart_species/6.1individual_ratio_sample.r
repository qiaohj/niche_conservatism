
library(ggplot2)
library(Rmisc)

library(rgdal)
library(raster)
library(stringr)
library(dplyr)
library(RSQLite)
library(DBI)
library("ape")
library("phangorn")
library("phytools")
library("geiger")
library("stringr")
library(tidyverse)
library(plotKML)

base<-"/media/huijieqiao/Butterfly/SMART_SPECIES"

simulations<-data.frame(label=c("12337_POOR_NARROW_1_1", 
                                "12337_POOR_NARROW_2_0.01", "12337_POOR_NARROW_2_0.1", 
                                "12337_POOR_NARROW_3_0.01", "12337_POOR_NARROW_3_0.1", 
                                "12337_POOR_NARROW_4_0.01", "12337_POOR_NARROW_4_0.1",
                                "12337_POOR_NARROW_5_0.01", "12337_POOR_NARROW_5_0.1",
                                "12337_POOR_NARROW_6_0.01", "12337_POOR_NARROW_6_0.1"),
                        nb="NARROW",
                        da="POOR",
                        global_id=12337,
                        niche_envolution_individual_ratio=c(1, rep(c(0.01, 0.1), 5)),
                        stringsAsFactors = F)
result<-data.frame()
for (i in c(1:nrow(simulations))){
  s<-simulations[i,]


  log<-sprintf("%s/RESULTS/%s/%s.log", base, s$label, s$label)
  if (!file.exists(log)){
    log<-sprintf("%s/RESULTS_TEST/%s/%s.log", base, s$label, s$label)
  }
  
  print(paste(i, nrow(simulations), s$label))
  log_db<-read.table(log, head=F, sep=",", stringsAsFactors = F, quote="'")
  colnames(log_db)<-c("Y", "ID", "GROUP", "N", "SP_ID", "SUITABLE")
  if ((nrow(log_db)>10000)&(length(unique(log_db$SP_ID))>10)){
    #asdf
  }
  log_db$SP_ID<-as.character(log_db$SP_ID)
  log_db<-as_tibble(log_db)
  
  individual_ratio<-log_db %>% dplyr::group_by(Y, SUITABLE) %>% dplyr::summarise(N_IND=sum(N))
  sp_N<-log_db %>% dplyr::group_by(Y) %>% dplyr::summarise(N_SP=length(unique(SP_ID)))
  cell_N<-log_db %>% dplyr::group_by(Y, SUITABLE) %>% dplyr::count()
  colnames(cell_N)[3]<-"N_CELL"
  item<-inner_join(individual_ratio, sp_N, by="Y")
  item<-inner_join(item, cell_N, by=c("Y", "SUITABLE"))
  item$LABLE<-s$label
  item$NB<-s$nb
  item$DA<-s$da
  ff<-strsplit(s$label, "_")[[1]]
  item$EVO_TYPE<-as.numeric(ff[4])
  item$GLOBAL_ID<-s$global_id
  item$EVO_RATIO<-s$niche_envolution_individual_ratio
  if (nrow(result)==0){
    result<-item
  }else{
    result<-bind_rows(result, item)
  }
}

nb_result<-data.frame()
for (i in c(2:nrow(simulations))){
  s<-simulations[i,]
 
  ff<-strsplit(s$label, "_")[[1]]
  EVO_TYPE<-as.numeric(ff[4])
  log<-sprintf("%s/RESULTS/%s/%s.nb.log", base, s$label, s$label)
  if (!file.exists(log)){
    log<-sprintf("%s/RESULTS_TEST/%s/%s.log", base, s$label, s$label)
  }
  
  print(paste(i, nrow(simulations), s$label))
  log_db<-read.csv(log, head=F, sep=",", stringsAsFactors = F)
  if (EVO_TYPE==2){
    colnames(log_db)<-c("Y", "ID", "UID", "PARENT_UID", "evoType", "SP_ID",
                      "ENV_1_LABEL", "ENV_1_MIN", "ENV_1_MAX",
                      "ENV_2_LABEL", "ENV_2_MIN", "ENV_2_MAX",
                      "ENV_3_LABEL", "ENV_3_MIN", "ENV_3_MAX",
                      "NONAME1", 
                      "ENV_1_V_LABEL", "ENV_1_V",
                      "ENV_2_V_LABEL", "ENV_2_V",
                      "ENV_3_V_LABEL", "ENV_3_V",
                      "NONAME2")
    new_col<-str_split_fixed(log_db$ENV_1_V_LABEL, "Debiased_", 2)
    log_db$R<-as.numeric(new_col[, 1])
    log_db$ENV_1_V_LABEL<-new_col[, 2]
    
    log_db$ENV_1_LABEL<-gsub("Debiased_", "", log_db$ENV_1_LABEL)
    log_db$ENV_2_LABEL<-gsub("Debiased_", "", log_db$ENV_2_LABEL)
    log_db$ENV_3_LABEL<-gsub("Debiased_", "", log_db$ENV_3_LABEL)
    log_db$ENV_2_V_LABEL<-gsub("Debiased_", "", log_db$ENV_2_V_LABEL)
    log_db$ENV_3_V_LABEL<-gsub("Debiased_", "", log_db$ENV_3_V_LABEL)
    log_db<-log_db %>% filter(log_db$ENV_1_V>(-1000))
    log_db<-log_db %>% filter(log_db$ENV_2_V>(-1000))
    log_db<-log_db %>% filter(log_db$ENV_3_V>(-1000))
  }
  if (EVO_TYPE==3){
  }
  log_db$SP_ID<-as.character(log_db$SP_ID)
  log_db<-as_tibble(log_db)
  fixed_db<-NULL
  for (var in c("Maximum_Monthly_Temperature", "Minimum_Monthly_Temperature", "Maximum_Monthly_Precipitation")){
    item<-NULL
    item_v<-NULL
    for (var2 in c("ENV_1", "ENV_2", "ENV_3")){
      
      print(paste(var, var2))
      sub<-log_db %>% filter(!!as.symbol(sprintf("%s_LABEL", var2))==var)
      sub<-sub [, c("Y","ID","UID","PARENT_UID","evoType","SP_ID",sprintf("%s_LABEL", var2),
                    sprintf("%s_MIN", var2), sprintf("%s_MAX", var2), "R")]
      colnames(sub)<-c("Y","ID","UID","PARENT_UID","evoType","SP_ID","ENV_LABEL","ENV_MIN","ENV_MAX", "R")
      
      sub_v<-log_db %>% filter(!!as.symbol(sprintf("%s_V_LABEL", var2))==var)
      sub_v<-sub_v [, c("Y","ID","UID","PARENT_UID","evoType","SP_ID",sprintf("%s_V_LABEL", var2), sprintf("%s_V", var2))]
      colnames(sub_v)<-c("Y","ID","UID","PARENT_UID","evoType","SP_ID","ENV_LABEL","ENV_V")
      
      
      if (is.null(item)){
        item<-sub
      }else{
        item<-bind_rows(sub, item)
      }
      
      if (is.null(item_v)){
        item_v<-sub_v
      }else{
        item_v<-bind_rows(sub_v, item_v)
      }
    }
    if (is.null(fixed_db)){
      fixed_db<-inner_join(item, item_v, by=c("Y","ID","UID","PARENT_UID","evoType","SP_ID","ENV_LABEL"))  
    }else{
      fixed_db<-bind_rows(fixed_db, 
                          inner_join(item, item_v, by=c("Y","ID","UID","PARENT_UID","evoType","SP_ID","ENV_LABEL")))
    }
    
    
  }
  fixed_db$LABLE<-s$label
  fixed_db$NB<-s$nb
  fixed_db$DA<-s$da
  
  fixed_db$EVO_TYPE<-as.numeric(ff[4])
  fixed_db$GLOBAL_ID<-s$global_id
  fixed_db$EVO_RATIO<-s$niche_envolution_individual_ratio
  if (nrow(nb_result)==0){
    nb_result<-fixed_db
  }else{
    nb_result<-bind_rows(nb_result, fixed_db)
  }
}

nb_result<- nb_result %>% filter((ENV_V>=ENV_MIN)&(ENV_V<=ENV_MAX))

fff<-nb_result[which((nb_result$Y==-137)&(nb_result$ENV_LABEL==var)),]
mean(fff$ENV_V, na.rm=TRUE)
nb_result$Y<-nb_result$Y - 1200
nb_result <- nb_result %>% filter(ENV_MIN, ENV_MAX, ENV_V)
mean_nb_df<-nb_result %>%
  dplyr::group_by(Y, NB, DA, EVO_RATIO, EVO_TYPE, ENV_LABEL, SP_ID) %>%
  dplyr::summarize(Mean_MIN = mean(ENV_MIN, na.rm=TRUE),
                   Mean_MAX = mean(ENV_MAX, na.rm=TRUE),
                   Mean_V = mean(ENV_V, na.rm=TRUE),
                   SD_MIN = sd(ENV_MIN),
                   SD_MAX = sd(ENV_MAX),
                   SD_V = sd(ENV_V),
                   CI_MIN = Rmisc::CI(ENV_MIN, ci=0.95)[2] - Rmisc::CI(ENV_MIN, ci=0.95)[3],
                   CI_MAX = Rmisc::CI(ENV_MAX, ci=0.95)[2] - Rmisc::CI(ENV_MAX, ci=0.95)[3],
                   CI_V = Rmisc::CI(ENV_V, ci=0.95)[2] - Rmisc::CI(ENV_V, ci=0.95)[3]
  )
mean_nb_df[which((mean_nb_df$ENV_LABEL==var)&(mean_nb_df$Mean_V>18)),]
range((mean_nb_df %>% filter(ENV_LABEL==var))[, "Mean_V"])
mean_nb_df[is.na(mean_nb_df)]<-0
for (var in c("Maximum_Monthly_Temperature", "Minimum_Monthly_Temperature", "Maximum_Monthly_Precipitation")){
  ggplot(mean_nb_df %>% filter(ENV_LABEL==var)) + 
    geom_line(aes(x=Y, y=Mean_MIN, color=factor(SP_ID))) +
    geom_line(aes(x=Y, y=Mean_MAX, color=factor(SP_ID)))+
    geom_line(aes(x=Y, y=Mean_V, color=factor(SP_ID)))+
    theme(legend.position = "none")
}

result$Y<-result$Y*-1
result$AVERAGE_N_CELL<-result$N_CELL/result$N_SP

mean_df<-result %>%
  dplyr::group_by(Y, SUITABLE, NB, DA, EVO_RATIO, EVO_TYPE) %>%
  dplyr::summarize(Mean_N_IND = mean(N_IND, na.rm=TRUE),
                   Mean_N_SP = mean(N_SP, na.rm=TRUE),
                   Mean_N_CELL = mean(N_CELL, na.rm=TRUE),
                   Mean_AVERAGE_N_CELL = mean(AVERAGE_N_CELL, na.rm=TRUE),
                   SD_N_IND = sd(N_IND),
                   SD_N_SP = sd(N_SP),
                   SD_N_CELL = sd(N_CELL),
                   SD_AVERAGE_N_CELL = sd(AVERAGE_N_CELL),
                   Median_N_IND = quantile(N_IND, na.rm=TRUE, .5),
                   Median_N_SP = quantile(N_SP, na.rm=TRUE, .5),
                   Median_N_CELL = quantile(N_CELL, na.rm=TRUE, .5),
                   Median_AVERAGE_N_CELL = quantile(AVERAGE_N_CELL, na.rm=TRUE, .5),
                   CI_N_IND = CI(N_IND, ci=0.95)[2] - CI(N_IND, ci=0.95)[3],
                   CI_N_SP = CI(N_SP, ci=0.95)[2] - CI(N_SP, ci=0.95)[3],
                   CI_N_CELL = CI(N_CELL, ci=0.95)[2] - CI(N_CELL, ci=0.95)[3],
                   CI_AVERAGE_N_CELL = CI(AVERAGE_N_CELL, ci=0.95)[2] - CI(AVERAGE_N_CELL, ci=0.95)[3]
  )
mean_df[is.na(mean_df)]<-0
mean_df$label<-paste(mean_df$NB, mean_df$DA, mean_df$EVO_RATIO, mean_df$EVO_TYPE)

mean_df_ratio<-result %>%
  dplyr::group_by(Y, SUITABLE, NB, DA, EVO_RATIO, EVO_TYPE, LABLE, GLOBAL_ID) %>%
  dplyr::summarize(N_IND = sum(N_IND, na.rm=TRUE),
                   N_SP = sum(N_SP, na.rm=TRUE),
                   N_CELL = sum(N_CELL, na.rm=TRUE)
  )
item_0<-mean_df_ratio %>% filter(SUITABLE==0)
item_1<-mean_df_ratio %>% filter(SUITABLE==1)
mean_df_ratio_merge<-full_join(item_0, item_1, by=c("Y", "NB", "DA", "EVO_RATIO", "EVO_TYPE", "LABLE", "GLOBAL_ID"))
mean_df_ratio_merge[is.na(mean_df_ratio_merge)]<-0
mean_df_ratio_merge$ratio<-mean_df_ratio_merge$N_IND.x/(mean_df_ratio_merge$N_IND.x+mean_df_ratio_merge$N_IND.y)
mean_df_ratio_df<-mean_df_ratio_merge %>%
  dplyr::group_by(Y, NB, DA, EVO_RATIO, EVO_TYPE) %>%
  dplyr::summarize(Mean_ratio = mean(ratio, na.rm=TRUE),
                   SD_ratio = sd(ratio),
                   Median_ratio = quantile(ratio, na.rm=TRUE, .5),
                   CI_ratio = CI(ratio, ci=0.95)[2] - CI(ratio, ci=0.95)[3]
  )
mean_df_ratio_df[is.na(mean_df_ratio_df)]<-0
mean_df_ratio_df$label<-paste(mean_df_ratio_df$NB, mean_df_ratio_df$DA, mean_df_ratio_df$EVO_RATIO, mean_df_ratio_df$EVO_TYPE)

comb<-expand.grid(unique(mean_df$NB), unique(mean_df$NB), stringsAsFactors=F)
cols<-rep(c("deepskyblue", "deepskyblue3", "black", 
            "khaki1", "khaki3", "black", 
            "hotpink", "hotpink3", "black", 
            "darkolivegreen3", "darkolivegreen", "black",
            "orangered3", "orangered4", "black",
            "black", "black", "black"), nrow(comb))
comb<-expand.grid(unique(mean_df$EVO_RATIO), unique(mean_df$EVO_TYPE), unique(mean_df$NB), unique(mean_df$DA), stringsAsFactors=F)
comb$label<-paste(comb$Var3, comb$Var4, comb$Var1, comb$Var2)
names(cols)<-comb$label

ggplot(mean_df %>% filter(SUITABLE==1), 
       aes(x=Y, y=Mean_N_SP))+
  geom_line(aes(color=factor(label)))+
  geom_ribbon(aes(ymin=Mean_N_SP-CI_N_SP, ymax=Mean_N_SP+CI_N_SP, fill=factor(label)), color=NA, alpha=0.3)+
  theme_bw()+
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))

ggplot(mean_df %>% filter(SUITABLE==1), 
       aes(x=Y, y=Mean_N_CELL))+
  geom_line(aes(color=factor(label)))+
  geom_ribbon(aes(ymin=Mean_N_CELL-CI_N_CELL, ymax=Mean_N_CELL+CI_N_CELL, fill=factor(label)), color=NA, alpha=0.3)+
  theme_bw()+
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))

ggplot(mean_df %>% filter((SUITABLE==1)&(Y<=-1100)), 
       aes(x=Y, y=Mean_N_CELL))+
  geom_line(aes(color=factor(label)))+
  geom_ribbon(aes(ymin=Mean_N_CELL-CI_N_CELL, ymax=Mean_N_CELL+CI_N_CELL, fill=factor(label)), color=NA, alpha=0.3)+
  theme_bw()+
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))

ggplot(mean_df %>% filter(SUITABLE==1), 
       aes(x=Y, y=Mean_AVERAGE_N_CELL))+
  geom_line(aes(color=factor(label)))+
  geom_ribbon(aes(ymin=Mean_AVERAGE_N_CELL-CI_AVERAGE_N_CELL, ymax=Mean_AVERAGE_N_CELL+CI_AVERAGE_N_CELL, fill=factor(label)), color=NA, alpha=0.3)+
  theme_bw()+
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))

ggplot(mean_df_ratio_df, aes(x=Y, y=Mean_ratio))+
  geom_line(aes(color=factor(label)))+
  geom_ribbon(aes(ymin=Mean_ratio-CI_ratio, ymax=Mean_ratio+CI_ratio, fill=factor(label)), color=NA, alpha=0.3)+
  theme_bw()+
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+
  ylim(0, 0.4)

ggplot(mean_df_ratio_df %>% filter((Y<=-1100)&(Y>=-1198)), aes(x=Y, y=Mean_ratio))+
  geom_line(aes(color=factor(label)))+
  geom_ribbon(aes(ymin=Mean_ratio-CI_ratio, ymax=Mean_ratio+CI_ratio, fill=factor(label)), color=NA, alpha=0.3)+
  theme_bw()+
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))



nb<-read.table(sprintf("%s/RESULTS_TEST/%s/%s.nb.log", base, "12337_POOR_NARROW_5_0.1", "12337_POOR_NARROW_5_0.1"), 
               head=F, sep=",", stringsAsFactors = F)


head(nb)
tail(nb)
nb$p1<-nb$V18-nb$V17
nb$p2<-nb$V19-nb$V18


plot(nb$V1, nb$p1)
plot(nb$V1, nb$p2)

hist(nb$V17)
hist(nb$V18)
hist(nb$V19)

label<-"12337_POOR_NARROW_5_0.01"
label<-"14823_GOOD_MODERATE_5_0.01"
label<-"12337_POOR_NARROW_6_0.01"

nb_4<-read.csv(sprintf("%s/RESULTS_TEST/%s/%s.nb_4.log", base, label, label), 
               head=F, sep=",", stringsAsFactors = F)

nb<-read.csv(sprintf("%s/RESULTS_TEST/%s/%s.nb.log", base, label, label), 
               head=F, sep=",", stringsAsFactors = F)

ggplot(nb) + geom_point(aes(x=V1, y=V8))

i<-8
shape <- readOGR(dsn = "/home/huijieqiao/git/ees_3d_data/SMART_SPECIES/ISEA3H8/isea3hGen/outputfiles", 
                 layer = "isea3h8p")
continent<-readOGR(dsn = "/media/huijieqiao/Butterfly/GISLayers/continents", 
                   layer = "continent")

plot(continent)
for (i in unique(nb$V1)){
  
  shape_t<-shape
  item<-nb %>% filter(V1==i)
  print(i, nrow(item))
  shape_t<-shape
  shape_t@coords<-subset(shape_t@coords, shape_t$global_id %in% item$V2)
  plot(shape_t, add=T)
}
table(nb$V2)

head(nb)
tail(nb)

head(nb_4[which(nb_4$V6!=0.25),], 100)

tail(nb_4[which(nb_4$V6!=0.25),], 100)

head(nb_4[which((nb_4$V3==10960)),], 20)
head(nb_4[which((nb_4$V1==55)&(nb_4$V3==10960)),], 100)

head(nb_4[which((nb_4$V6>0.25)),], 20)

N_Cell<-data.frame(table(nb_4$V3))
N_Cell[which(N_Cell$Freq==max(N_Cell$Freq)),]
nb_4_sub<-nb_4[which(nb_4$V3==10960),]
nb_sub<-nb[which(nb$V2==10960),]

nb_4_sub<-nb_4[which(nb_4$V3==12172),]

ggplot(nb_4_sub[which(nb_4_sub$V8==1),]) + geom_point(aes(x=V1, y=V6, color=factor(V4)))+
  geom_line(aes(x=V1, y=V6, color=factor(V4)))


mean_df<-result %>%
  dplyr::group_by(Y, SUITABLE, NB, DA, EVO_RATIO, EVO_TYPE) %>%
  dplyr::summarize(Mean_N_IND = mean(N_IND, na.rm=TRUE),
                   Mean_N_SP = mean(N_SP, na.rm=TRUE),
                   Mean_N_CELL = mean(N_CELL, na.rm=TRUE),
                   Mean_AVERAGE_N_CELL = mean(AVERAGE_N_CELL, na.rm=TRUE),
                   SD_N_IND = sd(N_IND),
                   SD_N_SP = sd(N_SP),
                   SD_N_CELL = sd(N_CELL),
                   SD_AVERAGE_N_CELL = sd(AVERAGE_N_CELL),
                   Median_N_IND = quantile(N_IND, na.rm=TRUE, .5),
                   Median_N_SP = quantile(N_SP, na.rm=TRUE, .5),
                   Median_N_CELL = quantile(N_CELL, na.rm=TRUE, .5),
                   Median_AVERAGE_N_CELL = quantile(AVERAGE_N_CELL, na.rm=TRUE, .5),
                   CI_N_IND = CI(N_IND, ci=0.95)[2] - CI(N_IND, ci=0.95)[3],
                   CI_N_SP = CI(N_SP, ci=0.95)[2] - CI(N_SP, ci=0.95)[3],
                   CI_N_CELL = CI(N_CELL, ci=0.95)[2] - CI(N_CELL, ci=0.95)[3],
                   CI_AVERAGE_N_CELL = CI(AVERAGE_N_CELL, ci=0.95)[2] - CI(AVERAGE_N_CELL, ci=0.95)[3]
  )

library(Rmisc)
library(stringr)
library(ggplot2)
library(dplyr)
setwd("~/git/ees_3d/R/smart_species")
base<-"/home/huijieqiao/git/ees_3d_data/SMART_SPECIES"
result_all<-NULL
evo_type_c<-c(1,2,3,4,5,7,8,9)
for (i in evo_type_c){
  print(i)
  result<-readRDS(sprintf("%s/Tables/individual_ratio_%d.rda", base, i))
  result$Y<-result$Y*-1
  if (is.null(result_all)){
    result_all<-result
  }else{
    result_all<-bind_rows(result_all, result)
  }
}

result_all$AVERAGE_N_CELL<-result_all$N_CELL/result_all$N_SP
saveRDS(result_all, sprintf("%s/Tables/individual_ratio.rda", base))


result<-readRDS(sprintf("%s/Tables/individual_ratio.rda", base))
result<-result%>%ungroup()
NULL_DF<-result %>% dplyr::filter(EVO_TYPE==1)
result<-result %>% dplyr::filter(EVO_TYPE!=1)
NULL_DF$EVO_RATIO<-0.005
result<-bind_rows(result, NULL_DF)
NULL_DF$EVO_RATIO<-0.05
result<-bind_rows(result, NULL_DF)
result[which(result$EVO_RATIO==0.1), "EVO_RATIO"]<-0.05
result[which(result$EVO_RATIO==0.01), "EVO_RATIO"]<-0.005
unique(result$EVO_TYPE)
unique(result$Y)

#result<-result_all
N_Sim<-result %>%
  dplyr::group_by(Y, NB, DA, EVO_RATIO, EVO_TYPE) %>%
  dplyr::summarize(N=length(unique(GLOBAL_ID))
  )
N_Sim$label<-paste(N_Sim$NB, N_Sim$DA, N_Sim$EVO_RATIO, N_Sim$EVO_TYPE)
saveRDS(N_Sim, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/N_Sim.rda")


mean_df<-result %>%
  dplyr::group_by(Y, SUITABLE, NB, DA, EVO_RATIO, EVO_TYPE) %>%
  dplyr::summarize(Mean_N_IND = mean(N_IND, na.rm=TRUE),
                   Mean_N_SP = mean(N_SP, na.rm=TRUE),
                   Mean_N_CELL = mean(N_CELL, na.rm=TRUE),
                   Mean_AVERAGE_N_CELL = mean(AVERAGE_N_CELL, na.rm=TRUE),
                   Mean_N_UNIQUE_CELL = mean(N_UNIQUE_CELL, na.rm=TRUE),
                   SD_N_IND = sd(N_IND),
                   SD_N_SP = sd(N_SP),
                   SD_N_CELL = sd(N_CELL),
                   SD_AVERAGE_N_CELL = sd(AVERAGE_N_CELL),
                   SD_N_UNIQUE_CELL = sd(N_UNIQUE_CELL),
                   Median_N_IND = quantile(N_IND, na.rm=TRUE, .5),
                   Median_N_SP = quantile(N_SP, na.rm=TRUE, .5),
                   Median_N_CELL = quantile(N_CELL, na.rm=TRUE, .5),
                   Median_AVERAGE_N_CELL = quantile(AVERAGE_N_CELL, na.rm=TRUE, .5),
                   Median_N_UNIQUE_CELL = quantile(N_UNIQUE_CELL, na.rm=TRUE, .5),
                   CI_N_IND_LOW = CI(N_IND, ci=0.95)[3],
                   CI_N_IND_MEAN = CI(N_IND, ci=0.95)[2],
                   CI_N_IND_HIGH = CI(N_IND, ci=0.95)[1],
                   CI_N_SP_LOW = CI(N_SP, ci=0.95)[3],
                   CI_N_SP_MEAN = CI(N_SP, ci=0.95)[2],
                   CI_N_SP_HIGH = CI(N_SP, ci=0.95)[1],
                   CI_N_CELL_LOW = CI(N_CELL, ci=0.95)[3],
                   CI_N_CELL_MEAN = CI(N_CELL, ci=0.95)[2],
                   CI_N_CELL_HIGH = CI(N_CELL, ci=0.95)[1],
                   CI_AVERAGE_N_CELL_LOW = CI(AVERAGE_N_CELL, ci=0.95)[3],
                   CI_AVERAGE_N_CELL_MEAN = CI(AVERAGE_N_CELL, ci=0.95)[2],
                   CI_AVERAGE_N_CELL_HIGH = CI(AVERAGE_N_CELL, ci=0.95)[1],
                   CI_N_UNIQUE_CELL_LOW = CI(N_UNIQUE_CELL, ci=0.95)[3],
                   CI_N_UNIQUE_CELL_MEAN = CI(N_UNIQUE_CELL, ci=0.95)[2],
                   CI_N_UNIQUE_CELL_HIGH = CI(N_UNIQUE_CELL, ci=0.95)[1]
  )
mean_df[is.na(mean_df)]<-0
mean_df$label<-paste(mean_df$NB, mean_df$DA, mean_df$EVO_RATIO, mean_df$EVO_TYPE)

saveRDS(mean_df, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/mean_df.rda")

mean_df_ratio<-result %>%
  dplyr::group_by(Y, SUITABLE, NB, DA, EVO_RATIO, EVO_TYPE, LABLE, GLOBAL_ID) %>%
  dplyr::summarize(N_IND = sum(N_IND, na.rm=TRUE),
                   N_SP = sum(N_SP, na.rm=TRUE),
                   N_CELL = sum(N_CELL, na.rm=TRUE),
                   N_UNIQUE_CELL = sum(N_UNIQUE_CELL, na.rm=TRUE)
  )
item_0<-mean_df_ratio %>% filter(SUITABLE==0)
item_1<-mean_df_ratio %>% filter(SUITABLE==1)
mean_df_ratio_merge<-full_join(item_0, item_1, by=c("Y", "NB", "DA", "EVO_RATIO", "EVO_TYPE", "LABLE", "GLOBAL_ID"))
mean_df_ratio_merge[is.na(mean_df_ratio_merge)]<-0
mean_df_ratio_merge$ratio<-mean_df_ratio_merge$N_IND.x/(mean_df_ratio_merge$N_IND.x+mean_df_ratio_merge$N_IND.y)
saveRDS(mean_df_ratio_merge, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/mean_df_ratio_merge.rda")

mean_df_ratio_df<-mean_df_ratio_merge %>%
  dplyr::group_by(Y, NB, DA, EVO_RATIO, EVO_TYPE) %>%
  dplyr::summarize(Mean_ratio = mean(ratio, na.rm=TRUE),
                   SD_ratio = sd(ratio),
                   Median_ratio = quantile(ratio, na.rm=TRUE, .5),
                   CI_ratio = CI(ratio, ci=0.95)[2] - CI(ratio, ci=0.95)[3]
  )
mean_df_ratio_df[is.na(mean_df_ratio_df)]<-0
mean_df_ratio_df$label<-paste(mean_df_ratio_df$NB, mean_df_ratio_df$DA, mean_df_ratio_df$EVO_RATIO, mean_df_ratio_df$EVO_TYPE)
saveRDS(mean_df_ratio_df, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/mean_df_ratio_df.rda")


function(x){
  x[which(x==1)]<-"Lazy"
  x[which(x==2)]<-"Darwin"
  x[which(x==3)]<-"AI Lamarck"
  x[which(x==4)]<-"AI"
  x[which(x==5)]<-"Lamarck II"
  x[which(x==6)]<-"Darwin xII"
  x[which(x==7)]<-"Darwin II"
  x[which(x==8)]<-"Combined"
  x[which(x==9)]<-"Lamarck"
  x
}

fix_df<-function(p_df){
  #p_df<-p_df %>% filter((EVO_TYPE!=6))
  #p_df[which(p_df$EVO_RATIO==0.01), "EVO_RATIO"]<-0.005
  #p_df[which(p_df$EVO_RATIO==0.1), "EVO_RATIO"]<-0.05
  #p_df_1<-p_df %>%filter(EVO_RATIO==1)
  #p_df_1$EVO_RATIO<-0.05
  #p_df[which(p_df$EVO_RATIO==1), "EVO_RATIO"]<-0.005
  #p_df<-bind_rows(p_df, p_df_1)
  #p_df$EVO_RATIO<-1
  p_df$EVO_TYPE<-fix_type(p_df$EVO_TYPE)
  p_df$WARP_LABEL<-paste(p_df$NB, p_df$DA, p_df$EVO_RATIO)
  p_df
}

NULL_DF<-result %>% filter(EVO_TYPE==1)
colnames(NULL_DF)[c(3, 4, 5, 6, 7, 10, 12, 13)]<-
  paste("NULL", colnames(NULL_DF)[c(3, 4, 5, 6, 7, 10, 12, 13)], sep="_")


FULL_DF<-expand.grid(GLOBAL_ID=unique(NULL_DF$GLOBAL_ID),
                     DA=unique(NULL_DF$DA),
                     NB=unique(NULL_DF$NB),
                     Y=unique(NULL_DF$Y),
                     SUITABLE=unique(NULL_DF$SUITABLE),
                     stringsAsFactors = F
)

FULL_DF<-left_join(FULL_DF, NULL_DF, by=c("Y", "SUITABLE", "NB", "DA", "GLOBAL_ID"))

FULL_DF$NULL_EVO_TYPE<-1
FULL_DF$NULL_EVO_RATIO<-1
FULL_DF[is.na(FULL_DF)]<-0
FULL_DF<-FULL_DF[, c("GLOBAL_ID", "DA", "NB", "Y", "SUITABLE", "NULL_N_IND", 
                     "NULL_N_SP", "NULL_N_CELL", "NULL_AVERAGE_N_CELL", "NULL_N_UNIQUE_CELL")]

FULL_DF_2<-NULL

for (EVO_TYPE in evo_type_c){
  print(EVO_TYPE)
  if (EVO_TYPE==1){
    ITEM<-FULL_DF
    ITEM$EVO_TYPE<-EVO_TYPE
    ITEM$EVO_RATIO<-1
  }else{
    FULL_DF_0.1<-FULL_DF
    FULL_DF_0.1$EVO_TYPE<-EVO_TYPE
    #FULL_DF_0.1$EVO_RATIO<-0.1
    
    FULL_DF_0.01<-FULL_DF
    FULL_DF_0.01$EVO_TYPE<-EVO_TYPE
    #FULL_DF_0.01$EVO_RATIO<-0.01
    ITEM<-bind_rows(FULL_DF_0.1, FULL_DF_0.01)
  }
  if (is.null(FULL_DF_2)){
    FULL_DF_2<-ITEM
  }else{
    FULL_DF_2<-bind_rows(FULL_DF_2, ITEM)
  }
}

#result[which(result$EVO_RATIO==0.005), "EVO_RATIO"]<-0.01
#result[which(result$EVO_RATIO==0.05), "EVO_RATIO"]<-0.1

ALL_DF<-left_join(FULL_DF_2, result, by=c("Y", "SUITABLE", "NB", "DA", "GLOBAL_ID", "EVO_TYPE", "EVO_RATIO"))

dim(ALL_DF)
head(ALL_DF)
tail(ALL_DF)
ALL_DF$LABLE<-paste(ALL_DF$GLOBAL_ID, ALL_DF$NB, ALL_DF$DA, ALL_DF$EVO_RATIO, ALL_DF$EVO_TYPE, sep="_")
#saveRDS(ALL_DF, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/ALL_DF_FULL.rda")

#ALL_DF<-ALL_DF%>%filter((EVO_TYPE!=6)&(EVO_RATIO!=0.1))

ALL_DF[is.na(ALL_DF)]<-0

ALL_DF$N_IND_DIFF<-0
ALL_DF[which(ALL_DF$N_IND>ALL_DF$NULL_N_IND), "N_IND_DIFF"]<-1
ALL_DF[which(ALL_DF$N_IND<ALL_DF$NULL_N_IND), "N_IND_DIFF"]<--1

ALL_DF$N_SP_DIFF<-0
ALL_DF[which(ALL_DF$N_SP>ALL_DF$NULL_N_SP), "N_SP_DIFF"]<-1
ALL_DF[which(ALL_DF$N_SP<ALL_DF$NULL_N_SP), "N_SP_DIFF"]<--1

ALL_DF$N_CELL_DIFF<-0
ALL_DF[which(ALL_DF$N_CELL>ALL_DF$NULL_N_CELL), "N_CELL_DIFF"]<-1
ALL_DF[which(ALL_DF$N_CELL<ALL_DF$NULL_N_CELL), "N_CELL_DIFF"]<--1

ALL_DF$N_UNIQUE_CELL_DIFF<-0
ALL_DF[which(ALL_DF$N_UNIQUE_CELL>ALL_DF$NULL_N_UNIQUE_CELL), "N_UNIQUE_CELL_DIFF"]<-1
ALL_DF[which(ALL_DF$N_UNIQUE_CELL<ALL_DF$NULL_N_UNIQUE_CELL), "N_UNIQUE_CELL_DIFF"]<--1

saveRDS(ALL_DF, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/ALL_DF.rda")


count_N_IND_DIFF<-ALL_DF %>% dplyr::group_by(Y, SUITABLE, NB, DA, N_IND_DIFF, EVO_TYPE, EVO_RATIO)%>% 
  dplyr::count()

count_N_IND_DIFF$label<-paste(count_N_IND_DIFF$NB, count_N_IND_DIFF$DA, count_N_IND_DIFF$EVO_RATIO, count_N_IND_DIFF$EVO_TYPE)

if (F){
  View(count_N_IND_DIFF %>% filter((Y==-0)&(SUITABLE==1)&(EVO_TYPE!=1)&(DA=="GOOD")&(NB=="MODERATE")&(EVO_RATIO==0.01)))
  ALL_DF %>% filter((Y==-0)&(SUITABLE==1)&(EVO_TYPE!=1))
  ALL_DF %>% filter((Y==-0)&(SUITABLE==1)&(EVO_TYPE!=1)&(GLOBAL_ID==19085))
  
  temp_a<-result%>%dplyr::ungroup()%>%dplyr::distinct(Y, GLOBAL_ID, NB, DA, EVO_TYPE, EVO_RATIO, SUITABLE)
  temp2_a<-temp_a %>%filter(SUITABLE==1) %>% dplyr::group_by(NB, DA, EVO_TYPE, EVO_RATIO, Y) %>% dplyr::count()
  temp2_a$label<-paste(temp2_a$NB, temp2_a$DA, temp2_a$EVO_RATIO, temp2_a$EVO_TYPE)
  View(temp2_a%>%filter(Y==0))
  temp<-ALL_DF%>%dplyr::group_by(DA, NB, Y, SUITABLE, EVO_TYPE, EVO_RATIO, N_IND_DIFF)%>%dplyr::count()
  
  temp2<-temp %>%filter(SUITABLE==1) %>% dplyr::group_by(NB, DA, EVO_TYPE, EVO_RATIO, Y, N_IND_DIFF) %>% dplyr::count()
  
  temp2$label<-paste(temp2$NB, temp2$DA, temp2$EVO_RATIO, temp2$EVO_TYPE, temp2$N_IND_DIFF)
  ggplot(temp2)+geom_line(aes(x=Y, y=n, color=factor(label)))+
    geom_text(data = subset(temp2, Y == 0), aes(label = label, colour = label, x = 0, y =n), hjust = -.1)+
    xlim(c(-1200, 350))+
    theme(legend.position = "none") 
  #
  View(temp2 %>% filter(Y==0))
}

saveRDS(count_N_IND_DIFF, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/count_N_IND_DIFF.rda")
count_N_IND_DIFF<-fix_df(count_N_IND_DIFF)
saveRDS(count_N_IND_DIFF, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/count_N_IND_DIFF_Fixed.rda")

count_N_SP_DIFF<-ALL_DF %>% dplyr::group_by(Y, SUITABLE, NB, DA, N_SP_DIFF, EVO_TYPE, EVO_RATIO)%>%
  dplyr::count()
count_N_SP_DIFF$label<-paste(count_N_SP_DIFF$NB, count_N_SP_DIFF$DA, count_N_SP_DIFF$EVO_RATIO, count_N_SP_DIFF$EVO_TYPE)
saveRDS(count_N_SP_DIFF, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/count_N_SP_DIFF.rda")
count_N_SP_DIFF<-fix_df(count_N_SP_DIFF)
saveRDS(count_N_SP_DIFF, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/count_N_SP_DIFF_Fixed.rda")


count_N_CELL_DIFF<-ALL_DF %>% dplyr::group_by(Y, SUITABLE, NB, DA, N_CELL_DIFF, EVO_TYPE, EVO_RATIO)%>% 
  dplyr::count()
count_N_CELL_DIFF$label<-paste(count_N_CELL_DIFF$NB, count_N_CELL_DIFF$DA, count_N_CELL_DIFF$EVO_RATIO, count_N_CELL_DIFF$EVO_TYPE)
saveRDS(count_N_CELL_DIFF, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/count_N_CELL_DIFF.rda")
count_N_CELL_DIFF<-fix_df(count_N_CELL_DIFF)
saveRDS(count_N_CELL_DIFF, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/count_N_CELL_DIFF_Fixed.rda")

count_N_UNIQUE_CELL_DIFF<-ALL_DF %>% dplyr::group_by(Y, SUITABLE, NB, DA, N_UNIQUE_CELL_DIFF, EVO_TYPE, EVO_RATIO)%>% 
  dplyr::count()
count_N_UNIQUE_CELL_DIFF$label<-paste(count_N_UNIQUE_CELL_DIFF$NB, count_N_UNIQUE_CELL_DIFF$DA, count_N_UNIQUE_CELL_DIFF$EVO_RATIO, count_N_UNIQUE_CELL_DIFF$EVO_TYPE)
saveRDS(count_N_UNIQUE_CELL_DIFF, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/count_N_UNIQUE_CELL_DIFF.rda")
count_N_UNIQUE_CELL_DIFF<-fix_df(count_N_UNIQUE_CELL_DIFF)
saveRDS(count_N_UNIQUE_CELL_DIFF, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/count_N_UNIQUE_CELL_DIFF_Fixed.rda")

sort_N_CELL<-ALL_DF%>% dplyr::filter((SUITABLE==1)&(EVO_RATIO!=0.005)) %>%
  dplyr::group_by(Y, GLOBAL_ID, DA, NB) %>% 
  dplyr::mutate(RANK = rank(N_CELL, ties.method = "average"))
sort_N_CELL<-fix_df(sort_N_CELL)
saveRDS(sort_N_CELL, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/sort_N_CELL_Fixed_0.05.rda")
mean_sort_N_CELL<-sort_N_CELL %>%
  dplyr::group_by(Y, NB, DA, EVO_TYPE) %>%
  dplyr::summarize(Mean_RANK = mean(RANK, na.rm=TRUE),
                   SD_RANK = sd(RANK),
                   Median_RANK = quantile(RANK, na.rm=TRUE, .5),
                   CI_RANK_HIGH = CI(RANK, ci=0.95)[1],
                   CI_RANK_MEAN = CI(RANK, ci=0.95)[2],
                   CI_RANK_LOW = CI(RANK, ci=0.95)[3]
  )
mean_sort_N_CELL[is.na(mean_sort_N_CELL)]<-0
#mean_sort_N_CELL<-fix_df(mean_sort_N_CELL)
mean_sort_N_CELL$label<-paste(mean_sort_N_CELL$NB, mean_sort_N_CELL$DA, mean_sort_N_CELL$EVO_TYPE)
saveRDS(mean_sort_N_CELL, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/mean_sort_N_CELL_Fixed_0.05.rda")

sort_N_CELL<-ALL_DF%>% dplyr::filter((SUITABLE==1)&(EVO_RATIO!=0.05)) %>%
  dplyr::group_by(Y, GLOBAL_ID, DA, NB) %>% 
  dplyr::mutate(RANK = rank(N_CELL, ties.method = "average"))
sort_N_CELL<-fix_df(sort_N_CELL)
saveRDS(sort_N_CELL, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/sort_N_CELL_Fixed_0.005.rda")

mean_sort_N_CELL<-sort_N_CELL %>%
  dplyr::group_by(Y, NB, DA, EVO_TYPE) %>%
  dplyr::summarize(Mean_RANK = mean(RANK, na.rm=TRUE),
                   SD_RANK = sd(RANK),
                   Median_RANK = quantile(RANK, na.rm=TRUE, .5),
                   CI_RANK_HIGH = CI(RANK, ci=0.95)[1],
                   CI_RANK_MEAN = CI(RANK, ci=0.95)[2],
                   CI_RANK_LOW = CI(RANK, ci=0.95)[3]
  )
mean_sort_N_CELL[is.na(mean_sort_N_CELL)]<-0
#mean_sort_N_CELL<-fix_df(mean_sort_N_CELL)
mean_sort_N_CELL$label<-paste(mean_sort_N_CELL$NB, mean_sort_N_CELL$DA, mean_sort_N_CELL$EVO_TYPE)
saveRDS(mean_sort_N_CELL, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/mean_sort_N_CELL_Fixed_0.005.rda")

#AVERAGE_N_CELL
sort_AVERAGE_N_CELL<-ALL_DF%>% dplyr::filter((SUITABLE==1)&(EVO_RATIO!=0.005)) %>%
  dplyr::group_by(Y, GLOBAL_ID, DA, NB) %>% 
  dplyr::mutate(RANK = rank(AVERAGE_N_CELL, ties.method = "average"))
sort_AVERAGE_N_CELL<-fix_df(sort_AVERAGE_N_CELL)
saveRDS(sort_AVERAGE_N_CELL, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/sort_AVERAGE_N_CELL_Fixed_0.05.rda")
mean_sort_AVERAGE_N_CELL<-sort_AVERAGE_N_CELL %>%
  dplyr::group_by(Y, NB, DA, EVO_TYPE) %>%
  dplyr::summarize(Mean_RANK = mean(RANK, na.rm=TRUE),
                   SD_RANK = sd(RANK),
                   Median_RANK = quantile(RANK, na.rm=TRUE, .5),
                   CI_RANK_HIGH = CI(RANK, ci=0.95)[1],
                   CI_RANK_MEAN = CI(RANK, ci=0.95)[2],
                   CI_RANK_LOW = CI(RANK, ci=0.95)[3]
  )
mean_sort_AVERAGE_N_CELL[is.na(mean_sort_AVERAGE_N_CELL)]<-0
#mean_sort_AVERAGE_N_CELL<-fix_df(mean_sort_AVERAGE_N_CELL)
mean_sort_AVERAGE_N_CELL$label<-paste(mean_sort_AVERAGE_N_CELL$NB, mean_sort_AVERAGE_N_CELL$DA, mean_sort_AVERAGE_N_CELL$EVO_TYPE)
saveRDS(mean_sort_AVERAGE_N_CELL, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/mean_sort_AVERAGE_N_CELL_Fixed_0.05.rda")

sort_AVERAGE_N_CELL<-ALL_DF%>% dplyr::filter((SUITABLE==1)&(EVO_RATIO!=0.05)) %>%
  dplyr::group_by(Y, GLOBAL_ID, DA, NB) %>% 
  dplyr::mutate(RANK = rank(AVERAGE_N_CELL, ties.method = "average"))
sort_AVERAGE_N_CELL<-fix_df(sort_AVERAGE_N_CELL)
saveRDS(sort_AVERAGE_N_CELL, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/sort_AVERAGE_N_CELL_Fixed_0.005.rda")

mean_sort_AVERAGE_N_CELL<-sort_AVERAGE_N_CELL %>%
  dplyr::group_by(Y, NB, DA, EVO_TYPE) %>%
  dplyr::summarize(Mean_RANK = mean(RANK, na.rm=TRUE),
                   SD_RANK = sd(RANK),
                   Median_RANK = quantile(RANK, na.rm=TRUE, .5),
                   CI_RANK_HIGH = CI(RANK, ci=0.95)[1],
                   CI_RANK_MEAN = CI(RANK, ci=0.95)[2],
                   CI_RANK_LOW = CI(RANK, ci=0.95)[3]
  )
mean_sort_AVERAGE_N_CELL[is.na(mean_sort_AVERAGE_N_CELL)]<-0
#mean_sort_AVERAGE_N_CELL<-fix_df(mean_sort_AVERAGE_N_CELL)
mean_sort_AVERAGE_N_CELL$label<-paste(mean_sort_AVERAGE_N_CELL$NB, mean_sort_AVERAGE_N_CELL$DA, mean_sort_AVERAGE_N_CELL$EVO_TYPE)
saveRDS(mean_sort_AVERAGE_N_CELL, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/mean_sort_AVERAGE_N_CELL_Fixed_0.005.rda")

#N_SP
sort_N_SP<-ALL_DF%>% dplyr::filter((SUITABLE==1)&(EVO_RATIO!=0.005)) %>%
  dplyr::group_by(Y, GLOBAL_ID, DA, NB) %>% 
  dplyr::mutate(RANK = rank(N_SP, ties.method = "average"))
sort_N_SP<-fix_df(sort_N_SP)
saveRDS(sort_N_SP, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/sort_N_SP_Fixed_0.05.rda")
mean_sort_N_SP<-sort_N_SP %>%
  dplyr::group_by(Y, NB, DA, EVO_TYPE) %>%
  dplyr::summarize(Mean_RANK = mean(RANK, na.rm=TRUE),
                   SD_RANK = sd(RANK),
                   Median_RANK = quantile(RANK, na.rm=TRUE, .5),
                   CI_RANK_HIGH = CI(RANK, ci=0.95)[1],
                   CI_RANK_MEAN = CI(RANK, ci=0.95)[2],
                   CI_RANK_LOW = CI(RANK, ci=0.95)[3]
  )
mean_sort_N_SP[is.na(mean_sort_N_SP)]<-0
#mean_sort_N_SP<-fix_df(mean_sort_N_SP)
mean_sort_N_SP$label<-paste(mean_sort_N_SP$NB, mean_sort_N_SP$DA, mean_sort_N_SP$EVO_TYPE)
saveRDS(mean_sort_N_SP, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/mean_sort_N_SP_Fixed_0.05.rda")

sort_N_SP<-ALL_DF%>% dplyr::filter((SUITABLE==1)&(EVO_RATIO!=0.05)) %>%
  dplyr::group_by(Y, GLOBAL_ID, DA, NB) %>% 
  dplyr::mutate(RANK = rank(N_SP, ties.method = "average"))
sort_N_SP<-fix_df(sort_N_SP)
saveRDS(sort_N_SP, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/sort_N_SP_Fixed_0.005.rda")

mean_sort_N_SP<-sort_N_SP %>%
  dplyr::group_by(Y, NB, DA, EVO_TYPE) %>%
  dplyr::summarize(Mean_RANK = mean(RANK, na.rm=TRUE),
                   SD_RANK = sd(RANK),
                   Median_RANK = quantile(RANK, na.rm=TRUE, .5),
                   CI_RANK_HIGH = CI(RANK, ci=0.95)[1],
                   CI_RANK_MEAN = CI(RANK, ci=0.95)[2],
                   CI_RANK_LOW = CI(RANK, ci=0.95)[3]
  )
mean_sort_N_SP[is.na(mean_sort_N_SP)]<-0
#mean_sort_N_SP<-fix_df(mean_sort_N_SP)
mean_sort_N_SP$label<-paste(mean_sort_N_SP$NB, mean_sort_N_SP$DA, mean_sort_N_SP$EVO_TYPE)
saveRDS(mean_sort_N_SP, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/mean_sort_N_SP_Fixed_0.005.rda")

#N_UNIQUE_CELL
sort_N_UNIQUE_CELL<-ALL_DF%>% dplyr::filter((SUITABLE==1)&(EVO_RATIO!=0.005)) %>%
  dplyr::group_by(Y, GLOBAL_ID, DA, NB) %>% 
  dplyr::mutate(RANK = rank(N_UNIQUE_CELL, ties.method = "average"))
sort_N_UNIQUE_CELL<-fix_df(sort_N_UNIQUE_CELL)
saveRDS(sort_N_UNIQUE_CELL, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/sort_N_UNIQUE_CELL_Fixed_0.05.rda")
mean_sort_N_UNIQUE_CELL<-sort_N_UNIQUE_CELL %>%
  dplyr::group_by(Y, NB, DA, EVO_TYPE) %>%
  dplyr::summarize(Mean_RANK = mean(RANK, na.rm=TRUE),
                   SD_RANK = sd(RANK),
                   Median_RANK = quantile(RANK, na.rm=TRUE, .5),
                   CI_RANK_HIGH = CI(RANK, ci=0.95)[1],
                   CI_RANK_MEAN = CI(RANK, ci=0.95)[2],
                   CI_RANK_LOW = CI(RANK, ci=0.95)[3]
  )
mean_sort_N_UNIQUE_CELL[is.na(mean_sort_N_UNIQUE_CELL)]<-0
#mean_sort_N_UNIQUE_CELL<-fix_df(mean_sort_N_UNIQUE_CELL)
mean_sort_N_UNIQUE_CELL$label<-paste(mean_sort_N_UNIQUE_CELL$NB, mean_sort_N_UNIQUE_CELL$DA, mean_sort_N_UNIQUE_CELL$EVO_TYPE)
saveRDS(mean_sort_N_UNIQUE_CELL, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/mean_sort_N_UNIQUE_CELL_Fixed_0.05.rda")

sort_N_UNIQUE_CELL<-ALL_DF%>% dplyr::filter((SUITABLE==1)&(EVO_RATIO!=0.05)) %>%
  dplyr::group_by(Y, GLOBAL_ID, DA, NB) %>% 
  dplyr::mutate(RANK = rank(N_UNIQUE_CELL, ties.method = "average"))
sort_N_UNIQUE_CELL<-fix_df(sort_N_UNIQUE_CELL)
saveRDS(sort_N_UNIQUE_CELL, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/sort_N_UNIQUE_CELL_Fixed_0.005.rda")

mean_sort_N_UNIQUE_CELL<-sort_N_UNIQUE_CELL %>%
  dplyr::group_by(Y, NB, DA, EVO_TYPE) %>%
  dplyr::summarize(Mean_RANK = mean(RANK, na.rm=TRUE),
                   SD_RANK = sd(RANK),
                   Median_RANK = quantile(RANK, na.rm=TRUE, .5),
                   CI_RANK_HIGH = CI(RANK, ci=0.95)[1],
                   CI_RANK_MEAN = CI(RANK, ci=0.95)[2],
                   CI_RANK_LOW = CI(RANK, ci=0.95)[3]
  )
mean_sort_N_UNIQUE_CELL[is.na(mean_sort_N_UNIQUE_CELL)]<-0
#mean_sort_N_UNIQUE_CELL<-fix_df(mean_sort_N_UNIQUE_CELL)
mean_sort_N_UNIQUE_CELL$label<-paste(mean_sort_N_UNIQUE_CELL$NB, mean_sort_N_UNIQUE_CELL$DA, mean_sort_N_UNIQUE_CELL$EVO_TYPE)
saveRDS(mean_sort_N_UNIQUE_CELL, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/mean_sort_N_UNIQUE_CELL_Fixed_0.005.rda")

target_label<-result %>% dplyr::filter((Y==0)&(EVO_TYPE==3))

target_labels<-paste(target_label$NB, target_label$DA, target_label$EVO_RATIO, target_label$GLOBAL_ID)
saveRDS(target_labels, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/target_labels_3.rda")

target_labels<-readRDS("../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/target_labels_3.rda")
result$focused_label<-paste(result$NB, result$DA, result$EVO_RATIO, result$GLOBAL_ID)
result_sub<-result%>%dplyr::filter(focused_label %in% target_labels)
saveRDS(result_sub, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/individual_ratio_sub_3.rda")

mean_df_sub<-result_sub %>%
  dplyr::group_by(Y, SUITABLE, NB, DA, EVO_RATIO, EVO_TYPE) %>%
  dplyr::summarize(Mean_N_IND = mean(N_IND, na.rm=TRUE),
                   Mean_N_SP = mean(N_SP, na.rm=TRUE),
                   Mean_N_CELL = mean(N_CELL, na.rm=TRUE),
                   Mean_AVERAGE_N_CELL = mean(AVERAGE_N_CELL, na.rm=TRUE),
                   Mean_N_UNIQUE_CELL = mean(N_UNIQUE_CELL, na.rm=TRUE),
                   SD_N_IND = sd(N_IND),
                   SD_N_SP = sd(N_SP),
                   SD_N_CELL = sd(N_CELL),
                   SD_AVERAGE_N_CELL = sd(AVERAGE_N_CELL),
                   SD_N_UNIQUE_CELL = sd(N_UNIQUE_CELL),
                   Median_N_IND = quantile(N_IND, na.rm=TRUE, .5),
                   Median_N_SP = quantile(N_SP, na.rm=TRUE, .5),
                   Median_N_CELL = quantile(N_CELL, na.rm=TRUE, .5),
                   Median_AVERAGE_N_CELL = quantile(AVERAGE_N_CELL, na.rm=TRUE, .5),
                   Median_N_UNIQUE_CELL = quantile(N_UNIQUE_CELL, na.rm=TRUE, .5),
                   CI_N_IND_LOW = CI(N_IND, ci=0.95)[3],
                   CI_N_IND_MEAN = CI(N_IND, ci=0.95)[2],
                   CI_N_IND_HIGH = CI(N_IND, ci=0.95)[1],
                   CI_N_SP_LOW = CI(N_SP, ci=0.95)[3],
                   CI_N_SP_MEAN = CI(N_SP, ci=0.95)[2],
                   CI_N_SP_HIGH = CI(N_SP, ci=0.95)[1],
                   CI_N_CELL_LOW = CI(N_CELL, ci=0.95)[3],
                   CI_N_CELL_MEAN = CI(N_CELL, ci=0.95)[2],
                   CI_N_CELL_HIGH = CI(N_CELL, ci=0.95)[1],
                   CI_AVERAGE_N_CELL_LOW = CI(AVERAGE_N_CELL, ci=0.95)[3],
                   CI_AVERAGE_N_CELL_MEAN = CI(AVERAGE_N_CELL, ci=0.95)[2],
                   CI_AVERAGE_N_CELL_HIGH = CI(AVERAGE_N_CELL, ci=0.95)[1],
                   CI_N_UNIQUE_CELL_LOW = CI(N_UNIQUE_CELL, ci=0.95)[3],
                   CI_N_UNIQUE_CELL_MEAN = CI(N_UNIQUE_CELL, ci=0.95)[2],
                   CI_N_UNIQUE_CELL_HIGH = CI(N_UNIQUE_CELL, ci=0.95)[1]
  )
mean_df_sub[is.na(mean_df_sub)]<-0
mean_df_sub$label<-paste(mean_df_sub$NB, mean_df_sub$DA, mean_df_sub$EVO_RATIO, mean_df_sub$EVO_TYPE)

saveRDS(mean_df_sub, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/mean_df_sub_3.rda")








result_nb<-readRDS("../../../ees_3d_data/SMART_SPECIES/Tables/individual_ratio_nb.rda")

mean_df_nb<-result_nb %>%
  dplyr::group_by(Y, NB, DA, EVO_RATIO, EVO_TYPE) %>%
  dplyr::summarize(Mean_TEMP_LOW = mean(TEMP_LOW, na.rm=TRUE),
                   Mean_TEMP_HIGH = mean(TEMP_HIGH, na.rm=TRUE),
                   Mean_PREC_LOW = mean(PREC_LOW, na.rm=TRUE),
                   Mean_PREC_HIGH = mean(PREC_HIGH, na.rm=TRUE),
                   Mean_TEMP_RANGE = mean(TEMP_RANGE, na.rm=TRUE),
                   Mean_PREC_RANGE = mean(PREC_RANGE, na.rm=TRUE),
                   SD_TEMP_LOW = sd(TEMP_LOW),
                   SD_TEMP_HIGH = sd(TEMP_HIGH),
                   SD_PREC_LOW = sd(PREC_LOW),
                   SD_PREC_HIGH = sd(PREC_HIGH),
                   SD_TEMP_RANGE = sd(TEMP_RANGE),
                   SD_PREC_RANGE = sd(PREC_RANGE),
                   Median_TEMP_LOW = quantile(TEMP_LOW, na.rm=TRUE, .5),
                   Median_TEMP_HIGH = quantile(TEMP_HIGH, na.rm=TRUE, .5),
                   Median_PREC_LOW = quantile(PREC_LOW, na.rm=TRUE, .5),
                   Median_PREC_HIGH = quantile(PREC_HIGH, na.rm=TRUE, .5),
                   Median_TEMP_RANGE = quantile(TEMP_RANGE, na.rm=TRUE, .5),
                   Median_PREC_RANGE = quantile(PREC_RANGE, na.rm=TRUE, .5),
                   CI_TEMP_LOW_LOW = CI(TEMP_LOW, ci=0.95)[3],
                   CI_TEMP_LOW_MEAN = CI(TEMP_LOW, ci=0.95)[2],
                   CI_TEMP_LOW_HIGH = CI(TEMP_LOW, ci=0.95)[1],
                   CI_TEMP_HIGH_LOW = CI(TEMP_HIGH, ci=0.95)[3],
                   CI_TEMP_HIGH_MEAN = CI(TEMP_HIGH, ci=0.95)[2],
                   CI_TEMP_HIGH_HIGH = CI(TEMP_HIGH, ci=0.95)[1],
                   CI_PREC_LOW_LOW = CI(PREC_LOW, ci=0.95)[3],
                   CI_PREC_LOW_MEAN = CI(PREC_LOW, ci=0.95)[2],
                   CI_PREC_LOW_HIGH = CI(PREC_LOW, ci=0.95)[1],
                   CI_PREC_HIGH_LOW = CI(PREC_HIGH, ci=0.95)[3],
                   CI_PREC_HIGH_MEAN = CI(PREC_HIGH, ci=0.95)[2],
                   CI_PREC_HIGH_HIGH = CI(PREC_HIGH, ci=0.95)[1],
                   CI_TEMP_RANGE_LOW = CI(TEMP_RANGE, ci=0.95)[3],
                   CI_TEMP_RANGE_MEAN = CI(TEMP_RANGE, ci=0.95)[2],
                   CI_TEMP_RANGE_HIGH = CI(TEMP_RANGE, ci=0.95)[1],
                   CI_PREC_RANGE_LOW = CI(PREC_RANGE, ci=0.95)[3],
                   CI_PREC_RANGE_MEAN = CI(PREC_RANGE, ci=0.95)[2],
                   CI_PREC_RANGE_HIGH = CI(PREC_RANGE, ci=0.95)[1]
                   
  )
mean_df_nb[is.na(mean_df_nb)]<-0
mean_df_nb$label<-paste(mean_df_nb$NB, mean_df_nb$DA, mean_df_nb$EVO_RATIO, mean_df_nb$EVO_TYPE)

saveRDS(mean_df_nb, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/mean_df_nb.rda")




result_nb$focused_label<-paste(result_nb$NB, result_nb$DA, result_nb$EVO_RATIO, result_nb$GLOBAL_ID)
result_nb_sub<-result_nb%>%dplyr::filter(focused_label %in% target_labels)
saveRDS(result_nb_sub, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/individual_ratio_nb_sub_3.rda")
unique(result_nb_sub$EVO_TYPE)

mean_df_nb_sub<-result_nb_sub %>%
  dplyr::group_by(Y, NB, DA, EVO_RATIO, EVO_TYPE) %>%
  dplyr::summarize(Mean_TEMP_LOW = mean(TEMP_LOW, na.rm=TRUE),
                   Mean_TEMP_HIGH = mean(TEMP_HIGH, na.rm=TRUE),
                   Mean_PREC_LOW = mean(PREC_LOW, na.rm=TRUE),
                   Mean_PREC_HIGH = mean(PREC_HIGH, na.rm=TRUE),
                   Mean_TEMP_RANGE = mean(TEMP_RANGE, na.rm=TRUE),
                   Mean_PREC_RANGE = mean(PREC_RANGE, na.rm=TRUE),
                   SD_TEMP_LOW = sd(TEMP_LOW),
                   SD_TEMP_HIGH = sd(TEMP_HIGH),
                   SD_PREC_LOW = sd(PREC_LOW),
                   SD_PREC_HIGH = sd(PREC_HIGH),
                   SD_TEMP_RANGE = sd(TEMP_RANGE),
                   SD_PREC_RANGE = sd(PREC_RANGE),
                   Median_TEMP_LOW = quantile(TEMP_LOW, na.rm=TRUE, .5),
                   Median_TEMP_HIGH = quantile(TEMP_HIGH, na.rm=TRUE, .5),
                   Median_PREC_LOW = quantile(PREC_LOW, na.rm=TRUE, .5),
                   Median_PREC_HIGH = quantile(PREC_HIGH, na.rm=TRUE, .5),
                   Median_TEMP_RANGE = quantile(TEMP_RANGE, na.rm=TRUE, .5),
                   Median_PREC_RANGE = quantile(PREC_RANGE, na.rm=TRUE, .5),
                   CI_TEMP_LOW_LOW = CI(TEMP_LOW, ci=0.95)[3],
                   CI_TEMP_LOW_MEAN = CI(TEMP_LOW, ci=0.95)[2],
                   CI_TEMP_LOW_HIGH = CI(TEMP_LOW, ci=0.95)[1],
                   CI_TEMP_HIGH_LOW = CI(TEMP_HIGH, ci=0.95)[3],
                   CI_TEMP_HIGH_MEAN = CI(TEMP_HIGH, ci=0.95)[2],
                   CI_TEMP_HIGH_HIGH = CI(TEMP_HIGH, ci=0.95)[1],
                   CI_PREC_LOW_LOW = CI(PREC_LOW, ci=0.95)[3],
                   CI_PREC_LOW_MEAN = CI(PREC_LOW, ci=0.95)[2],
                   CI_PREC_LOW_HIGH = CI(PREC_LOW, ci=0.95)[1],
                   CI_PREC_HIGH_LOW = CI(PREC_HIGH, ci=0.95)[3],
                   CI_PREC_HIGH_MEAN = CI(PREC_HIGH, ci=0.95)[2],
                   CI_PREC_HIGH_HIGH = CI(PREC_HIGH, ci=0.95)[1],
                   CI_TEMP_RANGE_LOW = CI(TEMP_RANGE, ci=0.95)[3],
                   CI_TEMP_RANGE_MEAN = CI(TEMP_RANGE, ci=0.95)[2],
                   CI_TEMP_RANGE_HIGH = CI(TEMP_RANGE, ci=0.95)[1],
                   CI_PREC_RANGE_LOW = CI(PREC_RANGE, ci=0.95)[3],
                   CI_PREC_RANGE_MEAN = CI(PREC_RANGE, ci=0.95)[2],
                   CI_PREC_RANGE_HIGH = CI(PREC_RANGE, ci=0.95)[1]
                   
  )
mean_df_nb_sub[is.na(mean_df_nb_sub)]<-0
mean_df_nb_sub$label<-paste(mean_df_nb_sub$NB, mean_df_nb_sub$DA, mean_df_nb_sub$EVO_RATIO, mean_df_nb_sub$EVO_TYPE)

saveRDS(mean_df_nb_sub, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/mean_df_nb_sub.rda")




result_nb_sub_2<-result_nb%>%dplyr::filter(!(focused_label %in% target_labels))
unique(result_nb_sub_2$EVO_TYPE)
saveRDS(result_nb_sub_2, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/individual_ratio_nb_sub_no_3.rda")

mean_df_nb_sub_2<-result_nb_sub_2 %>%
  dplyr::group_by(Y, NB, DA, EVO_RATIO, EVO_TYPE) %>%
  dplyr::summarize(Mean_TEMP_LOW = mean(TEMP_LOW, na.rm=TRUE),
                   Mean_TEMP_HIGH = mean(TEMP_HIGH, na.rm=TRUE),
                   Mean_PREC_LOW = mean(PREC_LOW, na.rm=TRUE),
                   Mean_PREC_HIGH = mean(PREC_HIGH, na.rm=TRUE),
                   Mean_TEMP_RANGE = mean(TEMP_RANGE, na.rm=TRUE),
                   Mean_PREC_RANGE = mean(PREC_RANGE, na.rm=TRUE),
                   SD_TEMP_LOW = sd(TEMP_LOW),
                   SD_TEMP_HIGH = sd(TEMP_HIGH),
                   SD_PREC_LOW = sd(PREC_LOW),
                   SD_PREC_HIGH = sd(PREC_HIGH),
                   SD_TEMP_RANGE = sd(TEMP_RANGE),
                   SD_PREC_RANGE = sd(PREC_RANGE),
                   Median_TEMP_LOW = quantile(TEMP_LOW, na.rm=TRUE, .5),
                   Median_TEMP_HIGH = quantile(TEMP_HIGH, na.rm=TRUE, .5),
                   Median_PREC_LOW = quantile(PREC_LOW, na.rm=TRUE, .5),
                   Median_PREC_HIGH = quantile(PREC_HIGH, na.rm=TRUE, .5),
                   Median_TEMP_RANGE = quantile(TEMP_RANGE, na.rm=TRUE, .5),
                   Median_PREC_RANGE = quantile(PREC_RANGE, na.rm=TRUE, .5),
                   CI_TEMP_LOW_LOW = CI(TEMP_LOW, ci=0.95)[3],
                   CI_TEMP_LOW_MEAN = CI(TEMP_LOW, ci=0.95)[2],
                   CI_TEMP_LOW_HIGH = CI(TEMP_LOW, ci=0.95)[1],
                   CI_TEMP_HIGH_LOW = CI(TEMP_HIGH, ci=0.95)[3],
                   CI_TEMP_HIGH_MEAN = CI(TEMP_HIGH, ci=0.95)[2],
                   CI_TEMP_HIGH_HIGH = CI(TEMP_HIGH, ci=0.95)[1],
                   CI_PREC_LOW_LOW = CI(PREC_LOW, ci=0.95)[3],
                   CI_PREC_LOW_MEAN = CI(PREC_LOW, ci=0.95)[2],
                   CI_PREC_LOW_HIGH = CI(PREC_LOW, ci=0.95)[1],
                   CI_PREC_HIGH_LOW = CI(PREC_HIGH, ci=0.95)[3],
                   CI_PREC_HIGH_MEAN = CI(PREC_HIGH, ci=0.95)[2],
                   CI_PREC_HIGH_HIGH = CI(PREC_HIGH, ci=0.95)[1],
                   CI_TEMP_RANGE_LOW = CI(TEMP_RANGE, ci=0.95)[3],
                   CI_TEMP_RANGE_MEAN = CI(TEMP_RANGE, ci=0.95)[2],
                   CI_TEMP_RANGE_HIGH = CI(TEMP_RANGE, ci=0.95)[1],
                   CI_PREC_RANGE_LOW = CI(PREC_RANGE, ci=0.95)[3],
                   CI_PREC_RANGE_MEAN = CI(PREC_RANGE, ci=0.95)[2],
                   CI_PREC_RANGE_HIGH = CI(PREC_RANGE, ci=0.95)[1]
                   
  )
mean_df_nb_sub_2[is.na(mean_df_nb_sub_2)]<-0
mean_df_nb_sub_2$label<-paste(mean_df_nb_sub_2$NB, mean_df_nb_sub_2$DA, 
                              mean_df_nb_sub_2$EVO_RATIO, mean_df_nb_sub_2$EVO_TYPE)

saveRDS(mean_df_nb_sub_2, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/mean_df_nb_sub_no_3.rda")








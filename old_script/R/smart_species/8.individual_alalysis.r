library(dplyr)
library(stringr)

target<-"17956_POOR_NARROW"
evo_type<-c(1:7)
base<-"/home/huijieqiao/git/ees_3d_data/SMART_SPECIES"
result<-readRDS(sprintf("%s/Tables/individual_ratio.rda", base))
result$Y<-result$Y*-1
head(result)
result$AVERAGE_N_CELL<-result$N_CELL/result$N_SP
item<-result %>% filter(str_detect(LABLE, sprintf("^%s", target)))


mean_df<-item %>%
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
library(rgdal)

p<-ggplot(mean_df %>% filter(SUITABLE==1), 
          aes(x=Y, y=Mean_N_SP))+
  geom_line(aes(color=factor(label)))+
  geom_ribbon(aes(ymin=Mean_N_SP-CI_N_SP, ymax=Mean_N_SP+CI_N_SP, fill=factor(label)), color=NA, alpha=0.3)+
  theme_bw()+
  ggtitle(paste(com$Var1, com$Var2))+
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"), guide = 'none')+
  geom_text(data = mean_df %>% filter((Y==0)&(SUITABLE==1)), aes(label = label, colour = label, x = 0, y = Mean_N_SP), hjust = -.1)+
  xlim(c(-1200, 350))
#ggsave(p, file=sprintf("%s/Figures/N_Species/%s_%s_ALL.png", base, com$Var1, com$Var2))

p<-ggplot(mean_df %>% filter(SUITABLE==1), 
          aes(x=Y, y=Mean_N_CELL))+
  geom_line(aes(color=factor(label)))+
  #geom_ribbon(aes(ymin=Mean_N_CELL-CI_N_CELL, ymax=Mean_N_CELL+CI_N_CELL, fill=factor(label)), color=NA, alpha=0.3)+
  theme_bw()+
  ggtitle(paste(com$Var1, com$Var2))+
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"), guide = 'none')+
  geom_text(data = mean_df %>% filter((Y==0)&(SUITABLE==1)), aes(label = label, colour = label, x = 0, y = Mean_N_CELL), hjust = -.1)+
  xlim(c(-1200, 350))

ggplot(mean_df %>% filter(SUITABLE==1), 
       aes(x=Y, y=Mean_AVERAGE_N_CELL))+
  geom_line(aes(color=factor(label)))+
  #geom_ribbon(aes(ymin=Mean_AVERAGE_N_CELL-CI_AVERAGE_N_CELL, ymax=Mean_AVERAGE_N_CELL+CI_AVERAGE_N_CELL, fill=factor(label)), color=NA, alpha=0.3)+
  theme_bw()+
  ggtitle(paste(com$Var1, com$Var2))+
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"), guide = 'none')+
  geom_text(data = mean_df %>% filter((Y==0)&(SUITABLE==1)), aes(label = label, colour = label, x = 0, y = Mean_AVERAGE_N_CELL), hjust = -.1)+
  xlim(c(-1200, 350))
#ggsave(p, file=sprintf("%s/Figures/N_Cells/%s_%s_ALL.png", base, com$Var1, com$Var2))


mean_df_ratio<-item %>%
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

ggplot(mean_df_ratio_df, aes(x=Y, y=Mean_ratio))+
  geom_line(aes(color=factor(label)))+
  geom_ribbon(aes(ymin=Mean_ratio-CI_ratio, ymax=Mean_ratio+CI_ratio, fill=factor(label)), color=NA, alpha=0.3)+
  theme_bw()+
  ggtitle(paste(com$Var1, com$Var2))+
  #scale_colour_manual(values = cols,aesthetics = c("colour", "fill"), guide = 'none')+
  scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+
  geom_text(data = mean_df_ratio_df %>% filter((Y==0)), aes(label = label, colour = label, x = 0, y = Mean_ratio), hjust = -.1)+
  xlim(c(-1200, 350))+ylim(c(0, 0.2))




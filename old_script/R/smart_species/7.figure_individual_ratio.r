library(dplyr)
library(ggplot2)
library(Rmisc)
base<-"~/Downloads"
#base<-"C:/Users/Huijie Qiao/Downloads"
base<-"/home/huijieqiao/git/ees_3d_data/SMART_SPECIES"

#USELESS NOW

result<-readRDS(sprintf("%s/Tables/individual_ratio.rda", base))
result$Y<-result$Y*-1
head(result)

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

saveRDS(mean_df, "../../../ees_3d_data/SMART_SPECIES/Tables/9.null_model_comparison_analysis/mean_df.rda")

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

comb<-expand.grid(unique(mean_df$NB), unique(mean_df$DA), stringsAsFactors=F)
cols<-rep(c("red", "red4", "black", 
            "orange", "orange4", "black", 
            "yellow", "yellow4", "black", 
            "green", "green4", "black",
            "turquoise", "turquoise4", "black",
            "blue", "blue4", "black",
            "black", "black", "black"), nrow(comb))
comb<-expand.grid(unique(mean_df$EVO_RATIO), unique(mean_df$EVO_TYPE), unique(mean_df$NB), unique(mean_df$DA), stringsAsFactors=F)
comb$label<-paste(comb$Var3, comb$Var4, comb$Var1, comb$Var2)
names(cols)<-comb$label

comb<-expand.grid(unique(mean_df$NB), unique(mean_df$DA), stringsAsFactors=F)
i=1
for (i in c(1:nrow(comb))){
  com<-comb[i,]
  mean_df_item<-mean_df %>% filter(NB==com$Var1 & DA==com$Var2)
  mean_df_ratio_df_item<-mean_df_ratio_df %>% filter(NB==com$Var1 & DA==com$Var2)
  N_Sim_item<-N_Sim %>% filter(NB==com$Var1 & DA==com$Var2)
  max_N<-N_Sim_item[which(N_Sim_item$Y==-1199), ]
  N_Sim_item<-left_join(N_Sim_item, max_N[, c("NB", "DA", "EVO_RATIO", "EVO_TYPE", "label", "N")], by=c("NB", "DA", "EVO_RATIO", "EVO_TYPE", "label"))
  N_Sim_item$N_AVE<-N_Sim_item$N.x/N_Sim_item$N.y
  p<-ggplot(N_Sim_item, 
            aes(x=Y, y= N_AVE, color=factor(label), fill=factor(label)))+
    geom_line()+
    theme_bw()+
    ggtitle(paste(com$Var1, com$Var2))+
    scale_colour_manual(values = cols, aesthetics = c("colour", "fill"), guide = 'none')+
    geom_text(data = subset(N_Sim_item, Y == 0), aes(label = label, colour = label, x = 0, y = N_AVE), hjust = -.1)+
    xlim(c(-1200, 350))
  
  ggsave(p, file=sprintf("%s/Figures/N_Sim/%s_%s_ALL.png", base, com$Var1, com$Var2))
  
  p<-ggplot(N_Sim_item %>% filter((Y!=-1199)&(EVO_TYPE!=3)), 
            aes(x=Y, y= N_AVE, color=factor(label), fill=factor(label)))+
    geom_line()+
    theme_bw()+
    ggtitle(paste(com$Var1, com$Var2))+
    scale_colour_manual(values = cols,aesthetics = c("colour", "fill"), guide = 'none')+
    geom_text(data = N_Sim_item %>% filter((Y==0)&(EVO_TYPE!=3)), aes(label = label, colour = label, x = 0, y = N_AVE), hjust = -.1)+
    ylim(c(0.52, 0.6))+
    xlim(c(-1200, 400))
  ggsave(p, file=sprintf("%s/Figures/N_Sim/%s_%s_PART.png", base, com$Var1, com$Var2))
  
  p<-ggplot(mean_df_item %>% filter(SUITABLE==1), 
            aes(x=Y, y=Mean_N_SP))+
    geom_line(aes(color=factor(label)))+
    geom_ribbon(aes(ymin=Mean_N_SP-CI_N_SP, ymax=Mean_N_SP+CI_N_SP, fill=factor(label)), color=NA, alpha=0.3)+
    theme_bw()+
    ggtitle(paste(com$Var1, com$Var2))+
    scale_colour_manual(values = cols,aesthetics = c("colour", "fill"), guide = 'none')+
    geom_text(data = mean_df_item %>% filter((Y==0)&(SUITABLE==1)), aes(label = label, colour = label, x = 0, y = Mean_N_SP), hjust = -.1)+
    xlim(c(-1200, 350))
  ggsave(p, file=sprintf("%s/Figures/N_Species/%s_%s_ALL.png", base, com$Var1, com$Var2))
  
  p<-ggplot(mean_df_item %>% filter(SUITABLE==1), 
            aes(x=Y, y=Mean_N_CELL))+
    geom_line(aes(color=factor(label)))+
    #geom_ribbon(aes(ymin=Mean_N_CELL-CI_N_CELL, ymax=Mean_N_CELL+CI_N_CELL, fill=factor(label)), color=NA, alpha=0.3)+
    theme_bw()+
    ggtitle(paste(com$Var1, com$Var2))+
    scale_colour_manual(values = cols,aesthetics = c("colour", "fill"), guide = 'none')+
    geom_text(data = mean_df_item %>% filter((Y==0)&(SUITABLE==1)), aes(label = label, colour = label, x = 0, y = Mean_N_CELL), hjust = -.1)+
    xlim(c(-1200, 350))
  ggsave(p, file=sprintf("%s/Figures/N_Cells/%s_%s_ALL.png", base, com$Var1, com$Var2))
  
  p<-ggplot(mean_df_item %>% filter((SUITABLE==1)&(Y<=-1100)), 
            aes(x=Y, y=Mean_N_CELL))+
    geom_line(aes(color=factor(label)))+
    #geom_ribbon(aes(ymin=Mean_N_CELL-CI_N_CELL, ymax=Mean_N_CELL+CI_N_CELL, fill=factor(label)), color=NA, alpha=0.3)+
    theme_bw()+
    ggtitle(paste(com$Var1, com$Var2))+
    scale_colour_manual(values = cols,aesthetics = c("colour", "fill"), guide = 'none')+
    geom_text(data = mean_df_item %>% filter((Y==-1100)&(SUITABLE==1)), aes(label = label, colour = label, x = -1100, y = Mean_N_CELL), hjust = -.1)+
    xlim(c(-1200, -1050))
  ggsave(p, file=sprintf("%s/Figures/N_Cells/%s_%s_1100.png", base, com$Var1, com$Var2))
  
  p<-ggplot(mean_df_item %>% filter(SUITABLE==1), 
            aes(x=Y, y=Mean_AVERAGE_N_CELL))+
    geom_line(aes(color=factor(label)))+
    #geom_ribbon(aes(ymin=Mean_AVERAGE_N_CELL-CI_AVERAGE_N_CELL, ymax=Mean_AVERAGE_N_CELL+CI_AVERAGE_N_CELL, fill=factor(label)), color=NA, alpha=0.3)+
    theme_bw()+
    ggtitle(paste(com$Var1, com$Var2))+
    scale_colour_manual(values = cols,aesthetics = c("colour", "fill"), guide = 'none')+
    geom_text(data = mean_df_item %>% filter((Y==0)&(SUITABLE==1)), aes(label = label, colour = label, x = 0, y = Mean_AVERAGE_N_CELL), hjust = -.1)+
    xlim(c(-1200, 350))
  
 
  ggsave(p, file=sprintf("%s/Figures/N_CELLs/%s_%s_AVERAGE_.png", base, com$Var1, com$Var2))
  
  ggplot(mean_df_ratio_df_item %>% filter(EVO_TYPE!=3), aes(x=Y, y=Mean_ratio))+
    geom_line(aes(color=factor(label)))+
    #geom_ribbon(aes(ymin=Mean_ratio-CI_ratio, ymax=Mean_ratio+CI_ratio, fill=factor(label)), color=NA, alpha=0.3)+
    theme_bw()+
    ggtitle(paste(com$Var1, com$Var2))+
    scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))
  
  p<-ggplot(mean_df_ratio_df_item, aes(x=Y, y=Mean_ratio))+
    geom_line(aes(color=factor(label)))+
    geom_ribbon(aes(ymin=Mean_ratio-CI_ratio, ymax=Mean_ratio+CI_ratio, fill=factor(label)), color=NA, alpha=0.3)+
    theme_bw()+
    ggtitle(paste(com$Var1, com$Var2))+
    scale_colour_manual(values = cols,aesthetics = c("colour", "fill"), guide = 'none')+
    geom_text(data = mean_df_ratio_df_item %>% filter((Y==0)), aes(label = label, colour = label, x = 0, y = Mean_ratio), hjust = -.1)+
    xlim(c(-1200, 350))
  
  p <- p + ylim(0, 1)
  
  ggsave(p, file=sprintf("%s/Figures/Trial_Error/%s_%s_ALL.png", base, com$Var1, com$Var2))
  
  p<-ggplot(mean_df_ratio_df_item %>% filter((Y<=-1100)&(Y>=-1198)&(EVO_TYPE!=3)), aes(x=Y, y=Mean_ratio))+
    geom_line(aes(color=factor(label)))+
    #geom_ribbon(aes(ymin=Mean_ratio-CI_ratio, ymax=Mean_ratio+CI_ratio, fill=factor(label)), color=NA, alpha=0.3)+
    theme_bw()+
    ggtitle(paste(com$Var1, com$Var2))+
    scale_colour_manual(values = cols,aesthetics = c("colour", "fill"), guide = 'none')+
    geom_text(data = mean_df_ratio_df_item %>% filter((Y==-1100)&(EVO_TYPE!=3)), aes(label = label, colour = label, x = -1100, y = Mean_ratio), hjust = -.1)+
    xlim(c(-1198, -1050))+ylim(0.15, 0.23)
  
  ggplot(mean_df_ratio_df_item %>% filter((EVO_TYPE!=3)&(EVO_RATIO!=0.8)), aes(x=Y, y=Mean_ratio))+
    geom_line(aes(color=factor(label)))+
    #geom_ribbon(aes(ymin=Mean_ratio-CI_ratio, ymax=Mean_ratio+CI_ratio, fill=factor(label)), color=NA, alpha=0.3)+
    theme_bw()+
    ggtitle(paste(com$Var1, com$Var2))+
    scale_colour_manual(values = cols,aesthetics = c("colour", "fill"), guide = 'none')+
    geom_text(data = mean_df_ratio_df_item %>% filter((Y==0)&(EVO_TYPE!=3)&(EVO_RATIO!=0.8)), aes(label = label, colour = label, x = 0, y = Mean_ratio), hjust = -.1)+
    xlim(c(-1198, 350))+ylim(0.1, 0.23)
  
  ggsave(p, file=sprintf("%s/Figures/Trial_Error/%s_%s_1100.png", base, com$Var1, com$Var2))
  
}

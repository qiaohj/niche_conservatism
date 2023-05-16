library(data.table)
library(ggplot2)
library(ggpubr)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
#this is useless, use the code after the 'if' section.
if (F){
  df_full_outliers_se_nb_da<-readRDS("../Data/niche_traits/niche_delta_nb_da.rda")
  df_full_outliers_se_nb<-readRDS("../Data/niche_traits/niche_delta_nb.rda")
  df_full_outliers_se<-readRDS("../Data/niche_traits/niche_delta.rda")
  
  df_full_outliers_se$evo_type<-format_evoType(df_full_outliers_se$species_evo_type)
  df_full_outliers_se$label<-sprintf("%s (%s)", df_full_outliers_se$evo_type, df_full_outliers_se$directional_speed)
  
  df_full_outliers_se$var2<-df_full_outliers_se$var
  df_full_outliers_se[var=="Debiased_Maximum_Monthly_Precipitation"]$var2<-"prcp"
  df_full_outliers_se[var=="Debiased_Minimum_Monthly_Temperature"]$var2<-"temp"
  df_full_outliers_se<-df_full_outliers_se[var2 %in% c("prcp", "temp")]
  p1<-ggplot(df_full_outliers_se[directional_speed==0.1], aes(x=(year-1200) * 0.1))+
    geom_line(aes(color=var2, y=delta_lower_limit))+
    geom_line(aes(color=var2, y=delta_upper_limit))+
    facet_wrap(~label,  nrow=1, ncol=3)+
    theme_bw()+
    theme(axis.title = element_blank())
  p1
  p2<-ggplot(df_full_outliers_se[directional_speed==0.5], aes(x=(year-1200) * 0.1))+
    geom_line(aes(color=var2, y=delta_lower_limit))+
    geom_line(aes(color=var2, y=delta_upper_limit))+
    facet_wrap(~label,  nrow=1, ncol=3)+
    theme_bw()+
    theme(axis.title = element_blank())
  
  p2
  df_full_outliers_se[directional_speed==0.5 & species_evo_type==2]
  p3<-ggplot(df_full_outliers_se[directional_speed==0.01], aes(x=(year-1200) * 0.1))+
    geom_line(aes(color=var2, y=delta_lower_limit))+
    geom_line(aes(color=var2, y=delta_upper_limit))+
    facet_wrap(~label,  nrow=1, ncol=3)+
    theme_bw()+
    theme(axis.title.y = element_blank())
  p3
  p<-ggarrange(p1, p2, p3, ncol=1)  
  p
  ggsave(p, filename="../Figures/Niche_Traits/niche_delta_year.png", width=12, height=6)
  
  p<-ggplot(df_full_outliers_se, aes(x=(year-1200) * 0.1))+
    geom_line(aes(color=var2, y=delta_centra))+
    facet_wrap(~label, scale="free", nrow=3, ncol=3)+
    theme_bw()
  
  p  
  ggsave(p, filename="../Figures/Niche_Traits/niche_delta_year.png", width=12, height=6)
  df_full_outliers_se$range<-df_full_outliers_se$delta_upper_limit - df_full_outliers_se$delta_lower_limit
  p1<-ggplot(df_full_outliers_se)+geom_boxplot(aes(x=label, y=range, color=var2))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  p1
  p2<-ggplot(df_full_outliers_se)+geom_boxplot(aes(x=label, y=delta_centra, color=var2))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  p2
  
  p<-ggarrange(p1, p2, nrow=1, ncol=2)
  p
  ggsave(p, filename="../Figures/Niche_Traits/niche_delta_full.png", width=12, height=6)
}
df_niche_delta<-df_full_outliers_se[, .(delta_breadth=mean(range),
                                        delta_centra=mean(delta_centra)),
                                    by=list(evo_type, label, var2,species_evo_type, directional_speed)]

df_all_se_nb_da<-readRDS("../Data/niche_traits/niche_traits_fn_se_nb_da_without_outlier_with_next_year.rda")
df_all_se_nb<-readRDS("../Data/niche_traits/niche_traits_fn_se_nb_without_outlier_with_next_year.rda")
df_all_se<-readRDS("../Data/niche_traits/niche_traits_fn_se_without_outlier_with_next_year.rda")
df_all_se$evo_type<-format_evoType(df_all_se$species_evo_type)
df_all_se[, label:=format_evoLabel(evo_type, directional_speed), by = seq_len(nrow(df_all_se))]

df_all_se_gg<-df_all_se[((directional_speed %in% c(0) & species_evo_type==1) |
                           (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                           (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7))) &
                          V_L %in% c("Debiased_Maximum_Monthly_Precipitation", "Debiased_Maximum_Monthly_Temperature")]
df_all_se_gg[V_L=="Debiased_Maximum_Monthly_Precipitation"]$V_L<-"Precipitation"
df_all_se_gg[V_L=="Debiased_Maximum_Monthly_Temperature"]$V_L<-"Temperature"

ggplot(df_all_se_gg[!is.nan(V_range_next_delta_ratio)])+
  geom_line(aes(x=(year-1200) * 0.1, y=nb_delta_ratio, color=label))+
  facet_wrap(~V_L, nrow=2, scale="free")+theme_bw()

p1<-ggplot(df_all_se_gg[directional_speed==0.1], aes(x=(year-1200) * 0.1))+
  geom_line(aes(color=V_L, y=nb_delta_ratio))+
  facet_wrap(~label,  nrow=1, ncol=3)+
  theme_bw()+
  theme(axis.title = element_blank())
p1
p2<-ggplot(df_all_se_gg[directional_speed==0.5], aes(x=(year-1200) * 0.1))+
  geom_line(aes(color=V_L, y=nb_delta_ratio))+
  facet_wrap(~label,  nrow=1, ncol=3)+
  theme_bw()+
  theme(axis.title = element_blank())

p2
df_all_se_gg[directional_speed==0.5 & species_evo_type==2]
p3<-ggplot(df_all_se_gg[directional_speed==0.01], aes(x=(year-1200) * 0.1))+
  geom_line(aes(color=V_L, y=nb_delta_ratio))+
  facet_wrap(~label,  nrow=1, ncol=3)+
  theme_bw()+
  theme(axis.title.y = element_blank())
p3
p<-ggarrange(p1, p2, p3, ncol=1)  
p
ggsave(p, filename="../Figures/Niche_Traits/niche_ratio_year.png", width=12, height=6)


p1<-ggplot(df_all_se_gg[directional_speed==0.1], aes(x=(year-1200) * 0.1))+
  geom_line(aes(color=V_L, y=V_range_next_delta))+
  facet_wrap(~label,  nrow=1, ncol=3)+
  theme_bw()+
  theme(axis.title = element_blank())
p1
p2<-ggplot(df_all_se_gg[directional_speed==0.5], aes(x=(year-1200) * 0.1))+
  geom_line(aes(color=V_L, y=V_range_next_delta))+
  facet_wrap(~label,  nrow=1, ncol=3)+
  theme_bw()+
  theme(axis.title = element_blank())

p2
df_all_se_gg[directional_speed==0.5 & species_evo_type==2]
p3<-ggplot(df_all_se_gg[directional_speed==0.01], aes(x=(year-1200) * 0.1))+
  geom_line(aes(color=V_L, y=V_range_next_delta))+
  facet_wrap(~label,  nrow=1, ncol=3)+
  theme_bw()+
  theme(axis.title.y = element_blank())
p3
p<-ggarrange(p1, p2, p3, ncol=1)  
p
ggsave(p, filename="../Figures/Niche_Traits/niche_delta_next_year.png", width=12, height=6)


p1<-ggplot(df_all_se_gg[directional_speed==0.1], aes(x=(year-1200) * 0.1))+
  geom_line(aes(color=V_L, y=V_range_next_delta_ratio))+
  facet_wrap(~label,  nrow=1, ncol=3)+
  theme_bw()+
  theme(axis.title = element_blank())
p1
p2<-ggplot(df_all_se_gg[directional_speed==0.5], aes(x=(year-1200) * 0.1))+
  geom_line(aes(color=V_L, y=V_range_next_delta_ratio))+
  facet_wrap(~label,  nrow=1, ncol=3)+
  theme_bw()+
  theme(axis.title = element_blank())

p2
df_all_se_gg[directional_speed==0.5 & species_evo_type==2]
p3<-ggplot(df_all_se_gg[directional_speed==0.01], aes(x=(year-1200) * 0.1))+
  geom_line(aes(color=V_L, y=V_range_next_delta_ratio))+
  facet_wrap(~label,  nrow=1, ncol=3)+
  theme_bw()+
  theme(axis.title.y = element_blank())
p3
p<-ggarrange(p1, p2, p3, ncol=1)  
p
ggsave(p, filename="../Figures/Niche_Traits/niche_delta_ratop_next_year.png", width=12, height=6)

p1<-ggplot(df_all_se_gg[directional_speed==0.1], aes(x=(year-1200) * 0.1))+
  geom_line(aes(color=V_L, y=nb_delta))+
  facet_wrap(~label,  nrow=1, ncol=3)+
  theme_bw()+
  theme(axis.title = element_blank())
p1
p2<-ggplot(df_all_se_gg[directional_speed==0.5], aes(x=(year-1200) * 0.1))+
  geom_line(aes(color=V_L, y=nb_delta))+
  facet_wrap(~label,  nrow=1, ncol=3)+
  theme_bw()+
  theme(axis.title = element_blank())

p2
df_all_se_gg[directional_speed==0.5 & species_evo_type==2]
p3<-ggplot(df_all_se_gg[directional_speed==0.01], aes(x=(year-1200) * 0.1))+
  geom_line(aes(color=V_L, y=nb_delta))+
  facet_wrap(~label,  nrow=1, ncol=3)+
  theme_bw()+
  theme(axis.title.y = element_blank())
p3
p<-ggarrange(p1, p2, p3, ncol=1)  
p
ggsave(p, filename="../Figures/Niche_Traits/niche_delta_year.png", width=12, height=6)

p1<-ggplot(df_all_se_gg[directional_speed==0.1], aes(x=(year-1200) * 0.1))+
  geom_line(aes(color=V_L, y=V_min_next_delta))+
  geom_line(aes(color=V_L, y=V_max_next_delta))+
  facet_wrap(~label,  nrow=1, ncol=3)+
  theme_bw()+
  theme(axis.title = element_blank())
p1
p2<-ggplot(df_all_se_gg[directional_speed==0.5], aes(x=(year-1200) * 0.1))+
  geom_line(aes(color=V_L, y=V_min_next_delta))+
  geom_line(aes(color=V_L, y=V_max_next_delta))+
  facet_wrap(~label,  nrow=1, ncol=3)+
  theme_bw()+
  theme(axis.title = element_blank())

p2
df_all_se_gg[directional_speed==0.5 & species_evo_type==2]
p3<-ggplot(df_all_se_gg[directional_speed==0.01], aes(x=(year-1200) * 0.1))+
  geom_line(aes(color=V_L, y=V_min_next_delta))+
  geom_line(aes(color=V_L, y=V_max_next_delta))+
  facet_wrap(~label,  nrow=1, ncol=3)+
  theme_bw()+
  theme(axis.title.y = element_blank())
p3
p<-ggarrange(p1, p2, p3, ncol=1)  
p
ggsave(p, filename="../Figures/Niche_Traits/niche_delta_limit_year.png", width=12, height=6)


df_all_last_year<-readRDS("../Data/niche_traits/niche_traits_fn_without_outlier_last_year.rda")
df_all_last_year$evo_type<-format_evoType(df_all_last_year$species_evo_type)
df_all_last_year[, label:=format_evoLabel(evo_type, directional_speed), 
                 by = seq_len(nrow(df_all_last_year))]

df_all_last_year_gg<-df_all_last_year[((directional_speed %in% c(0) & species_evo_type==1) |
                                         (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                                         (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7))) &
                                        V_L %in% c("Debiased_Maximum_Monthly_Precipitation", "Debiased_Minimum_Monthly_Temperature")]
df_all_last_year_gg[V_L=="Debiased_Maximum_Monthly_Precipitation"]$V_L<-"Precipitation"
df_all_last_year_gg[V_L=="Debiased_Maximum_Monthly_Temperature"]$V_L<-"Temperature"

p1<-ggplot(df_all_last_year_gg)+geom_boxplot(aes(x=label, y=nb_delta_ratio, color=V_L))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_log10()
p1
p2<-ggplot(df_all_last_year_gg)+geom_boxplot(aes(x=label, y=V_mean_delta, color=V_L))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_log10()
p2

p<-ggarrange(p1, p2, nrow=1, ncol=2)
p
ggsave(p1, filename="../Figures/Niche_Traits/niche_delta_full.png", width=12, height=6)

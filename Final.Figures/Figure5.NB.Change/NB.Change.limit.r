library(data.table)
library(ggplot2)
library(ggpubr)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
df_full_outliers_se<-readRDS("../Data/niche_traits/niche_delta.rda")

df_full_outliers_se$evo_type<-format_evoType(df_full_outliers_se$species_evo_type)
df_full_outliers_se$label<-sprintf("%s (%s)", df_full_outliers_se$evo_type, df_full_outliers_se$directional_speed)

df_full_outliers_se$var2<-df_full_outliers_se$var
df_full_outliers_se[var=="Debiased_Maximum_Monthly_Precipitation"]$var2<-"prcp"
df_full_outliers_se[var=="Debiased_Minimum_Monthly_Temperature"]$var2<-"temp"
df_full_outliers_se<-df_full_outliers_se[var2 %in% c("prcp", "temp")]
df_full_outliers_se$range<-df_full_outliers_se$delta_upper_limit - df_full_outliers_se$delta_lower_limit
df_niche_delta<-df_full_outliers_se[, .(delta_breadth=mean(range),
                                        delta_centra=mean(delta_centra)),
                                    by=list(evo_type, label, var2, species_evo_type, directional_speed)]

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
df_all_se_gg$V_L<-factor(df_all_se_gg$V_L, levels=c("Temperature", "Precipitation"))
df_all_se_gg<-formatLabels(df_all_se_gg)

item<-df_all_se_gg[directional_speed==0.1]
p1<-ggplot(item, aes(x=(year-1200) * 0.1))+
  geom_line(aes(color=V_L, y=V_min_next_delta))+
  geom_line(aes(color=V_L, y=V_max_next_delta))+
  facet_wrap(~label_line,  nrow=1, ncol=3)+
  scale_color_manual(values=c("#4477AA", "#EE6677"), breaks=c("Precipitation", "Temperature"))+
  labs(x=x_label, color="Environmental variable")+
  theme_bw()+
  theme(axis.title = element_blank())
p1

item<-df_all_se_gg[directional_speed==0.5]
p2<-ggplot(item, aes(x=(year-1200) * 0.1))+
  geom_line(aes(color=V_L, y=V_min_next_delta))+
  geom_line(aes(color=V_L, y=V_max_next_delta))+
  facet_wrap(~label_line,  nrow=1, ncol=3)+
  scale_color_manual(values=c("#4477AA", "#EE6677"), breaks=c("Precipitation", "Temperature"))+
  labs(x=x_label, color="Environmental variable")+
  theme_bw()+
  theme(axis.title = element_blank())

p2
item<-df_all_se_gg[directional_speed==0.01]
p3<-ggplot(item, aes(x=(year-1200) * 0.1))+
  geom_line(aes(color=V_L, y=V_min_next_delta))+
  geom_line(aes(color=V_L, y=V_max_next_delta))+
  facet_wrap(~label_line,  nrow=1, ncol=3)+
  scale_color_manual(values=c("#4477AA", "#EE6677"), breaks=c("Precipitation", "Temperature"))+
  labs(x=x_label, color="Environmental variable")+
  theme_bw()+
  theme(axis.title.y = element_blank())
p3
p<-ggarrange(p1, p2, p3, ncol=1, common.legend = T, legend = "bottom")  
p
ggsave(p, filename="../Figures/Figure5.NB.Change/niche_delta_limit_year.png", width=10, height=8, bg="white")
ggsave(p, filename="../Figures/Figure5.NB.Change/niche_delta_limit_year.pdf", width=10, height=8, bg="white")

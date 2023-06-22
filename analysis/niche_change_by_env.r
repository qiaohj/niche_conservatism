library(ggplot2)
library(data.table)
library(RSQLite)
library(DBI)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
df_all1<-readRDS("../Data/niche_traits/niche_traits_fn_without_outlier_last_year.rda")

temp<-df_all1[V_L=="Debiased_Maximum_Monthly_Temperature"]
prec<-df_all1[V_L=="Debiased_Maximum_Monthly_Precipitation"]

colnames(temp)
cols<-c("global_id", "nb", "da", "SP_ID", "species_evo_type", "directional_speed", "V_range")
temp<-temp[, ..cols]
colnames(temp)[7]<-"V_range_temp"

prec<-prec[, ..cols]
colnames(prec)[7]<-"V_range_prec"

env_var<-merge(temp, prec, by=c("global_id", "nb", "da", "SP_ID", "species_evo_type", "directional_speed"))

ggplot(env_var)+geom_point(aes(x=V_range_temp, y=V_range_prec))+
  facet_grid(species_evo_type~directional_speed)

env_var$evo_type<-format_evoType(env_var$species_evo_type)
env_var[, label:=format_evoLabel(evo_type, directional_speed), 
            by=seq_len(nrow(env_var))]
env_var[label=="shift-directional (0.5)"]
env_var<-env_var[((directional_speed %in% c(0) & species_evo_type==1) |
        (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
        (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)))]

env_var[, .(cor=cor(V_range_temp, V_range_prec)),
        by=c("label")]

env_var$init_range_temp<-60
env_var[nb=="NARROW"]$init_range_temp<-40
env_var$init_range_prec<-10
env_var[nb=="NARROW"]$init_range_prec<-5
env_var$ratio_temp<-env_var$V_range_temp/env_var$init_range_temp
env_var$ratio_prec<-env_var$V_range_prec/env_var$init_range_prec

env_var_se<-env_var[, .(ratio_temp=mean(ratio_temp),
                        sd_ratio_temp=sd(ratio_temp),
                        ratio_prec=mean(ratio_prec),
                        sd_ratio_prec=sd(ratio_prec),
                        V_range_temp=mean(V_range_temp),
                        V_range_prec=mean(V_range_prec),
                        sd_V_range_temp=sd(V_range_temp),
                        sd_V_range_prec=sd(V_range_prec)),
                    by=list(species_evo_type, directional_speed,
                            evo_type, label)]

ggplot(env_var_se)+geom_point(aes(x=label, y=ratio_temp))+
  geom_point(aes(x=label, y=ratio_prec), color="red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

df<-readRDS("../Data/niche_traits/niche_traits_fn_se_without_outliers_3SD.rda")


df$init_range<-60
df[nb=="NARROW" & V_L=="Debiased_Maximum_Monthly_Temperature"]$init_range<-40
df[nb=="NARROW" & V_L=="Debiased_Minimum_Monthly_Temperature"]$init_range<-40

df[nb=="NARROW" & V_L=="Debiased_Maximum_Monthly_Precipitation"]$init_range<-5
df[nb=="NARROW" & V_L=="Debiased_Maximum_Monthly_Precipitation"]$init_range<-5

df[nb=="BROAD" & V_L=="Debiased_Maximum_Monthly_Precipitation"]$init_range<-10
df[nb=="BROAD" & V_L=="Debiased_Maximum_Monthly_Precipitation"]$init_range<-10
df<-df[((directional_speed %in% c(0) & species_evo_type==1) |
                    (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                    (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)))]

df$ratio<-df$V_range/df$init_range
df$evo_type<-format_evoType(df$species_evo_type)
df[, label:=format_evoLabel(evo_type, directional_speed), 
        by=seq_len(nrow(df))]
df$year<-df$year - 1200

df_se<-df[, .(ratio=mean(ratio),
              sd_ratio=sd(ratio)),
          by=list(V_L, year, label)]
df_se<-df_se[V_L %in% c("Debiased_Maximum_Monthly_Temperature", "Debiased_Maximum_Monthly_Precipitation")]
df_se$VAR_L<-"Temperature"
df_se[V_L=="Debiased_Maximum_Monthly_Precipitation"]$VAR_L<-"Precipitation"
ggplot(df_se)+
  geom_ribbon(aes(x=year, ymin=ratio-sd_ratio, ymax=ratio+sd_ratio, fill=label), alpha=0.3)+
  geom_line(aes(x=year, y=ratio, color=label))+
  facet_wrap(~VAR_L, nrow=2, scale="free")

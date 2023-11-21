library(data.table)
library(ggplot2)
library(ggpubr)
library(rempsyc)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
if (F){
  df_full_outliers<-readRDS("../Data/niche_traits/niche_delta_raw.rda")
  df_full_outliers$delta_lower_limit_abs<-abs(df_full_outliers$delta_lower_limit)
  df_full_outliers$delta_upper_limit_abs<-abs(df_full_outliers$delta_upper_limit)
  df_full_outliers<-df_full_outliers[year>=100]
  
  df_full_outliers_se_nb_da<-df_full_outliers[, .(delta_box=mean(delta_box), 
                                                  delta_box_sd=sd(delta_box)),
                                              by=list(var, year, species_evo_type, directional_speed, nb, da)]
  df_full_outliers_se_da<-df_full_outliers[, .(delta_box=mean(delta_box), 
                                               delta_box_sd=sd(delta_box)),
                                           by=list(var, year, species_evo_type, directional_speed, da)]
  df_full_outliers_se_da2<-df_full_outliers[, .(delta_box=mean(delta_box), 
                                                delta_box_sd=sd(delta_box)),
                                           by=list(var, year, species_evo_type, da)]
  saveRDS(df_full_outliers_se_nb_da, "../Data/niche_traits/niche_delta_nb_da.rda")
  saveRDS(df_full_outliers_se_da, "../Data/niche_traits/niche_delta_da.rda")
  saveRDS(df_full_outliers_se_da2, "../Data/niche_traits/niche_delta_da2.rda")
}
df_full_outliers_se_nb_da<-readRDS("../Data/niche_traits/niche_delta_nb_da_delta_abs.rda")
df_full_outliers_se_da<-readRDS("../Data/niche_traits/niche_delta_da.rda")

delta_da<-df_full_outliers_se_da[, c("var", "year", "species_evo_type", "directional_speed",
                                     "da", "delta_box")]

coms<-unique(delta_da[, c("var", "species_evo_type", "directional_speed")])
all_result<-list()
for (i in c(1:nrow(coms))){
  item<-delta_da[var==coms[i]$var & 
                   species_evo_type==coms[i]$species_evo_type ]
  all_result[[length(all_result)+1]]<-t.test_my2(item[da=="POOR"]$delta_box, item[da=="GOOD"]$delta_box, "greater", coms[i])
  all_result[[length(all_result)+1]]<-t.test_my2(item[da=="POOR"]$delta_box, item[da=="GOOD"]$delta_box, "less", coms[i])
}

all_result<-rbindlist(all_result)

all_result<-all_result[species_evo_type %in% c(3, 4)]
all_result<-all_result[var!="Debiased_Minimum_Monthly_Temperature"]
#all_result<-all_result[p_label!=""]
all_result$var<-ifelse(all_result$var=="Debiased_Maximum_Monthly_Precipitation", 
                       "Precipitation", "Temperature")
all_result$evo_type<-format_evoType(all_result$species_evo_type)
all_result[, label:=format_evoLabel(evo_type, directional_speed), 
           by=seq_len(nrow(all_result))]
setorderv(all_result, c("var", "species_evo_type", "directional_speed"))
all_result_df<-all_result
all_result_df<-all_result_df[species_evo_type %in% c(3, 4)]
all_result_df<-all_result_df[, c("var", "label", "alternative", "p_value")]
all_result_df$alternative<-ifelse(all_result_df$alternative=="greater", "Poor > Good", "Poor < Good")
my_table<-nice_table(all_result_df, 
                     title=c("Paired Samples T-test", "POOR vs GOOD"),
                     stars=T,
                     col.format.p=4)

flextable::save_as_docx(my_table, path ="../Figures/Table6.t.test.dispersal.nb_delta/t.test.docx" 
                          )

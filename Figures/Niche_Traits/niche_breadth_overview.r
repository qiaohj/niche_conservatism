library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
df_nb_trait<-readRDS("../Data/niche_traits/niche_traits_fn_without_outlier_with_next_year.rda")
df_nb_trait<-df_nb_trait[((directional_speed %in% c(0) & species_evo_type==1) |
                            (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                            (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)))]
df_nb_trait$year<-df_nb_trait$year - 1200

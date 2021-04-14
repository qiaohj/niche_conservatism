library(RSQLite)
library(DBI)
library(dplyr)
setwd("~/git/ees_3d/R/speciation_extinction")
da_list<-data.frame(da=c("GOOD", "POOR"), 
                    v=c("0.376596738,0.724239343,0.976679685,0.9995805560000001,1.0",
                        "0.710669364,0.999605965,0.9999990249999999,0.999999999299,1.0"),
                    stringsAsFactors = F)




base<-"/media/huijieqiao/Speciation_Extin/Configurations"
base_db<-sprintf("%s/env_Hadley3D.sqlite", base)
envdb <- dbConnect(RSQLite::SQLite(), base_db)
v_min_temp<-dbReadTable(envdb, "Debiased_Minimum_Monthly_Temperature")
v_max_temp<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Temperature")
v_max_prec<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Precipitation")
dbDisconnect(envdb)

nb_range<-list("BROAD"=c(60, 10),
               "NARROW"=c(40, 5))

base_db<-sprintf("%s/old_conf/conf_2.sqlite", base)
seed_db<-envdb <- dbConnect(RSQLite::SQLite(), base_db)
simulations_t<-dbReadTable(seed_db, "simulations")
dbDisconnect(seed_db)

mask_seed<-unique(simulations_t$global_id)


simulations<-NULL
i=1
id = 1
for (i in c(1:length(mask_seed))){
  item_t<-mask_seed[i]
  da<-da_list$da[1]
  start_point<-round(runif(5, 700, 1200))
  for (jj in c(1:length(start_point))){
    sp<-start_point[jj]
    print(paste(i, length(mask_seed), jj, sp))
    for (da in da_list$da){
      nb<-names(nb_range)[1]
      for (nb in names(nb_range)){
        for (niche_envolution_type in c(1,2,7)){
          for (niche_envolution_individual_ratio in c(0.05, 0.005)){
            if ((niche_envolution_type==1)&(niche_envolution_individual_ratio==0.05)){
              niche_envolution_individual_ratio<-1
            }
            if ((niche_envolution_type==1)&(niche_envolution_individual_ratio==0.005)){
              next()
            }
            item<-data.frame(id=id, global_id=item_t)
            item$random_index<-0
            item$label<-paste(item$global_id, da, nb, niche_envolution_type, niche_envolution_individual_ratio, sp, sep="_")
            id<-id+1
            item$da<-da
            item$nb<-nb
            nb_temp<-((v_max_temp %>% dplyr::filter((global_id==item$global_id)&(year==sp)))$v + 
              (v_min_temp %>% dplyr::filter((global_id==item$global_id)&(year==sp)))$v)/2
            nb1_min<-nb_temp-nb_range[[nb]][1]/2
            nb1_max<-nb_temp+nb_range[[nb]][1]/2
            nb_prec<-(v_max_prec %>% dplyr::filter((global_id==item$global_id)&(year==sp)))$v
            nb2_min<-nb_prec-nb_range[[nb]][2]/2
            nb2_max<-nb_prec+nb_range[[nb]][2]/2
            
            item$nb_v<-paste(paste(nb1_min, nb1_max, sep=","), 
                             paste(nb1_min, nb1_max, sep=","),
                             paste(nb2_min, nb2_max, sep=","),
                             sep="|")
            item$dispersal_ability<-da_list[which(da_list$da==da), "v"]
            item$dispersal_speed<-1
            item$dispersal_method<-2
            item$number_of_path<--1
            item$speciation_years<-100
            item$species_extinction_threshold<-0
            item$species_extinction_time_steps<-1
            item$species_extinction_threahold_percentage<-1
            item$group_extinction_threshold<-0
            item$initial_seeds<-item$global_id
            item$environments<-("Debiased_Minimum_Monthly_Temperature,Debiased_Maximum_Monthly_Temperature,Debiased_Maximum_Monthly_Precipitation")
            item$from<-sp
            item$to<-0
            item$step<--1
            item$mask<-"mask"
            item$burn_in_year<-0
            if (niche_envolution_type==1){
              item$niche_breadth_evolution_ratio<-"1,1,1,1"
            }
            if (niche_envolution_type==2){
              item$niche_breadth_evolution_ratio<-"0,1,1,1"
            }
            if (niche_envolution_type==7){
              item$niche_breadth_evolution_ratio<-"0,0,1,1"
            }
            item$niche_breadth_evolution_random_range<-0.01
            item$niche_breadth_evolution_parent_level<-5
            item$niche_envolution_individual_ratio<-niche_envolution_individual_ratio
            item$is_run<-1
            item$evo_type<-niche_envolution_type
            if (is.null(simulations)){
              simulations<-item
            }else{
              simulations<-bind_rows(simulations, item)
            }
          }
        }
      }
    }
  }
  mydb <- dbConnect(RSQLite::SQLite(), sprintf("%s/conf.sqlite", base))
  dbWriteTable(mydb, "simulations", simulations, overwrite=T)
  timeLine<-data.frame(from=1200, to=0, step=-1)
  dbWriteTable(mydb, "timeline", timeLine, overwrite=T)
  dbDisconnect(mydb)
}



{
  library(data.table)
  library(RSQLite)
  library(DBI)
  setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
  base_db<-"../Configuration/conf.sqlite"
  mydb <- dbConnect(RSQLite::SQLite(), base_db)
  #mydb <- dbConnect(RSQLite::SQLite(), "/media/huijieqiao/SSD_Fast/conf.sqlite")
  simulations<-dbReadTable(mydb, "simulations")
  dbDisconnect(mydb)
  simulations<-data.table(simulations)
  simulations<-simulations[is_run==1]
  combinations<-simulations[, .(N=.N), by=list(nb, da, species_evo_type, directional_speed)]
  combinations<-combinations[((directional_speed %in% c(0) & species_evo_type==1) |
                                (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                                (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)))]
  
  bootstrap_seeds_all<-readRDS("../Data/diversity/bootstrap_seeds.rda")
  xx<-bootstrap_seeds_all[, .(N=.N), by=list(global_id, rep)]
  xx[N>1]
  xx[N==max(N)]
  template<-"%d_%s_%s_%d_%s_%d"
  j=1
  i=1
  base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"
  
  rrrr<-51
  
  for (rrrr in c(1:100)){
    target<-sprintf("../Data/diversity_boostrap_items_full_year_unique/r_%d", rrrr)
    if (dir.exists(target)){
      next()
    }
    dir.create(target)
    
    bootstrap_seeds<-bootstrap_seeds_all[rep==rrrr]
    bootstrap_seeds_se<-bootstrap_seeds[,.(N=.N), by=list(global_id)]
    bootstrap_seeds_se<-bootstrap_seeds_se[N>1]
    bootstrap_seeds<-bootstrap_seeds_se
    bootstrap_seeds$N<-bootstrap_seeds$N - 1
    d_year<-list()
    for (y in c(0:1198)){
      d_year[[as.character(y)]]<-list()
    }
    for (i in c(1:nrow(bootstrap_seeds))){
      seed<-bootstrap_seeds[i]
      
      for (j in c(1:nrow(combinations))){
        
        com<-combinations[j]
        print(paste("rep", rrrr, i, nrow(bootstrap_seeds), j, nrow(combinations),
                    seed$global_id, com$da, com$nb, com$species_evo_type, 
                    com$directional_speed))
        sp<-sprintf(template, seed$global_id, com$da, com$nb, com$species_evo_type, 
                    com$directional_speed, 0)
        ttt<-sprintf("%s/%s/%s.diversity.rda", base, sp, sp)
        df_full<-readRDS(ttt)
        
        if (is.null(df_full)){
          next()
        }
        years<-unique(df_full$year)
        for (y in years){
          df_item<-df_full[year==y]
          if (nrow(df_item)==0){
            next()
          }
          
          #range(df_item$year)
          df_item$seed_id<-seed$global_id
          df_item$da<-com$da
          df_item$nb<-com$nb
          df_item$species_evo_type<-com$species_evo_type
          df_item$directional_speed<-com$directional_speed 
          df_item$N_SPECIES<-df_item$N_SPECIES * seed$N
          df_item$N_INDIVIDUAL<-df_item$N_INDIVIDUAL * seed$N
          
          d_year[[as.character(y)]][[length(d_year[[as.character(y)]])+1]]<-df_item
        }
        
      }
      
    }
    for (y in c(0:1198)){
      setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
      print(paste("Saving ", y))
      dddd<-d_year[[as.character(y)]]
      
      dddd<-rbindlist(dddd)
      diversity_se<-dddd[, .(N_SPECIES=sum(N_SPECIES), N_INDIVIDUAL=sum(N_INDIVIDUAL)),
                         by=list(year, global_id, 
                                 species_evo_type, directional_speed)]
      diversity_se$rep<-rrrr
      saveRDS(diversity_se, sprintf("%s/y%d_r%d.rda", target, y, rrrr))
      diversity_se_nb<-dddd[, .(N_SPECIES=sum(N_SPECIES), N_INDIVIDUAL=sum(N_INDIVIDUAL)),
                            by=list(year, global_id, 
                                    species_evo_type, directional_speed, nb)]
      diversity_se_nb$rep<-rrrr
      saveRDS(diversity_se_nb, sprintf("%s/y%d_r%d_nb.rda", target, y, rrrr))
      diversity_se_nb_da<-dddd[, .(N_SPECIES=sum(N_SPECIES), N_INDIVIDUAL=sum(N_INDIVIDUAL)),
                               by=list(year, global_id, 
                                       species_evo_type, directional_speed, nb, da)]
      
      diversity_se_nb_da$rep<-rrrr
      saveRDS(diversity_se_nb_da, sprintf("%s/y%d_r%d_nb_da.rda", target, y, rrrr))
    }
  }
  
}

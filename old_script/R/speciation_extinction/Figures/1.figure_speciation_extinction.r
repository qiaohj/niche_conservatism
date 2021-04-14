library("ggplot2")
library("dplyr")
library("Hmisc")
library("Rmisc")
library("vegan")
library("lubridate")
library("zoo")
library("stats")
library("GeneCycle")
library("TTR")
library("forecast")
library("gridExtra")
library("tseries")
library("SimilarityMeasures")
library("patchwork")
library("hrbrthemes")
library("pastecs")
library("mgcv")

base<-"/home/huijieqiao/git/ees_3d_data/SMART_SPECIES"
stat<-readRDS(sprintf("%s/Tables/Speciation_Extinction/stat.rda", base))

detail<-readRDS(sprintf("%s/Tables/Speciation_Extinction/detail.rda", base))

fix_type<-function(x){
  x[which(x==1)]<-"Lazy"
  x[which(x==2)]<-"Darwin I"
  x[which(x==3)]<-"AI Lamarck"
  x[which(x==4)]<-"AI"
  x[which(x==5)]<-"Lamarck II"
  x[which(x==6)]<-"Darwin xII"
  x[which(x==7)]<-"Darwin II"
  x[which(x==8)]<-"Combined"
  x[which(x==9)]<-"Lamarck I"
  x
}

fix_df<-function(p_df){
  colnames(p_df)<-toupper(colnames(p_df))
  p_df<-p_df%>%dplyr::filter(EVO_TYPE %in% c(1,2,4,5,7,8,9))
  p_df$EVO_TYPE<-fix_type(p_df$EVO_TYPE)
  
  p_df_0<-p_df%>%dplyr::filter(EVO_RATIO==1)
  p_df_1<-p_df%>%dplyr::filter(EVO_RATIO!=1)
  p_df_0$EVO_RATIO<-0.05
  p_df<-bind_rows(p_df_1, p_df_0)
  p_df_0$EVO_RATIO<-0.005
  p_df<-bind_rows(p_df, p_df_0)
  
  p_df$WARP_LABEL<-paste(p_df$NB, p_df$DA, p_df$EVO_RATIO)
  p_df
}

stat<-fix_df(stat)


cols<-c("turquoise",
        "chocolate",
        "black")
rep=100
sample_size=100
seeds<-unique(stat$GLOBAL_ID)

x_year_label<-"Year (*100)"
ribbon_alpha<-0.1
names(cols)<-unique(stat$EVO_TYPE)
stat_speciation<-stat%>%
  dplyr::select(GLOBAL_ID, DA, NB, EVO_TYPE, EVO_RATIO, N_SPECIATION, WARP_LABEL)
colnames(stat_speciation)[6]<-"N"
stat_speciation$TYPE<-"SPECIATION"
stat_extinction<-stat%>%
  dplyr::select(GLOBAL_ID, DA, NB, EVO_TYPE, EVO_RATIO, N_EXTINCT, WARP_LABEL)
colnames(stat_extinction)[6]<-"N"
stat_extinction$TYPE<-"EXTINCTION"
stat_df<-bind_rows(stat_extinction, stat_speciation)

if (F){
  stat_df_rep<-NULL
  for (i in c(1:rep)){
    print(i)
    sub_seeds<-seeds[sample(length(seeds), sample_size)]
    sub_stat_df<-stat_df%>%dplyr::filter(GLOBAL_ID %in% sub_seeds)
    sub_stat_df$REP<-i
    if (is.null(stat_df_rep)){
      stat_df_rep<-sub_stat_df
    }else{
      stat_df_rep<-bind_rows(stat_df_rep, sub_stat_df)
    }
  }
  saveRDS(stat_df_rep, sprintf("%s/Tables/Speciation_Extinction/stat_df_rep.rda", base))
}

stat_df_rep<-readRDS(sprintf("%s/Tables/Speciation_Extinction/stat_df_rep.rda", base))
stat_df_sum<-stat_df_rep%>%dplyr::group_by(DA, NB, EVO_TYPE, EVO_RATIO, WARP_LABEL, TYPE, REP)%>%
  dplyr::summarise(N=sum(N))

stat_df_sum_se<-stat_df_sum%>%ungroup()%>%
  dplyr::group_by(DA, NB, EVO_TYPE, EVO_RATIO, WARP_LABEL, TYPE)%>%
  dplyr::summarise(MEAN=mean(N),
                   SD=sd(N),
                   CI=CI(N)[2]-CI(N)[3],
                   N_SEED=n())
p<-ggplot(stat_df_sum_se, 
          aes(x=EVO_TYPE, y= MEAN, fill=factor(TYPE)))+
  geom_bar(stat="identity", position = "dodge2")+
  geom_errorbar(aes(ymin=MEAN-CI, ymax=MEAN+CI), width=.2,
                position=position_dodge(.9))+
  theme_bw()+
  xlab("Scenario")+
  ylab("N")+
  ggtitle("N of speciation and extinction per senario")+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')
ggsave(p, filename=sprintf("%s/Figures/Speciation_Extinction/speciation_extinction_by_scenario.png", base))

detail<-fix_df(detail)
if (F){
  detail_df_rep<-NULL
  for (i in c(1:rep)){
    print(i)
    sub_seeds<-seeds[sample(length(seeds), sample_size)]
    sub_detail_df<-detail%>%dplyr::filter(GLOBAL_ID %in% sub_seeds)
    sub_detail_df$REP<-i
    if (is.null(detail_df_rep)){
      detail_df_rep<-sub_detail_df
    }else{
      detail_df_rep<-bind_rows(detail_df_rep, sub_detail_df)
    }
  }
  saveRDS(detail_df_rep, sprintf("%s/Tables/Speciation_Extinction/detail_df_rep.rda", base))
  
  detail_df_rep<-readRDS(sprintf("%s/Tables/Speciation_Extinction/detail_df_rep.rda", base))
  year=100
  speciation_extinction_by_year<-NULL
  for (i in c(1:rep)){ 
    detail_df_rep_sub<-detail_df_rep%>%dplyr::filter(REP==i)
    for (year in c(1200:0)){
      print(paste(year, i))
      N_SPECIES<-detail_df_rep_sub%>%dplyr::filter((TO<=year)&(FROM>=year))%>%
        dplyr::group_by(DA, NB, EVO_TYPE, EVO_RATIO, WARP_LABEL)%>%
        dplyr::summarise(N_SPECIES=n())
      N_SPECIATION<-detail_df_rep_sub%>%dplyr::filter((year==TO)&(TYPE=="NODE"))%>%
        dplyr::group_by(DA, NB, EVO_TYPE, EVO_RATIO, WARP_LABEL)%>%
        dplyr::summarise(N_SPECIATION=n())
      N_EXTINCTION<-detail_df_rep_sub%>%dplyr::filter((year==TO)&(TYPE=="LEAF"))%>%
        dplyr::group_by(DA, NB, EVO_TYPE, EVO_RATIO, WARP_LABEL)%>%
        dplyr::summarise(N_EXTINCTION=n())
      
      item<-left_join(N_SPECIES, N_SPECIATION, by=c("DA", "NB", "EVO_TYPE", "EVO_RATIO", "WARP_LABEL"))
      item<-left_join(item, N_EXTINCTION, by=c("DA", "NB", "EVO_TYPE", "EVO_RATIO", "WARP_LABEL"))
      item[is.na(item)]<-0
      item$YEAR<-year
      item$REP<-i
      if (is.null(speciation_extinction_by_year)){
        speciation_extinction_by_year<-item
      }else{
        speciation_extinction_by_year<-bind_rows(speciation_extinction_by_year, item)
      }
    }
    saveRDS(speciation_extinction_by_year, 
            sprintf("%s/Tables/Speciation_Extinction/speciation_extinction_by_year.rda", base))
    
  }
  saveRDS(speciation_extinction_by_year, 
          sprintf("%s/Tables/Speciation_Extinction/speciation_extinction_by_year.rda", base))
}

speciation_extinction_by_year<-readRDS(sprintf("%s/Tables/Speciation_Extinction/speciation_extinction_by_year.rda", base))
speciation_extinction_by_year_se<-speciation_extinction_by_year%>%ungroup()%>%
  dplyr::group_by(DA, NB, EVO_TYPE, EVO_RATIO, WARP_LABEL, YEAR)%>%
  dplyr::summarise(MEAN_N_SPECIES=mean(N_SPECIES),
                   SD_N_SPECIES=sd(N_SPECIES),
                   CI_N_SPECIES=CI(N_SPECIES)[2]-CI(N_SPECIES)[3],
                   MEAN_N_SPECIATION=mean(N_SPECIATION),
                   SD_N_SPECIATION=sd(N_SPECIATION),
                   CI_N_SPECIATION=CI(N_SPECIATION)[2]-CI(N_SPECIATION)[3],
                   MEAN_N_EXTINCTION=mean(N_EXTINCTION),
                   SD_N_EXTINCTION=sd(N_EXTINCTION),
                   CI_N_EXTINCTION=CI(N_EXTINCTION)[2]-CI(N_EXTINCTION)[3],
                   N_SEED=n())

saveRDS(speciation_extinction_by_year_se, 
        sprintf("%s/Tables/Speciation_Extinction/speciation_extinction_by_year_se.rda", base))
speciation_extinction_by_year_se$YEAR<-speciation_extinction_by_year_se$YEAR*-1

speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==0), "MEAN_N_EXTINCTION"]<-0
speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==0), "SD_N_EXTINCTION"]<-0
speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==0), "CI_N_EXTINCTION"]<-0

speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==0), "MEAN_N_SPECIATION"]<-0
speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==0), "SD_N_SPECIATION"]<-0
speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==0), "CI_N_SPECIATION"]<-0

speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==-1200), "MEAN_N_SPECIATION"]<-0
speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==-1200), "SD_N_SPECIATION"]<-0
speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==-1200), "CI_N_SPECIATION"]<-0

speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==-1199), "MEAN_N_SPECIATION"]<-0
speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==-1199), "SD_N_SPECIATION"]<-0
speciation_extinction_by_year_se[which(speciation_extinction_by_year_se$YEAR==-1199), "CI_N_SPECIATION"]<-0

speciation_extinction_by_year_se$AVERAGE_SPECIATION<-
  speciation_extinction_by_year_se$MEAN_N_SPECIATION/
  speciation_extinction_by_year_se$MEAN_N_SPECIES
speciation_extinction_by_year_se$AVERAGE_EXTINCTION<-
  speciation_extinction_by_year_se$MEAN_N_EXTINCTION/
  speciation_extinction_by_year_se$MEAN_N_SPECIES


p<-ggplot(speciation_extinction_by_year_se, 
          aes(x=YEAR, y= MEAN_N_SPECIES, color=factor(EVO_TYPE)))+
  geom_line()+
  geom_errorbar(aes(ymin=MEAN_N_SPECIES-CI_N_SPECIES, ymax=MEAN_N_SPECIES+CI_N_SPECIES), width=.2,
                position=position_dodge(.9), alpha=ribbon_alpha)+
  theme_bw()+
  xlab("Time step")+
  ylab("N Species")+
  ggtitle("N of Species by year")+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')
ggsave(p, filename=sprintf("%s/Figures/Speciation_Extinction/species_by_year.png", base))

p<-ggplot(speciation_extinction_by_year_se %>% dplyr::filter(YEAR>=-1100), 
          aes(x=YEAR, y= MEAN_N_SPECIATION, color=factor(EVO_TYPE)))+
  geom_line()+
  #geom_errorbar(aes(ymin=MEAN_N_SPECIATION-CI_N_SPECIATION, ymax=MEAN_N_SPECIATION+CI_N_SPECIATION), width=.2,
  #              position=position_dodge(.9), alpha=ribbon_alpha)+
  theme_bw()+
  xlab("Time step")+
  ylab("N SPECIATION")+
  ggtitle("N of SPECIATION by year")+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')
ggsave(p, filename=sprintf("%s/Figures/Speciation_Extinction/speciation_by_year.png", base))

p<-ggplot(speciation_extinction_by_year_se %>% dplyr::filter((YEAR>=-1100)&(YEAR<0)), 
          aes(x=YEAR, y= MEAN_N_EXTINCTION, color=factor(EVO_TYPE)))+
  geom_line()+
  #geom_errorbar(aes(ymin=MEAN_N_EXTINCTION-CI_N_EXTINCTION, ymax=MEAN_N_EXTINCTION+CI_N_EXTINCTION), width=.2,
  #              position=position_dodge(.9), alpha=ribbon_alpha)+
  theme_bw()+
  xlab("Time step")+
  ylab("N EXTINCTION")+
  ggtitle("N of EXTINCTION by year")+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')
ggsave(p, filename=sprintf("%s/Figures/Speciation_Extinction/extinction_by_year.png", base))

p<-ggplot(speciation_extinction_by_year_se %>% dplyr::filter(YEAR>=-1100), 
          aes(x=YEAR, y= AVERAGE_SPECIATION, color=factor(EVO_TYPE)))+
  geom_line()+
  #geom_errorbar(aes(ymin=MEAN_N_SPECIATION-CI_N_SPECIATION, ymax=MEAN_N_SPECIATION+CI_N_SPECIATION), width=.2,
  #              position=position_dodge(.9), alpha=ribbon_alpha)+
  theme_bw()+
  xlab("Time step")+
  ylab("N SPECIATION PER SPECIES")+
  ggtitle("SPECIATION PER SPECIES by year")+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')
ggsave(p, filename=sprintf("%s/Figures/Speciation_Extinction/speciation_per_species_by_year.png", base))

p<-ggplot(speciation_extinction_by_year_se %>% dplyr::filter((YEAR>=-1100)&(YEAR<0)), 
          aes(x=YEAR, y= AVERAGE_EXTINCTION, color=factor(EVO_TYPE)))+
  geom_line()+
  #geom_errorbar(aes(ymin=MEAN_N_EXTINCTION-CI_N_EXTINCTION, ymax=MEAN_N_EXTINCTION+CI_N_EXTINCTION), width=.2,
  #              position=position_dodge(.9), alpha=ribbon_alpha)+
  theme_bw()+
  xlab("Time step")+
  ylab("N EXTINCTION PER SPECIES")+
  ggtitle("N EXTINCTION PER SPECIES by year")+
  facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')
ggsave(p, filename=sprintf("%s/Figures/Speciation_Extinction/extinction_per_species_by_year.png", base))



if (F){
  library("DBI")
  mydb <- dbConnect(RSQLite::SQLite(), sprintf("%s/ISEA3H8/SQLITE/env_Hadley3D.sqlite", base))  
  max_prec<-dbReadTable(mydb, "Debiased_Maximum_Monthly_Precipitation")
  max_temp<-dbReadTable(mydb, "Debiased_Maximum_Monthly_Temperature")
  min_temp<-dbReadTable(mydb, "Debiased_Minimum_Monthly_Temperature")
  dbDisconnect(mydb) 
  head(max_prec)
  max_prec_se<-max_prec%>%dplyr::group_by(year)%>%
    dplyr::summarise(mean=mean(v),
                     sd=sd(v),
                     ci=CI(v)[2]-CI(v)[3])
  max_prec_se$year<-max_prec_se$year*-1
  max_prec_se$type<-"Maximum Monthly Precipitation"
  
  max_temp_se<-max_temp%>%dplyr::group_by(year)%>%
    dplyr::summarise(mean=mean(v),
                     sd=sd(v),
                     ci=CI(v)[2]-CI(v)[3])
  max_temp_se$type<-"Maximum Monthly Temperature"
  
  max_temp_se$year<-max_temp_se$year*-1
  plot(max_temp_se$year, max_temp_se$mean, type="l", col="red")
  
  min_temp_se<-min_temp%>%dplyr::group_by(year)%>%
    dplyr::summarise(mean=mean(v),
                     sd=sd(v),
                     ci=CI(v)[2]-CI(v)[3])
  min_temp_se$type<-"Minimum Monthly Temperature"
  min_temp_se$year<-min_temp_se$year*-1
  lines(min_temp_se$year, min_temp_se$mean, col="blue")
  
  env_se<-bind_rows(bind_rows(max_prec_se, max_temp_se), min_temp_se)
  
  env_df_new<-NULL
  for (var in c("Maximum Monthly Precipitation",
                "Maximum Monthly Temperature",
                "Minimum Monthly Temperature")){
    print(var)
    env_df<-env_se%>%dplyr::filter(type==var)
    
    max_temp_gam<-gam(mean~s(year, k=20),data=env_df)
    #plot(max_temp_gam)
    
    env_df$predicted<-predict(max_temp_gam, env_df)
    
    v<-as.vector(pull(env_df, predicted))
    tp<-turnpoints(v)
    env_df$peak<-tp$peaks
    env_df$pit<-tp$pits
    env_df$direction<-0
    env_df$group<-0
    env_df$length<-0
    direction<-0
    length<-0
    group<-0
    for (year in c(-1200:0)){
      if (pull(env_df[which(env_df$year==year), "peak"])){
        env_df[which(env_df$group==group), "length"]<-length
        direction<- -1
        length<-0
        group<-group+1
      }
      if (pull(env_df[which(env_df$year==year), "pit"])){
        env_df[which(env_df$group==group), "length"]<-length
        direction<- 1
        length<-0
        group<-group+1
      }
      length<-length+1
      env_df[which(env_df$year==year), "direction"]<- direction
      env_df[which(env_df$year==year), "group"]<- group
    }
    env_df[which(env_df$group==group), "length"]<-length
    unique(env_df[, c("group", "length")])

    if (is.null(env_df_new)){
      env_df_new<-env_df
    }else{
      env_df_new<-bind_rows(env_df_new, env_df)
    }
    ggplot(env_df, aes(x=year, y=predicted, color=factor(direction)))+geom_point()
  }
  saveRDS(env_df_new, sprintf("%s/Tables/Speciation_Extinction/env_se.rda", base))
}

env_se<-readRDS(sprintf("%s/Tables/Speciation_Extinction/env_se.rda", base))

ggplot(env_se, aes(x=year, y=mean, color=factor(type)))+geom_line()

if (F){
  prec<-env_se%>%dplyr::filter(type=="Maximum Monthly Precipitation")
  range(prec$mean)
  min_prec<-min(prec$mean)
  range_prec<-range(prec$mean)[2] - range(prec$mean)[1]
  
}
min_prec<-6
scale<-23
offset<-35


p<-ggplot() +
  geom_line(data=env_se%>%dplyr::filter(type!="Maximum Monthly Precipitation"),
            aes(x=year, y=mean, color=factor(type)), size=2) + 
  geom_line(data=env_se%>%dplyr::filter(type=="Maximum Monthly Precipitation"),
            aes(x=year, y=(mean-min_prec)*scale-offset), size=2, color="black") + 
  #geom_point(data=env_se%>%dplyr::filter(type!="Maximum Monthly Precipitation"),
  #          aes(x=year, y=predicted, group=type, color=factor(direction)), size=0.5) + 
  #geom_point(data=env_se%>%dplyr::filter(type=="Maximum Monthly Precipitation"),
  #          aes(x=year, y=(predicted-min_prec)*scale-offset, color=factor(direction)), size=0.5) + 
  
  scale_y_continuous(
    # Features of the first axis
    name = "Monthly Temperature",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(.-min_prec)*scale-offset, name="Monthly Precipitation")
  ) + 
  
  theme_ipsum() +
  ggtitle("Environmental variables")
ggsave(p, filename=sprintf("%s/Figures/Speciation_Extinction/env_curves.png", base), width=10, height = 5)

p<-ggplot() +
  geom_line(data=env_se%>%dplyr::filter(type!="Maximum Monthly Precipitation"),
            aes(x=year, y=mean, color=factor(type)), size=2) + 
  geom_line(data=env_se%>%dplyr::filter(type=="Maximum Monthly Precipitation"),
            aes(x=year, y=(mean-min_prec)*scale-offset), size=2, color="black") + 
  geom_point(data=env_se%>%dplyr::filter(type!="Maximum Monthly Precipitation"),
             aes(x=year, y=predicted, group=type, color=factor(direction)), size=0.5) + 
  geom_point(data=env_se%>%dplyr::filter(type=="Maximum Monthly Precipitation"),
             aes(x=year, y=(predicted-min_prec)*scale-offset, color=factor(direction)), size=0.5) + 
  
  scale_y_continuous(
    # Features of the first axis
    name = "Monthly Temperature",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(.-min_prec)*scale-offset, name="Monthly Precipitation")
  ) + 
  
  theme_ipsum() +
  ggtitle("Environmental variables")
ggsave(p, filename=sprintf("%s/Figures/Speciation_Extinction/env_curves_with_smooth.png", base), width=10, height = 5)

speciation_extinction_by_year_se_var<-speciation_extinction_by_year_se
for (var in c("Maximum Monthly Precipitation",
              "Maximum Monthly Temperature",
              "Minimum Monthly Temperature")){
  env<-env_se%>%dplyr::filter(type==var)
  speciation_extinction_by_year_se_var<-left_join(speciation_extinction_by_year_se,
                                                  env[, c("year", "direction", "group", "length")],
                                                  by=c("YEAR"="year"))
  col_name<-ncol(speciation_extinction_by_year_se_var)
  col_name<-c((col_name-2):col_name)
  colnames(speciation_extinction_by_year_se_var)[col_name]<-
    c("DIRECTION", "GROUP", "LENGTH")
  
  speciation_extinction_direction<-speciation_extinction_by_year_se_var%>%ungroup()%>%
    dplyr::group_by(DA, NB, EVO_TYPE, EVO_RATIO, WARP_LABEL, GROUP, DIRECTION, LENGTH)%>%
    dplyr::summarise(SPECIATION=sum(AVERAGE_SPECIATION),
                     EXTINCTION=sum(AVERAGE_EXTINCTION))
  speciation_extinction_direction$AVERAGE_SPECIATION<-
    speciation_extinction_direction$SPECIATION/speciation_extinction_direction$LENGTH
  speciation_extinction_direction$AVERAGE_EXTINCTION<-
    speciation_extinction_direction$EXTINCTION/speciation_extinction_direction$LENGTH
  
  speciation_extinction_direction$DIRECTION_STR<-""
  speciation_extinction_direction[which(speciation_extinction_direction$DIRECTION==1), "DIRECTION_STR"]<-"INCREASE"
  speciation_extinction_direction[which(speciation_extinction_direction$DIRECTION==0), "DIRECTION_STR"]<-"UNKNOWN"
  speciation_extinction_direction[which(speciation_extinction_direction$DIRECTION==-1), "DIRECTION_STR"]<-"DECREASE"
  
  
  p<-ggplot(speciation_extinction_direction, 
         aes(x=factor(GROUP), y=AVERAGE_SPECIATION, shape=factor(DIRECTION_STR), color=factor(EVO_TYPE)))+
    geom_point()+
    theme_bw()+
    ggtitle(sprintf("N SPECIATION PER SPECIES by group in (%s)", var))+
    facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')
  ggsave(p, filename=
           sprintf("%s/Figures/Speciation_Extinction/speciation_per_species_by_group_%s.png", 
                   base, gsub(" ", "_", var)))
  p<-ggplot(speciation_extinction_direction, 
         aes(x=factor(GROUP), y=AVERAGE_EXTINCTION, shape=factor(DIRECTION_STR), color=factor(EVO_TYPE)))+
    geom_point()+
    theme_bw()+
    ggtitle(sprintf("N EXTINCTION PER SPECIES by group in (%s)", var))+
    facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')
  ggsave(p, filename=
           sprintf("%s/Figures/Speciation_Extinction/extinction_per_species_by_group_%s.png", 
                   base, gsub(" ", "_", var)))
  
}


library(stringr)
base<-"/home/huijieqiao/git/ees_3d_data/JSON_TEMPLATE"
target<-"/home/huijieqiao/git/ees_3d_data/TEST"


mask_f<-"/home/huijieqiao/git/ees_3d_data/ISEA3H8/CSV/mask.csv"
da<-read.csv("/home/huijieqiao/git/ees_3d_data/JSON_TEMPLATE/dispersal.csv", sep=",", stringsAsFactors = F)
v_mean_temp_f<-"/home/huijieqiao/git/ees_3d_data/ISEA3H8/CSV/Debiased_Mean_Annual_Temperature"
v_mean_prec_f<-"/home/huijieqiao/git/ees_3d_data/ISEA3H8/CSV/Debiased_Mean_Annual_Precipitation"
mask<-read.csv(mask_f, sep=" ", stringsAsFactors = F)
v_mean_temp<-read.csv(sprintf("%s/0000.csv", v_mean_temp_f), sep=" ", stringsAsFactors = F)
v_mean_prec<-read.csv(sprintf("%s/0000.csv", v_mean_prec_f), sep=" ", stringsAsFactors = F)

hist(v_mean_temp$v)
hist(v_mean_prec$v)
species <- sprintf("%s/species.json", base)
str<-readChar(species, file.info(species)$size)
id=31832

da_label<-"GOOD"

nb_range_label<-"BROAD"
nb_range<-list("BROAD"=c(20, 2),
               "MODERATE"=c(10,1),
               "NARROW"=c(5,0.5))


nb1_min<-v_mean_temp[which(v_mean_temp$global_id==id), "v"]-nb_range[[nb_range_label]][1]/2
nb1_max<-v_mean_temp[which(v_mean_temp$global_id==id), "v"]+nb_range[[nb_range_label]][1]/2
nb2_min<-v_mean_prec[which(v_mean_prec$global_id==id), "v"]-nb_range[[nb_range_label]][2]/2
nb2_max<-v_mean_prec[which(v_mean_prec$global_id==id), "v"]+nb_range[[nb_range_label]][2]/2
dispersal_ability<-da[which(da$da==da_label), "v"]
dispersal_speed<-100
dispersal_method<-2
number_of_path<--1
speciation_years<-10000
species_extinction_threshold<-0
species_extinction_time_steps<-1
species_extinction_threahold_percentage<-0.8
group_extinction_threshold<-0
initial_seeds<-id

str<-str_replace(str, "%id", as.character(id))
str<-str_replace(str, "%nb1_min", as.character(nb1_min))
str<-str_replace(str, "%nb1_max", as.character(nb1_max))
str<-str_replace(str, "%nb2_min", as.character(nb2_min))
str<-str_replace(str, "%nb2_max", as.character(nb2_max))
str<-str_replace(str, "%dispersal_ability", as.character(dispersal_ability))
str<-str_replace(str, "%dispersal_speed", as.character(dispersal_speed))
str<-str_replace(str, "%dispersal_method", as.character(dispersal_method))
str<-str_replace(str, "%number_of_path", as.character(number_of_path))
str<-str_replace(str, "%speciation_years", as.character(speciation_years))
str<-str_replace(str, "%species_extinction_threshold", as.character(species_extinction_threshold))
str<-str_replace(str, "%species_extinction_time_steps", as.character(species_extinction_time_steps))
str<-str_replace(str, "%species_extinction_threahold_percentage", as.character(species_extinction_threahold_percentage))
str<-str_replace(str, "%group_extinction_threshold", as.character(group_extinction_threshold))
str<-str_replace(str, "%initial_seeds", as.character(initial_seeds))
species_label<-sprintf("%d_%s_%s", id, da_label, nb_range_label)
write(str, sprintf("%s/Species_Configurations/%s.json", 
                       target, species_label))



scenario<-sprintf("%s/scenario.json", base)
str<-readChar(scenario, file.info(scenario)$size)
environment1<-v_mean_temp_f
environment2<-v_mean_prec_f
total_years<-120000
mask<-mask_f
species<-species_label
burn_in_year<-0
neighbor_info<-"/home/huijieqiao/git/ees_3d_data/ISEA3H8/isea3h8neigpbor.nbr"
str<-str_replace(str, "%environment1", as.character(environment1))
str<-str_replace(str, "%environment2", as.character(environment2))
str<-str_replace(str, "%total_years", as.character(total_years))
str<-str_replace(str, "%mask", as.character(mask))
str<-str_replace(str, "%species", as.character(species))
str<-str_replace(str, "%burn_in_year", as.character(burn_in_year))
str<-str_replace(str, "%neighbor_info", as.character(neighbor_info))
write(str, sprintf("%s/Scenario_Configurations/%s.json", 
                   target, species_label))


cmd<-"ees_3d"
print(sprintf("./%s %s %s %s/Results 64000 1 1 0", cmd, target, species_label, target))

bind<-function(df1, df2){
  if (is.null(df1)){
    df2
  }else{
    rbindlist(list(df1, df2))
  }
}

#1: conservatism
#2: shift-directional
#3: expansion-directional
#4: expansion-omnidirectional
#5: random-central
#6: random-symmetrical
#7: random-asymmetrical
evo_types<-c("conservatism",
             "shift-directional",
             "expansion-directional",
             "expansion-omnidirectional",
             "random-central",
             "random-symmetrical",
             "random-asymmetrical")
evo_type_amp<-data.frame(type=c("conservatism",
             "shift-directional", "shift-directional", "shift-directional",
             "expansion-directional", "expansion-directional", "expansion-directional",
             "expansion-omnidirectional", "expansion-omnidirectional", "expansion-omnidirectional",
             "random-central",
             "random-symmetrical",
             "random-asymmetrical"),
             amp=c(0.00, 0.01, 0.10, 0.50, 0.01, 0.10, 0.50, 0.01, 0.10, 0.50,
                   0.01, 0.01, 0.01))

format_evoType<-function(index){
  evo_type<-rep(evo_types[1], length(index))
  evo_type[index==2]<-evo_types[2]
  evo_type[index==3]<-evo_types[3]
  evo_type[index==4]<-evo_types[4]
  evo_type[index==5]<-evo_types[5]
  evo_type[index==6]<-evo_types[6]
  evo_type[index==7]<-evo_types[7]
  evo_type<-factor(evo_type, levels=evo_types)
}

format_evoType_amplitude<-function(evoType, amplitude, order=1){
  evo_type<-sprintf(sprintf("%s (%.2f)", evoType, amplitude))
  evo_type_amps<-sprintf(sprintf("%s (%.2f)", evo_type_amp$type, evo_type_amp$amp))
  if (order==-1){
    evo_type_amps<-rev(evo_type_amps)
  }
  evo_type<-factor(evo_type, levels=evo_type_amps)
}



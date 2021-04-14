library(dplyr)
library(ggplot2)

result_nb<-readRDS("../../../ees_3d_data/SMART_SPECIES/Tables/individual_ratio_nb.rda")
nb<-unique(result_nb$NB)
da<-unique(result_nb$DA)
id<-unique(result_nb$GLOBAL_ID)
evo_type<-unique(result_nb$EVO_TYPE)
evo_ratio<-unique(result_nb$EVO_RATIO)

coms<-expand.grid(nb=nb, da=da, id=id, 
                  evo_ratio=evo_ratio, stringsAsFactors = F)

i=id[1]
for (i in id){
  
  item<-result_nb%>%filter(GLOBAL_ID==i)
  if (length(unique(item$Y))<500){
    next()
  }
  print(i)
  item<-fix_df(item)
  
  item_sum<-item%>%dplyr::group_by(Y, NB, DA, EVO_RATIO, WARP_LABEL, EVO_TYPE)%>%dplyr::summarise(
    MIN_TEMP=min(TEMP_LOW),
    MAX_TEMP=max(TEMP_HIGH),
    MIN_PREC=min(PREC_LOW),
    MAX_PREC=max(PREC_HIGH)
    )
  print(unique(item_sum$EVO_TYPE))
  p<-ggplot(item_sum, aes(x=Y))+
    geom_line(aes(y=MIN_TEMP, color=EVO_TYPE))+
    geom_line(aes(y=MAX_TEMP, color=EVO_TYPE))+
    theme_bw()+
    xlab(x_year_label)+
    ylab("Temperature")+
    ggtitle(sprintf("High / LOW limit of Temperature (ID: %d)", i))+
    scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+
    facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')
  print(p)
  inp <- readline(prompt="Enter to continue, X to exit: ")
  if (toupper(inp)=="X"){
    break()
  }
  p<-ggplot(item_sum, aes(x=Y))+
    geom_line(aes(y=MIN_PREC, color=EVO_TYPE))+
    geom_line(aes(y=MAX_PREC, color=EVO_TYPE))+
    theme_bw()+
    xlab(x_year_label)+
    ylab("Precipitation")+
    ggtitle(sprintf("High / LOW limit of Precipitation (ID: %d)", i))+
    scale_colour_manual(values = cols,aesthetics = c("colour", "fill"))+
    facet_wrap( ~ WARP_LABEL, ncol=2, scales = 'free')
  print(p)
  inp <- readline(prompt="Enter to continue, X to exit: ")
  if (toupper(inp)=="X"){
    break()
  }
}

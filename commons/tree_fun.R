setColor<-function(id, color_i){
  if (data_df[which(data_df$id==id), "colorIndex"]!=0){
    return()
  }
  data_df[which(data_df$id==id), "colorIndex"]<<-color_i
  if (!is.na(data_df[which(data_df$id==id), "ancestor"])){
    setColor(data_df[which(data_df$id==id), "ancestor"], color_i)
  }
}
set_label<-function(node){
  item<-data_df[node, ]
  if (!is.na(item$ancestor)){
    ancestor<-data_df[item$ancestor, ]
    data_df[node, "new_label"]<<-sprintf("%s-%s", 
                                         ancestor$new_label, item$simp_label)
  }
  children<-children(phylo_tree, node)
  if (length(children)==0){
    return()
  }
  for (i in c(1:length(children))){
    set_label(children[i])
  }
  
}
library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
nb_mammals<-readRDS("../Data/IUCN_NB/Mammals.rda")
nb_birds<-readRDS("../Data/IUCN_NB/Birds.rda")
nb_mammals$range_t<-nb_mammals$tmax_max - nb_mammals$tmin_min
nb_mammals$range_p<-nb_mammals$prcp_max - nb_mammals$prcp_min
nb_mammals$group<-"Mammals"
nb_birds$range_t<-nb_birds$tmax_max - nb_birds$tmin_min
nb_birds$range_p<-nb_birds$prcp_max - nb_birds$prcp_min
nb_birds$group<-"Birds"
nb_iucn<-rbindlist(list(nb_mammals, nb_birds))

nb_range_last_year<-readRDS("../Data/niche_traits/niche_traits_last_year_fn.rda")


ggplot(nb_iucn)+geom_histogram(aes(x=range_t, fill=group), bins=50)+
  facet_wrap(~group)

ggplot(nb_range_last_year[((directional_speed %in% c(0) & species_evo_type==1) |
                             (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                             (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7))) &
                            V_range<=100 & V_L=="Debiased_Maximum_Monthly_Temperature"])+
  geom_histogram(aes(x=V_range), bins=50)+
  xlim(0, 100)+
  facet_grid(species_evo_type+directional_speed~da+nb)



df_all<-nb_range_last_year[((directional_speed %in% c(0) & species_evo_type==1) |
                              (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                              (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)))]
unique(nb_range_last_year$V_L)
plot(df_all[V_L=="Debiased_Maximum_Monthly_Temperature"]$V_range,
     df_all[V_L=="Debiased_Minimum_Monthly_Temperature"]$V_range)
df_all_prcp<-df_all[V_L=="Debiased_Maximum_Monthly_Precipitation"]
cols<-c("SP_ID", "V_range", "da", "nb", "species_evo_type",  "directional_speed")
df_all_prcp<-df_all_prcp[, ..cols]
colnames(df_all_prcp)[2]<-"prcp"

df_all_temp<-df_all[V_L=="Debiased_Maximum_Monthly_Temperature"]
cols<-c("SP_ID", "V_range", "da", "nb", "species_evo_type",  "directional_speed")
df_all_temp<-df_all_temp[, ..cols]
colnames(df_all_temp)[2]<-"temp"

df_all_gg<-merge(df_all_prcp, df_all_temp, 
                 by=c("SP_ID", "da", "nb", "species_evo_type",  "directional_speed"))
df_all_gg$label<-sprintf("%s (%s)",
                         df_all_gg$species_evo_type, 
                         as.character(df_all_gg$directional_speed))

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Here's the stat_
StatBag <- ggproto("Statbag", Stat,
                   compute_group = function(data, scales, prop = 0.5) {
                     
                     #################################
                     #################################
                     # originally from aplpack package, plotting functions removed
                     plothulls_ <- function(x, y, fraction, n.hull = 1,
                                            col.hull, lty.hull, lwd.hull, density=0, ...){
                       # function for data peeling:
                       # x,y : data
                       # fraction.in.inner.hull : max percentage of points within the hull to be drawn
                       # n.hull : number of hulls to be plotted (if there is no fractiion argument)
                       # col.hull, lty.hull, lwd.hull : style of hull line
                       # plotting bits have been removed, BM 160321
                       # pw 130524
                       if(ncol(x) == 2){ y <- x[,2]; x <- x[,1] }
                       n <- length(x)
                       if(!missing(fraction)) { # find special hull
                         n.hull <- 1
                         if(missing(col.hull)) col.hull <- 1
                         if(missing(lty.hull)) lty.hull <- 1
                         if(missing(lwd.hull)) lwd.hull <- 1
                         x.old <- x; y.old <- y
                         idx <- chull(x,y); x.hull <- x[idx]; y.hull <- y[idx]
                         for( i in 1:(length(x)/3)){
                           x <- x[-idx]; y <- y[-idx]
                           if( (length(x)/n) < fraction ){
                             return(cbind(x.hull,y.hull))
                           }
                           idx <- chull(x,y); x.hull <- x[idx]; y.hull <- y[idx];
                         }
                       }
                       if(missing(col.hull)) col.hull <- 1:n.hull
                       if(length(col.hull)) col.hull <- rep(col.hull,n.hull)
                       if(missing(lty.hull)) lty.hull <- 1:n.hull
                       if(length(lty.hull)) lty.hull <- rep(lty.hull,n.hull)
                       if(missing(lwd.hull)) lwd.hull <- 1
                       if(length(lwd.hull)) lwd.hull <- rep(lwd.hull,n.hull)
                       result <- NULL
                       for( i in 1:n.hull){
                         idx <- chull(x,y); x.hull <- x[idx]; y.hull <- y[idx]
                         result <- c(result, list( cbind(x.hull,y.hull) ))
                         x <- x[-idx]; y <- y[-idx]
                         if(0 == length(x)) return(result)
                       }
                       result
                     } # end of definition of plothulls
                     #################################
                     
                     
                     # prepare data to go into function below
                     the_matrix <- matrix(data = c(data$x, data$y), ncol = 2)
                     
                     # get data out of function as df with names
                     setNames(data.frame(plothulls_(the_matrix, fraction = prop)), nm = c("x", "y"))
                     # how can we get the hull and loop vertices passed on also?
                   },
                   
                   required_aes = c("x", "y")
)

# Here's the stat_ function
#' @inheritParams ggplot2::stat_identity
#' @param prop Proportion of all the points to be included in the bag (default is 0.5)
stat_bag <- function(mapping = NULL, data = NULL, geom = "polygon",
                     position = "identity", na.rm = FALSE, show.legend = NA, 
                     inherit.aes = TRUE, prop = 0.5, alpha = 0.3, ...) {
  layer(
    stat = StatBag, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, prop = prop, alpha = alpha, ...)
  )
}

# here's the geom_
geom_bag <- function(mapping = NULL, data = NULL,
                     stat = "identity", position = "identity",
                     prop = 0.5, 
                     alpha = 0.3,
                     ...,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBag,
    geom = GeomBag,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      alpha = alpha,
      prop = prop,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBag <- ggproto("GeomBag", Geom,
                   draw_group = function(data, panel_scales, coord) {
                     n <- nrow(data)
                     if (n == 1) return(zeroGrob())
                     
                     munched <- coord_munch(coord, data, panel_scales)
                     # Sort by group to make sure that colors, fill, etc. come in same order
                     munched <- munched[order(munched$group), ]
                     
                     # For gpar(), there is one entry per polygon (not one entry per point).
                     # We'll pull the first value from each group, and assume all these values
                     # are the same within each group.
                     first_idx <- !duplicated(munched$group)
                     first_rows <- munched[first_idx, ]
                     
                     ggplot2:::ggname("geom_bag",
                                      grid:::polygonGrob(munched$x, munched$y, default.units = "native",
                                                         id = munched$group,
                                                         gp = grid::gpar(
                                                           col = first_rows$colour,
                                                           fill = alpha(first_rows$fill, first_rows$alpha),
                                                           lwd = first_rows$size * .pt,
                                                           lty = first_rows$linetype
                                                         )
                                      )
                     )
                     
                     
                   },
                   
                   default_aes = aes(colour = "NA", fill = "grey20", size = 0.5, linetype = 1,
                                     alpha = NA, prop = 0.5),
                   
                   handle_na = function(data, params) {
                     data
                   },
                   
                   required_aes = c("x", "y"),
                   
                   draw_key = draw_key_polygon
)

p<-ggplot(data=df_all_gg, aes(x=prcp, y=temp))+
  
  #geom_point(color=label)+
  geom_bag(data=df_all_gg[species_evo_type %in% c(3) & directional_speed==0.1], aes(fill=nb,color=label),
           prop = 0.95)+
  geom_bag(data=df_all_gg[species_evo_type %in% c(3) & directional_speed==0.5], aes(fill=nb,color=label),
           prop = 0.95)+
  geom_bag(data=df_all_gg[species_evo_type %in% c(4) & directional_speed==0.1], aes(fill=nb,color=label),
           prop = 0.95)+
  geom_bag(data=df_all_gg[species_evo_type %in% c(4) & directional_speed==0.5], aes(fill=nb,color=label),
           prop = 0.95)+
  geom_bag(data=df_all_gg[species_evo_type %in% c(6)], aes(fill=nb,color=label),
           prop = 0.95)+
  geom_bag(data=df_all_gg[species_evo_type %in% c(7)], aes(fill=nb,color=label),
           prop = 0.95)+
  geom_point(data=data.frame(prcp=c(5, 10), temp=c(40, 60), nb=c("NARROW", "BROAD")))+
  geom_bag(data=nb_iucn, aes(x=range_p, y=range_t, fill=group),
           prop = 0.95)+
  theme_bw()
ggsave(p, filename="../Figures/Niche_Traits/niche_shift_fn.png", width=10, height=8)
p<-ggplot(data=df_all_gg[species_evo_type %in% c(3, 4, 6, 7) & 
                           directional_speed %in% c(0.01, 0.1, 0.5)], 
          aes(x=prcp, y=temp))+
  
  #geom_point(color=label)+
  geom_bag(aes(fill=nb),
           prop = 0.95, color="grey")+
  geom_bag(data=nb_iucn, aes(x=range_p, y=range_t, fill=group),
           prop = 0.95)+
  facet_wrap(~label, nrow=3, ncol=2)
p

ggsave(p, filename="../Figures/Niche_Traits/niche_shift_fn_panel.png", width=10, height=8)

###fn years
df_fn_year<-readRDS("../Data/niche_traits/nb_range_year_fn.rda")
unique(df_fn_year$V_L)
df_fn_year_gg<-df_fn_year[V_L %in% c("Debiased_Maximum_Monthly_Temperature", "Debiased_Maximum_Monthly_Precipitation")]
df_fn_year_gg[V_L=="Debiased_Maximum_Monthly_Temperature"]$V_L<-"temp"
df_fn_year_gg[V_L=="Debiased_Maximum_Monthly_Precipitation"]$V_L<-"prcp"
df_fn_year_gg$label<-sprintf("%s %s (%s)", df_fn_year_gg$da, df_fn_year_gg$species_evo_type,
                             as.character(df_fn_year_gg$directional_speed))
p<-ggplot(df_fn_year_gg[((directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(3, 4)) |
                        (directional_speed %in% c(0.01) & species_evo_type %in% c(6, 7)))], 
       aes(x=year, y=V_range, linetype=nb, color=label))+
  geom_line()+
  facet_wrap(~V_L, nrow=2, scale="free")+
  theme_bw()
ggsave(p, filename="../Figures/Niche_Traits/niche_shift_fn_years.png", width=10, height=8)

##rn
nb_range_last_year_rn<-readRDS("../Data/niche_traits/niche_traits_last_year_rn.rda")




df_all_rn<-nb_range_last_year_rn[((directional_speed %in% c(0) & species_evo_type==1) |
                              (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                              (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)))]
unique(nb_range_last_year_rn$var)
df_all_prcp_rn<-df_all_rn[var=="PREC"]
cols<-c("sp_id", "range_v", "da", "nb", "species_evo_type",  "directional_speed")
df_all_prcp_rn<-df_all_prcp_rn[, ..cols]
colnames(df_all_prcp_rn)[2]<-"prcp"

df_all_temp_rn<-df_all_rn[var=="TEMP"]
cols<-c("sp_id", "range_v", "da", "nb", "species_evo_type",  "directional_speed")
df_all_temp_rn<-df_all_temp_rn[, ..cols]
colnames(df_all_temp_rn)[2]<-"temp"

df_all_gg_rn<-merge(df_all_temp_rn, df_all_prcp_rn, 
                 by=c("sp_id", "da", "nb", "species_evo_type",  "directional_speed"))
df_all_gg_rn$label<-sprintf("%s (%s)",
                            df_all_gg_rn$species_evo_type, 
                         as.character(df_all_gg_rn$directional_speed))

p<-ggplot(data=df_all_gg_rn, aes(x=prcp, y=temp))+
  #geom_point(color=label)+
  geom_bag(data=df_all_gg_rn[species_evo_type %in% c(1)], aes(fill=nb,color=label),
           prop = 0.95)+
  #geom_point(color=label)+
  geom_bag(data=df_all_gg_rn[species_evo_type %in% c(2) & directional_speed==0.1], aes(fill=nb,color=label),
           prop = 0.95)+
  geom_bag(data=df_all_gg_rn[species_evo_type %in% c(2) & directional_speed==0.5], aes(fill=nb,color=label),
           prop = 0.95)+
  geom_bag(data=df_all_gg_rn[species_evo_type %in% c(3) & directional_speed==0.1], aes(fill=nb,color=label),
           prop = 0.95)+
  geom_bag(data=df_all_gg_rn[species_evo_type %in% c(3) & directional_speed==0.5], aes(fill=nb,color=label),
           prop = 0.95)+
  geom_bag(data=df_all_gg_rn[species_evo_type %in% c(4) & directional_speed==0.1], aes(fill=nb,color=label),
           prop = 0.95)+
  geom_bag(data=df_all_gg_rn[species_evo_type %in% c(4) & directional_speed==0.5], aes(fill=nb,color=label),
           prop = 0.95)+
  geom_bag(data=df_all_gg_rn[species_evo_type %in% c(5)], aes(fill=nb,color=label),
           prop = 0.95)+
  geom_bag(data=df_all_gg_rn[species_evo_type %in% c(6)], aes(fill=nb,color=label),
           prop = 0.95)+
  geom_bag(data=df_all_gg_rn[species_evo_type %in% c(7)], aes(fill=nb,color=label),
           prop = 0.95)+
  geom_point(data=data.frame(prcp=c(5, 10), temp=c(40, 60), nb=c("NARROW", "BROAD")))+
  geom_bag(data=nb_iucn, aes(x=range_p, y=range_t, fill=group),
           prop = 0.95)+
  theme_bw()
p
ggsave(p, filename="../Figures/Niche_Traits/niche_shift_rn.png", width=10, height=8)
p<-ggplot(data=df_all_gg_rn[((directional_speed %in% c(0) & species_evo_type==1) |
                            (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                            (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)))], 
          aes(x=prcp, y=temp))+
  
  #geom_point(color=label)+
  geom_bag(aes(fill=nb),
           prop = 0.95, color="grey")+
  geom_bag(data=nb_iucn, aes(x=range_p, y=range_t, fill=group),
           prop = 0.95)+
  facet_wrap(~label, nrow=2, ncol=5)
p

ggsave(p, filename="../Figures/Niche_Traits/niche_shift_rn_panel.png", width=10, height=4)


df_rn_year<-readRDS("../Data/niche_traits/nb_range_year_rn.rda")
unique(df_rn_year$var)
df_rn_year_gg<-df_rn_year[var %in% c("TEMP", "PREC")]
df_rn_year_gg[var=="TEMP"]$var<-"temp"
df_rn_year_gg[var=="PREC"]$var<-"prcp"
df_rn_year_gg$label<-sprintf("%s %s (%s)", df_rn_year_gg$da, df_rn_year_gg$species_evo_type,
                             as.character(df_rn_year_gg$directional_speed))
p<-ggplot(df_rn_year_gg[((directional_speed %in% c(0) & species_evo_type==1) |
                           (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                           (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)))], 
          aes(x=year * -0.1, y=V_range, linetype=nb, color=label))+
  geom_line()+
  facet_wrap(~var, nrow=2, scale="free")+
  theme_bw()
p
ggsave(p, filename="../Figures/Niche_Traits/niche_shift_rn_years.png", width=10, height=8)

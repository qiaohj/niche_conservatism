library(sf)
library(dggridR)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
dggridR::dg_closest_res(dggs, col="spacing_km", val=100)
dggs<-dggridR::dgconstruct(res = 8)
earth<-dgearthgrid(dggs)
shpfname = "../Shape/isea3h8/isea3h8_sf.shp"
write_sf(earth, shpfname)

x<-c(-180, -180, 180, 180, -180)
y<-c(-90, 90, 90, -90, -90)
coords<-cbind(x, y)
box<-st_sfc(st_polygon(list(coords)), crs=4326)
box<-  st_as_sfc(box)

grid<-read_sf("../Shape/isea3h8/isea3h8.kml")
area<-st_area(grid)
plot(grid)

st_crs(st_read(shpfname))

shp<-dgshptogrid(dggs, box)
plot(shp)



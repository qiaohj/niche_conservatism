library(ncdf4)
ncin <- nc_open("/home/huijieqiao/git/ariane-2.3.0_02/examples/qualitative/ariane_trajectories_qualitative.nc")
print(ncin)
ncin$var

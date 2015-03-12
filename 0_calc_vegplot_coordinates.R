library(rgdal)
library(foreach)
library(tools)
library(maptools)

su_shp_dir <- "Clean_SamplingUnit_Shps"

su_shps <- dir(su_shp_dir, pattern=".shp$")

vg_pts <- foreach(su_shp=su_shps, .combine=spRbind) %do% {
    pts <- readOGR(su_shp_dir, file_path_sans_ext(su_shp))
    pts <- pts[pts$Code == "VG", ]
    return(pts)
}

# Pasoh plot coordinates are wrong. Add these coordinates in manually.
psh_pts_data <- data.frame(Unit_ID=paste0("VGPSH", 1:9),
                           Code="VG",
                           sitecode="PSH",
                           protocol="VG",
                           number=1:9)
psh_pts_x <- c(102.30661, 102.33218, 102.33183, 101.97898, 101.97989, 101.97735, 102.31609, 102.34319, 102.2899)
psh_pts_y <- c(2.98011, 3.05078, 3.0985, 3.11016, 3.19218, 3.1628, 2.9978, 2.95985, 3.09306)
psh_pts <- SpatialPointsDataFrame(coords=cbind(psh_pts_x, psh_pts_y), data=psh_pts_data, 
                                  proj4string=CRS(proj4string(vg_pts)))

vg_pts <- vg_pts[vg_pts$sitecode != "PSH", ]
vg_pts <- spRbind(vg_pts, psh_pts)


save(vg_pts, file='vg_pts.RData')
writeOGR(vg_pts, ".", "vg_pts",driver='ESRI Shapefile', overwrite=TRUE)

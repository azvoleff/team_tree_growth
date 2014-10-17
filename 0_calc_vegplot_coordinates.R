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

# Plots 1-3 are missing for Pasoh. Add these coordinates in manually.
psh_pts_data <- data.frame(Unit_ID=c("VGPSH1","VGPSH2", "VGPSH3"),
                           Code="VG",
                           sitecode="PSH",
                           protocol="VG",
                           number=c(1, 2, 3))
psh_pts_x <- c(102.18362, 102.33218, 102.33264)
psh_pts_y <- c(2.58797, 3.05078, 3.09845)
psh_pts <- SpatialPointsDataFrame(coords=cbind(psh_pts_x, psh_pts_y), data=psh_pts_data, 
                                  proj4string=CRS(proj4string(vg_pts)))

vg_pts <- spRbind(vg_pts, psh_pts)


save(vg_pts, file='vg_pts.RData')
writeOGR(vg_pts, ".", "vg_pts",driver='ESRI Shapefile')

library(rgdal)
library(tools)
library(plyr)
library(stringr)

shapefiles <- dir('Sampling_Units', pattern='.shp$')

dirs <- rep('Sampling_Units', length(shapefiles))
layers <- gsub('.shp$', '', shapefiles)

su <- mapply(readOGR, dirs, layers)

# One site has the variables names are in all caps:
lapply(su, names)
# So ensure the variable names all match:
su <- lapply(su, FUN=function(sampling_unit) {
       names(sampling_unit) <- c('Unit_ID', 'Code', 'Lat', 'Long')
       sampling_unit
})

su <- ldply(su, data.frame)
su <- su[-1]

su$sitecode <- gsub('(^CT)|(^CL)|(^VG)|([0-9]{1,3}([.][0-9])?$)', 
                                '', su$Unit_ID)
su$protocol <- str_extract(su$Unit_ID, '(^CT)|(^CL)|(^VG)')
su$number <- str_extract(su$Unit_ID, '[0-9]{1,3}([.][0-9])?$')

ddply(su, .(sitecode, protocol), summarize,
      num_plots=length(sitecode))

vg_pts <- su[su$protocol == 'VG', ]
vg_pts <- SpatialPointsDataFrame(cbind(vg_pts$Long, vg_pts$Lat), vg_pts, 
                                 proj4string=CRS('+init=epsg:4326'))
save(vg_pts, file='vg_pts.RData')

cl_pts <- su[su$protocol == 'CL', ]
cl_pts <- SpatialPointsDataFrame(cbind(cl_pts$Long, cl_pts$Lat), cl_pts,
                                 proj4string=CRS('+init=epsg:4326'))
save(cl_pts, file='cl_pts.RData')

ct_pts <- su[su$protocol == 'CT', ]
ct_pts <- SpatialPointsDataFrame(cbind(ct_pts$Long, ct_pts$Lat), ct_pts,
                                 proj4string=CRS('+init=epsg:4326'))
save(ct_pts, file='ct_pts.RData')

su_spdf <- SpatialPointsDataFrame(cbind(su$Long, su$Lat), su,
                                        proj4string=CRS('+init=epsg:4326'))
cols_to_drop <- names(su_spdf) %in% c('Lat', 'Long', 'coords.x1', 'coords.x2')
su_spdf <- su_spdf[!cols_to_drop]

for (this_sitecode in unique(su$sitecode)) {
    these_su <- su_spdf[which(su_spdf$sitecode == this_sitecode), ]
    writeOGR(these_su, "Clean_SamplingUnit_Shps",
             paste0(this_sitecode, "_", "TEAM_Sampling_Points"),
             driver="ESRI Shapefile")
}

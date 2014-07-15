library(rgdal)
library(tools)
library(plyr)
library(stringr)

shapefiles <- dir('Sampling_Units', pattern='.shp$')

dirs <- rep('Sampling_Units', length(shapefiles))
layers <- gsub('.shp$', '', shapefiles)

sampling_units <- mapply(readOGR, dirs, layers)

# One site has the variables names are in all caps:
lapply(sampling_units, names)
# So ensure the variable names all match:
sampling_units <- lapply(sampling_units, FUN=function(sampling_unit) {
       names(sampling_unit) <- c('Unit_ID', 'Code', 'Lat', 'Long')
       sampling_unit
})

sampling_units <- ldply(sampling_units, data.frame)
sampling_units <- sampling_units[-1]

sampling_units$sitecode <- gsub('(^CT)|(^CL)|(^VG)|([0-9]{1,3}([.][0-9])?$)', 
                                '', sampling_units$Unit_ID)
sampling_units$protocol <- str_extract(sampling_units$Unit_ID, '(^CT)|(^CL)|(^VG)')
sampling_units$number <- str_extract(sampling_units$Unit_ID, '[0-9]{1,3}([.][0-9])?$')

ddply(sampling_units, .(sitecode, protocol), summarize,
      num_plots=length(sitecode))

vg_pts <- sampling_units[sampling_units$protocol == 'VG', ]
vg_pts <- SpatialPointsDataFrame(cbind(vg_pts$Long, vg_pts$Lat), vg_pts, 
                                 proj4string=CRS('+init=epsg:4326'))
save(vg_pts, file='vg_pts.RData')

cl_pts <- sampling_units[sampling_units$protocol == 'CL', ]
cl_pts <- SpatialPointsDataFrame(cbind(cl_pts$Long, cl_pts$Lat), cl_pts,
                                 proj4string=CRS('+init=epsg:4326'))
save(cl_pts, file='cl_pts.RData')

ct_pts <- sampling_units[sampling_units$protocol == 'CT', ]
ct_pts <- SpatialPointsDataFrame(cbind(ct_pts$Long, ct_pts$Lat), ct_pts,
                                 proj4string=CRS('+init=epsg:4326'))
save(ct_pts, file='ct_pts.RData')

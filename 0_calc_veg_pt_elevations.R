library(dplyr)
library(raster)
library(gdalUtils)
library(rgdal)
library(rgeos)
library(foreach)
library(doParallel)

sitecode_key <- read.csv('H:/Data/TEAM/Sitecode_Key/sitecode_key.csv')
sitecodes <- unique(sitecode_key$sitecode)

cl <- makeCluster(4)
registerDoParallel(cl)

prefixes <- c('D:/azvoleff/Data', # CI-TEAM
              'H:/Data', # Buffalo drive
              'O:/Data', # Blue drive
              '/localdisk/home/azvoleff/Data') # vertica1
prefix <- prefixes[match(TRUE, unlist(lapply(prefixes, function(x) file_test('-d', x))))]

load('vg_pts.RData')
sitecodes <- as.character(sitecodes[sitecodes %in% vg_pts$sitecode])

vg_pts_elev <- foreach(sitecode=sitecodes,
                       .packages=c("rgdal", "raster", "sp", "rgeos", 
                                   "gdalUtils"),
                       .inorder=FALSE, .combine=rbind)  %dopar% {
    # Load the DEM extents needed for the auto_setup_dem function
    load('dem_extents.RData')
    dem_path <- file.path(prefix, 'CGIAR_SRTM', 'Tiles')
    dem_extents$filename <- gsub('H:\\\\Data\\\\CGIAR_SRTM', dem_path, dem_extents$filename)
    dem_extents$filename <- gsub('\\\\', '/', dem_extents$filename)

    these_vg_pts <- vg_pts[vg_pts$sitecode==sitecode, ]

    these_vg_pts <- spTransform(these_vg_pts, CRS(proj4string(dem_extents)))
    intersecting <- as.logical(gIntersects(dem_extents, gConvexHull(these_vg_pts), byid=TRUE))

    if (sum(intersecting) == 0) {
        stop('no intersecting dem extents found')
    } else {
        dem_extents <- dem_extents[intersecting, ]
    }

    dem_list <- dem_extents$filename
    dem_rasts <- lapply(dem_list, raster)

    if (length(dem_list) > 1) {
        dem_prj <- projection(dem_rasts[[1]])
        if (any(lapply(dem_rasts, projection) != dem_prj)) {
            stop("each DEM in dem_list must have the same projection")
        }
        mosaic_file <- extension(rasterTmpFile(), '.tif')
        # Calculate minimum bounding box coordinates:
        mosaic_te <- as.numeric(bbox(these_vg_pts))
        # Expand bbox slightly:
        mosaic_te[1] <- mosaic_te[1] - .05
        mosaic_te[2] <- mosaic_te[2] - .05
        mosaic_te[3] <- mosaic_te[3] + .05
        mosaic_te[4] <- mosaic_te[4] + .05
        # Use mosaic_rasters from gdalUtils for speed:
        mosaic_rasters(dem_list, mosaic_file, te=mosaic_te, of="GTiff", 
                       overwrite=TRUE, ot='Int16')
        dem_mosaic <- raster(mosaic_file)
    } else {
        dem_mosaic <- dem_rasts[[1]]
        mosaic_file <- filename(dem_mosaic)
    }

    these_vg_pts_elev <- extract(dem_mosaic, these_vg_pts)
    return(data.frame(sitecode=sitecode, 
                      plot_ID=as.character(these_vg_pts@data$Unit_ID), 
                      elev_m=these_vg_pts_elev))
}
save(vg_pts_elev, file="vg_pts_elev.RData")

load(file="vg_pts_elev.RData")
library(ggplot2)
ggplot(vg_pts_elev, aes(elev_m, fill=sitecode)) + geom_bar()
ggsave('veg_point_elevation_hists.png')

elev_by_site <- group_by(vg_pts_elev, sitecode) %>%
    summarise(elev_max=max(elev_m),
              elev_min=min(elev_m),
              elev_range=elev_max - elev_min)
vg_pts_elev$sitecode <- ordered(vg_pts_elev$sitecode,
                                levels=elev_by_site$sitecode[order(elev_by_site$elev_max)])
ggplot(vg_pts_elev, aes(sitecode, elev_m)) +
    geom_point() +
    xlab("Site") + ylab("Elevation (m)")
ggsave('veg_point_elevation_hists_bysite.png')

table(is.na(vg_pts_elev$elev_m))

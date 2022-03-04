# render historical georeferenced map with elev data
# specifically usgs topos

library(tidyverse)
library(rayshader)
library(raster)
source("utilities_ray.r")

geoTIFF_name <- "data/lone_mountain.tif"
map_img <- raster::stack(geoTIFF_name)
full_extent <- extent(map_img)
crs(map_img)
newcrs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#reduce size for quicker plotting
small_ras <- raster::aggregate(map_img,fact=3)
small_ras <- projectRaster(small_ras,crs = newcrs)
full_extent <- extent(small_ras)

hillshade_img <- as.array(small_ras)
rgb_scaler <- max(hillshade_img,na.rm = TRUE)
hillshade_img <- hillshade_img/rgb_scaler
# -------------------------------------------------------------
# Download external elevation data
# get elevation data


image_size <- define_image_size(extent_to_bbox(full_extent), major_dim = 600)

elev_file = "data/spanish_peaks_topo.tif"
get_usgs_elevation_data(extent_to_bbox(small_ras), 
                        size = image_size$size, file = elev_file,
                        sr_bbox = 4326, sr_image = 4326)


fudge_fact <- -0.0002
borderless_extent <- extent(c(-111.5+fudge_fact,
                              -dms_to_dec(111,22,30)+fudge_fact,
                              45.25+fudge_fact,
                              dms_to_dec(45,22,30)+fudge_fact))
elev_img <- raster::raster(elev_file)
# change extent to visible map and crop
elev_img <- crop(elev_img,borderless_extent)
elev_img <- raster::extend(elev_img,full_extent,value = 1900)

elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)


#elev_matrix <- elev_matrix %>% 
#  zero_out_border(full_extent,borderless_extent)

zscale = 30

#ambmat <- ambient_shade(elev_matrix, zscale = zscale,
#                        multicore = TRUE)
raymat <- ray_shade(elev_matrix, sunaltitude = 45,zscale = zscale, 
                    lambert = TRUE,
                    multicore = TRUE)

# show one view
#hillshade_img %>% 
#  add_overlay(array_img, alphalayer = 1.0) %>%
#  add_shadow(raymat, max_darken = 0.9) %>%
##  add_shadow(ambmat, max_darken = 0.5) %>%
#  plot_map()


#Plot in 3D
rgl::clear3d()
hillshade_img %>%
  add_shadow(raymat,0.3) %>%
#  add_shadow(ambmat,0) %>%
  plot_3d(elev_matrix,zscale=zscale,zoom = .5)

#rgl::close3d()
#Render snapshot with depth of field
#render_depth(focus=0.982,focallength = 4000)




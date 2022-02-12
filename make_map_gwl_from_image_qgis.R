# render historical georeferenced map with elev data
# specifically usgs topos

library(tidyverse)
library(rayshader)
library(raster)
source("utilities_ray.r")

geoTIFF_name <- "img/Greenwood Lake-1910_modified.tif"
#geoTIFF_name <- "data/India_modified.tif"
map_img <- raster::stack(geoTIFF_name)
extent(map_img)

# change extent to visible map and crop
new_extent <- extent(c(-74.5,-74.25,41.00,41.25))

# #debug
# new_extent <- extent(map_img)

#reduce size for quicker plotting
small_ras <- raster::aggregate(map_img,fact=5)
cropped_array <- crop(small_ras,new_extent) %>% as.array()

hillshade_img <- as.array(cropped_array/255)



# -------------------------------------------------------------
# Download external elevation data
# get elevation data


image_size <- define_image_size(extent_to_bbox(cropped_img), major_dim = 600)



elev_file = "data/gwl.tif"
get_usgs_elevation_data(extent_to_bbox(cropped_img), 
                        size = image_size$size, file = elev_file,
                        sr_bbox = 4326, sr_image = 4326)
elev_img <- raster::raster(elev_file)
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

zscale = 20

ambmat <- ambient_shade(elev_matrix, zscale = zscale,
                        multicore = TRUE)
raymat <- ray_shade(elev_matrix, sunaltitude = 45,zscale = zscale, 
                    lambert = TRUE,
                    multicore = TRUE)

# show one view
hillshade_img %>% 
#  add_overlay(array_img, alphalayer = 1.0) %>%
  add_shadow(raymat, max_darken = 0.9) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  plot_map()


#Plot in 3D
hillshade_img %>%
  add_shadow(raymat,0.3) %>%
  add_shadow(ambmat,0) %>%
  plot_3d(elev_matrix,zscale=zscale,zoom = .2)

rgl::close3d()
#Render snapshot with depth of field
render_depth(focus=0.982,focallength = 4000)

#Plot in 2D
img %>%
  add_shadow(ray_layer,0.3) %>%
  add_shadow(ambient_layer,0) %>%
  plot_map()



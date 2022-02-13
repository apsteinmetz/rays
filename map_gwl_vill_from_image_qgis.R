# render historical georeferenced map with elev data
# specifically usgs topos

#get sun angles


library(tidyverse)
library(rayshader)
library(raster)
library(suncalc)

source("utilities_ray.r")

geoTIFF_name <- "img/Greenwood Lake-1910_modified.tif"
map_img <- raster::stack(geoTIFF_name)
full_extent <- extent(map_img)

sun_positions <- 7:17 %>% map(function(h) 
  getSunlightPosition(date = as.POSIXct(glue::glue("2017-11-12 {h}:00:00",tz = "EST")),
                      lat=41,lon = -74)) %>% 
  bind_rows() %>% 
  transmute(altitude = altitude *180/pi,
         azimuth = azimuth *180/pi) %>%
  mutate(azimuth = ifelse(azimuth >= 0,azimuth + 180, 180 + azimuth))
         
#reduce size for quicker plotting
small_ras <- raster::aggregate(map_img,fact=5)

# change extent to visible map and crop
borderless_extent <- extent(c(-74.5,-74.25,41.00,41.25))
village_extent <- extent(c(-74.33,-74.25,41.16,41.25))
#cropped_ras <- crop(small_ras,borderless_extent)
cropped_small_ras <- crop(small_ras,village_extent)
cropped_ras <- crop(map_img,village_extent)

hillshade_img <- as.array(cropped_ras/255)
#hillshade_img <- as.array(small_ras/255)

# -------------------------------------------------------------
# Download external elevation data
# get elevation data


image_size <- define_image_size(extent_to_bbox(village_extent), major_dim = 200)

elev_file = "data/gwl_vill.tif"
get_usgs_elevation_data(extent_to_bbox(cropped_ras), 
                        size = image_size$size, file = elev_file,
                        sr_bbox = 4326, sr_image = 4326)

elev_img <- raster::raster(elev_file)
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

#elev_matrix <- elev_matrix %>% 
#  zero_out_border(full_extent,borderless_extent)

zscale = 20

#ambmat <- ambient_shade(elev_matrix, 
#                        zscale = zscale,
#                        multicore = TRUE)
raymat <- ray_shade(elev_matrix, 
                    sunaltitude = 35,
                    sunangle = 90,
                    zscale = zscale, 
                    lambert = TRUE,
                    multicore = TRUE)

# show one view
hillshade_img %>% 
#  add_overlay(array_img, alphalayer = 1.0) %>%
  add_shadow(raymat, max_darken = 0.0) %>%
#  add_shadow(ambmat, max_darken = 0.5) %>%
  plot_map()


#Plot in 3D
hillshade_img %>%
  add_shadow(raymat,max_darken = 0.0) %>%
#  add_shadow(ambmat,0) %>%
  plot_3d(elev_matrix,zscale=zscale,zoom = 0.5)

rgl::close3d()




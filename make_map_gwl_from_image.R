library(raster)
# devtools::install_github("tylermorganwall/rayshader")
library(rayshader)
library(tidyverse)
library(rgdal)
library(sf)
library(leaflet)
source("utilities_ray.r")
library(png)
library(jpeg)


major_dim <- 600
zscale = 20

# bounding box from historical topo map extent
nw <- list(lat = dms_to_dec(41,15),long=-dms_to_dec(74,30))
se <- list(lat = dms_to_dec(41),long=-dms_to_dec(74,15))
ne <-list(lat = nw$lat,long = se$long)
sw <-list(lat = se$lat,long = nw$long)
bbox_map <- list(
  p1 = nw,
  p2 = se
)
# bounding box from historical topo includes border
nw <- list(lat = dms_to_dec(41,16,13),long=-dms_to_dec(74,30,20))
se <- list(lat = dms_to_dec(40,58,52),long=-dms_to_dec(74,14,4))
ne <-list(lat = nw$lat,long = se$long)
sw <-list(lat = se$lat,long = nw$long)
bbox_full <- list(
  p1 = nw,
  p2 = se
)


bbox <- bbox_full
image_size <- define_image_size(bbox, major_dim = 600)

# view bounding box.
# just to confirm extent
leaflet() %>%
  addTiles() %>% 
  addRectangles(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
    lng2 = bbox$p2$long, lat2 = bbox$p2$lat,
    fillColor = "transparent"
  ) %>%
  fitBounds(
    lng1 = bbox_map$p1$long, lat1 = bbox_map$p1$lat,
    lng2 = bbox_map$p2$long, lat2 = bbox_map$p2$lat
  )


# # convert a lat/long to pixels on an image
# house_pos <- find_image_coordinates(latlon_house$long,
#                                     latlon_house$lat,
#                                     bbox = bbox,
#                                     image_width=image_size$width,
#                                     image_height=image_size$height)
# 
# ranch_pos <- find_image_coordinates(latlon_ranch$long,
#                                     latlon_ranch$lat,
#                                     bbox = bbox,
#                                     image_width=image_size$width,
#                                     image_height=image_size$height)

# -------------------------------------------------------------
# Download external data

elev_file = "data/gwl.tif"
get_usgs_elevation_data(bbox, size = image_size$size, file = elev_file,
                        sr_bbox = 4326, sr_image = 4326)

# -------------------------------------------------------------

#load overlays
overlay_file <- "img/greenwood lake-1910s.jpg"
overlay_img <- jpeg::readJPEG(overlay_file)
# load elevation data
elev_img <- raster::raster(elev_file)
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

#make flat border according to map border
elev_matrix <- make_elev_matrix_border(elev_matrix,bbox_full,bbox_map)

zscale = 20

# calculate rayshader layers
ambmat <- ambient_shade(elev_matrix, zscale = zscale,
                          multicore = TRUE)
raymat <- ray_shade(elev_matrix, sunaltitude = 45,zscale = zscale, 
                    lambert = TRUE,
                    multicore = TRUE)

# show one view
elev_matrix %>%
  sphere_shade(texture = "bw", zscale = zscale) %>%
  add_overlay(overlay_img, alphalayer = 1.0) %>%
  add_shadow(raymat, max_darken = 0.9) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  plot_map()


rgl::clear3d()
elev_matrix %>% 
  sphere_shade(texture = "bw") %>% 
  add_overlay(overlay_img, alphalayer = 1.0) %>%
  add_shadow(raymat, max_darken = 0.6) %>%
  add_shadow(ambmat, max_darken = 0.6) %>%
  plot_3d(elev_matrix, zscale = zscale, windowsize = c(1200, 1000),
          water = FALSE, soliddepth = "auto", wateralpha = 0,
          theta = 0, phi = 30, zoom = 0.5, fov = 60)

render_snapshot()
frame_count = 0
azimuth = 90
for (zoom in 80:30){
#  render_camera(phi = 90, theta = 0,zoom = zoom/100 ,fov=80)
  frame_count <- frame_count + 1
#  render_snapshot(paste0("frames/denison",str_pad(frame_count,3,pad="0"),".png"))
}

for (azimuth in 90:15){
  render_camera(phi = azimuth, theta = 0,zoom = 0.3,fov=80)
  frame_count <- frame_count + 1
  render_snapshot(paste0("frames/denison",str_pad(frame_count,3,pad="0"),".png"))
}


for (theta in 1:360){
  render_camera(phi = 15, theta = theta,zoom = 0.3,fov=80)
  frame_count <- frame_count + 1
  render_snapshot(paste0("frames/denison",str_pad(frame_count,3,pad="0"),".png"))
}

#Run this command in the command line using ffmpeg to stitch together a video:
# using frame file name instead of "frame"
#ffmpeg -framerate 60 -i "frame%03d.png" -vcodec libx264 raymovie.mp4

#And run this command to convert the video to post to the web:
#ffmpeg -i raymovie.mp4 -pix_fmt yuv420p -profile:v baseline -level 3 -vf scale=-2:-2 rayweb.mp4

rgl::close3d()

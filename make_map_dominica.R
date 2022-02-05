library(raster)
# devtools::install_github("tylermorganwall/rayshader")
library(rayshader)
library(tidyverse)
library(rgdal)
library(sf)
library(leaflet)
source("utilities_ray.r")
library(png)


#boiling lake
# https://www.google.com/maps/place/Boiling+Lake/@15.3183146,-61.294805,421m
latlon_mid <- list(lat=15.3183146,long=-61.294805)

lat_width <- 0.015
lon_width <- 0.015
major_dim <- 1200
# texture from http://matthewkling.github.io/media/rayshader/
texture <- create_texture("red", "darkgreen",
                          "khaki", "khaki", "khaki")

bbox <- list(
  p1 = list(long=latlon_mid$long-lon_width, lat = latlon_mid$lat+lat_width),
  p2 = list(long=latlon_mid$long+lon_width, lat = latlon_mid$lat-lat_width)
)

# view bounding box.
leaflet() %>%
  addTiles() %>% 
  addRectangles(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
    lng2 = bbox$p2$long, lat2 = bbox$p2$lat,
    fillColor = "transparent"
  ) %>%
  fitBounds(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
    lng2 = bbox$p2$long, lat2 = bbox$p2$lat
  )


image_size <- define_image_size(bbox, major_dim)

lake_pos <- latlon_mid
# valley of desolation
valley_pos <- list(lat=15.312906,long=-61.301114)

# load elevation data
# http://charim-geonode.net/layers/geonode:dem1
# 5 meters/pixel resolution
elev_file <- file.path("data", "dominica_charm.tif")

# overlay_file <- overlay_file <- "dominica_historical.png"
overlay_file <- "dominica_sat.png"
overlay_file2 <- "dominica_street.png"

# -------------------------------------------------------------
# Download external data

# get_usgs_elevation_data(bbox, size = image_size$size, 
#                         file = file.path("data","elev_usgs_dominica.tif"),
#                         sr_bbox = 4326, sr_image = 4326)
# 
# plot 2D# fetch overlay image
get_arcgis_map_image(bbox, map_type = "World_Imagery", file = 
                      file.path("img",overlay_file),
                      width = image_size$width, height = image_size$height, 
                      sr_bbox = 4326)
overlay_img <- png::readPNG(file.path("img",overlay_file))

# # plot 2D# fetch overlay image streets
# get_arcgis_map_image(bbox, map_type = "World_Street_Map", 
#                      file = file.path("img",overlay_file2),
#                      width = image_size$width, height = image_size$height, 
#                      sr_bbox = 4326)
# overlay_img2 <- png::readPNG(file.path("img",overlay_file2))

# -------------------------------------------------------------

# load elevation data
elev_img <- raster::raster(elev_file) %>% 
  crop_img(bbox)
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

zscale = 3
zoom = 0.5

# calculate rayshader layers
ambmat <- ambient_shade(elev_matrix, zscale = zscale)
# raymat <- ray_shade(elev_matrix,anglebreaks=seq(20,30,1),zscale = zscale, lambert = TRUE)
raymat <- ray_shade(elev_matrix, sunaltitude = 45,zscale = zscale, lambert = TRUE)
# watermap <- detect_water(elev_matrix)


# show one view
elev_matrix %>%
  sphere_shade(texture = "bw", zscale = zscale) %>%
  # add_water(watermap, color = "imhof4") %>%
#  add_shadow(raymat, max_darken = 0.9) %>%
#  add_shadow(ambmat, max_darken = 0.5) %>%
  add_overlay(overlay_img, alphalayer = 0.7) %>%
#  add_overlay(overlay_img2, alphalayer = 0.7) %>%
  plot_map()


rgl::clear3d()
elev_matrix %>% 
  sphere_shade(texture = "bw") %>% 
  # add_water(watermap, color = "imhof4") %>%
  # add_overlay(overlay_img, alphalayer = 0.9) %>%
  add_overlay(overlay_img, alphalayer = 0.8) %>%
  add_shadow(raymat, max_darken = 0.4) %>%
  add_shadow(ambmat, max_darken = 0.6) %>%
  plot_3d(elev_matrix, zscale = zscale, windowsize = c(1200, 1000),
          water = FALSE, soliddepth = "auto", wateralpha = 0,
          theta = 0, phi = 30, zoom = zoom, fov = 60)

render_label(elev_matrix,text="Boiling Lake",
             lat=lake_pos$lat,long=lake_pos$long, z=NULL,
             extent=extent(elev_img),
             zscale=zscale,relativez=FALSE,freetype = FALSE)

render_label(elev_matrix,text="Valley of Desolation",
             lat=valley_pos$lat,long=valley_pos$long, z=NULL,
             extent=extent(elev_img),
             zscale=zscale,relativez=FALSE,freetype = FALSE)
render_snapshot()

frame_count = 0
start_azimuth = 90
start_theta = 0
# zoom in
for (zoom in 80:50){
#  render_camera(phi = 90, theta = start_theta,zoom = zoom/100 ,fov=80)
  frame_count <- frame_count + 1
#  render_snapshot(paste0("frames/dominica",str_pad(frame_count,3,pad="0"),".png"))
}

for (azimuth in start_azimuth:45){
  render_camera(phi = azimuth, theta = start_theta,zoom = zoom/100,fov=80)
  frame_count <- frame_count + 1
  render_snapshot(paste0("frames/dominica",str_pad(frame_count,3,pad="0"),".png"))
}

# spin 360
for (theta in start_theta+1:360){
  render_camera(phi = azimuth, theta = theta,zoom = zoom/100,fov=80)
  frame_count <- frame_count + 1
  render_snapshot(paste0("frames/dominica",str_pad(frame_count,3,pad="0"),".png"))
}

# rgl::close3d()

#Run this command in the command line using ffmpeg to stitch together a video:
# using frame file name instead of "frame"
# and index number of first frame instead of nnn
#ffmpeg -framerate 60 =start_number nnn -i "frame%03d.png" -vcodec libx264 raymovie.mp4

#And run this command to convert the video to post to the web:
#ffmpeg -i raymovie.mp4 -pix_fmt yuv420p -profile:v baseline -level 3 -vf scale=-2:-2 rayweb.mp4


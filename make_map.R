library(raster)
# devtools::install_github("tylermorganwall/rayshader")
library(rayshader)
library(tidyverse)
library(rgdal)
library(sf)
library(leaflet)
source("utilities_ray.r")
library(png)

latlon_alder<-list(lat=38.374304,long=-106.1165398)
latlon_salida<-list(lat=38.53,long=-105.9922354)
latlon_mid <- list(lat=38.442294,long= -106.033139)
latlon_house <- list(lat=38.513622,long=-106.0157)
latlon_ranch <- list(lat=38.38096,long=-106.0711)
latlon_denison <- list(lat=40.0711479,long=-82.5225355)


latlon_mid <- latlon_denison

lat_width <- 0.007
lon_width <- 0.007
major_dim <- 1200
zscale = 20
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


image_size <- define_image_size(bbox, major_dim = 1200)

house_pos <- find_image_coordinates(latlon_house$long,
                                    latlon_house$lat,
                                    bbox = bbox,
                                    image_width=image_size$width,
                                    image_height=image_size$height)

ranch_pos <- find_image_coordinates(latlon_ranch$long,
                                    latlon_ranch$lat,
                                    bbox = bbox,
                                    image_width=image_size$width,
                                    image_height=image_size$height)

elev_file <- file.path("data", "denison.tif")
overlay_file <- "img/denison_map.png"
overlay_file2 <- "img/denison_map2.png"
# -------------------------------------------------------------
# Download external data

get_usgs_elevation_data(bbox, size = image_size$size, file = elev_file,
                        sr_bbox = 4326, sr_image = 4326)

# plot 2D# fetch overlay image
get_arcgis_map_image(bbox, map_type = "World_Imagery", file = overlay_file,
                     width = image_size$width, height = image_size$height, 
                     sr_bbox = 4326)

# plot 2D# fetch overlay image
get_arcgis_map_image(bbox, map_type = "World_Street_Map", file = overlay_file2,
                     width = image_size$width, height = image_size$height, 
                     sr_bbox = 4326)

# -------------------------------------------------------------

#load overlays
overlay_img2 <- png::readPNG(overlay_file2)
overlay_img <- png::readPNG(overlay_file)
# load elevation data
elev_img <- raster::raster(elev_file)
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

zscale = 1

# calculate rayshader layers
ambmat <- ambient_shade(elev_matrix, zscale = zscale)
# raymat <- ray_shade(elev_matrix,anglebreaks=seq(20,30,1),zscale = zscale, lambert = TRUE)
raymat <- ray_shade(elev_matrix, sunaltitude = 45,zscale = zscale, lambert = TRUE)
# watermap <- detect_water(elev_matrix)


# show one view
elev_matrix %>%
  sphere_shade(texture = "bw", zscale = zscale) %>%
  # add_water(watermap, color = "imhof4") %>%
  add_shadow(raymat, max_darken = 0.9) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
#  add_overlay(overlay_img, alphalayer = 0.7) %>%
  add_overlay(overlay_img2, alphalayer = 0.7) %>%
  plot_map()


rgl::clear3d()
elev_matrix %>% 
  sphere_shade(texture = "bw") %>% 
  # add_water(watermap, color = "imhof4") %>%
  # add_overlay(overlay_img, alphalayer = 0.9) %>%
  add_overlay(overlay_img2, alphalayer = 0.8) %>%
  add_shadow(raymat, max_darken = 0.4) %>%
  add_shadow(ambmat, max_darken = 0.6) %>%
  plot_3d(elev_matrix, zscale = zscale, windowsize = c(1200, 1000),
          water = FALSE, soliddepth = "auto", wateralpha = 0,
          theta = 0, phi = 30, zoom = 0.5, fov = 60)

#render_label(elev_matrix,text="House",x=house_pos$x,y=house_pos$y,z=4000,zscale=zscale,relativez=FALSE,freetype = FALSE)
#render_label(elev_matrix,text="Ranch",x=ranch_pos$x,y=ranch_pos$y,z=4000,zscale=zscale,relativez=FALSE,freetype = FALSE)
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


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
library(elevatr)

source("utilities_ray.r")

geoTIFF_name <- "data/71st Inf Div Occupation Zone_modified.tif"
map_img <- raster::stack(geoTIFF_name)
full_extent <- extent(map_img)


#reduce size for quicker plotting
small_ras <- raster::aggregate(map_img,fact=5)

# change extent to visible map and crop
borderless_extent <- extent(c(9.6137,11.4366,47.15,48.95))
#cropped_ras <- raster::crop(small_ras,borderless_extent)

#hillshade_img <- as.array(cropped_ras/255)
hillshade_img <- as.array(small_ras/255)
major_dim <- 600

# texture from http://matthewkling.github.io/media/rayshader/
texture <- create_texture("red", "darkgreen",
                          "khaki", "khaki", "khaki")

bbox <- extent_to_bbox(full_extent)

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

#lauingen
lauingen_pos <- find_image_coordinates(long=10.4286742,
                                    lat=48.5678019,
                                    bbox = bbox,
                                    image_width=image_size$width,
                                    image_height=image_size$height)


elev_file <- file.path("data", "schwaben.tif")
#overlay_file <- "img/denison_map.png"
#overlay_file2 <- "img/denison_map2.png"
# -------------------------------------------------------------
# Download external data

elev_img <- get_elev_raster(small_ras,z=8) %>% 
  raster::crop(full_extent)
# -------------------------------------------------------------

#load overlays
#overlay_img2 <- png::readPNG(overlay_file2)
#overlay_img <- png::readPNG(overlay_file)
# load elevation data
borderless_extent <- extent(c(9.6137,11.4366,47.189,48.90))

elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

elev_matrix <- elev_matrix %>% zero_out_border(full_extent,borderless_extent)

# calculate rayshader layers
zscale = 50
raymat <- ray_shade(elev_matrix, sunaltitude = 25,zscale = zscale, lambert = TRUE)
# watermap <- detect_water(elev_matrix)

# ambmat <- ambient_shade(elev_matrix, zscale = zscale)
# raymat <- ray_shade(elev_matrix,anglebreaks=seq(20,30,1),zscale = zscale, lambert = TRUE)


# show one view
#hillshade_img %>% 
# elev_matrix %>%
#   sphere_shade(texture = "bw", zscale = zscale) %>%
#   # add_water(watermap, color = "imhof4") %>%
#   add_shadow(raymat, max_darken = 0.9) %>%
#   add_shadow(ambmat, max_darken = 0.5) %>%
# #  add_overlay(overlay_img, alphalayer = 0.7) %>%
# #  add_overlay(overlay_img2, alphalayer = 0.7) %>%
#   plot_map()


rgl::clear3d()
hillshade_img %>% 
#elev_matrix %>% 
#  sphere_shade(texture = "bw") %>% 
  # add_water(watermap, color = "imhof4") %>%
  # add_overlay(overlay_img, alphalayer = 0.9) %>%
#  add_overlay(overlay_img2, alphalayer = 0.8) %>%
  add_shadow(raymat, max_darken = 0.4) %>%
  add_shadow(ambmat, max_darken = 0.6) %>%
  plot_3d(elev_matrix, zscale = zscale, windowsize = c(1200, 1000),
          water = FALSE, soliddepth = "auto", wateralpha = 0,
          theta = 0, phi = 30, zoom = 0.5, fov = 60)

render_label(elev_matrix,text="Lauingen",x=lauingen_pos$x,y=lauingen_pos$y,z=4000,zscale=zscale,relativez=FALSE,freetype = FALSE)
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


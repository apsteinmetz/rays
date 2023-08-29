library(raster)
# devtools::install_github("tylermorganwall/rayshader")
library(rayshader)
library(tidyverse)
library(rgdal)
library(sf)
library(leaflet)
library(png)
library(jpeg)
library(elevatr)
library(basemaps)


map_full <- raster::stack("img/denison_plan_modified.tif")
map_overlay <- readPNG("img/denison_plan_geo.png")
map_inset <- raster::stack("img/denison_plan_inset1.tif")

full_extent <- extent(map_full)
inset_extent <- extent(map_inset)

# get image overlay
satview <- basemap_png(
  ext = map_full,
  map_service = "esri",
  map_type = "world_imagery",
  map_res = NULL,
  verbose = TRUE,
  browse = FALSE
)
#reduce size for quicker plotting
small_ras <- map_full
#small_ras <- raster::aggregate(map_full,fact=5)

#cropped_ras <- raster::crop(small_ras,trimmed_extent)
raw_elevation <- get_elev_raster(raster(small_ras),z = 14,clip = "bbox")

elevation <-raster::crop(raw_elevation,extent(small_ras))
#crop_elevation <- raster::crop(raw_elevation,extent(cropped_ras))
inset_elevation <- raster::crop(raw_elevation,extent(map_inset))
inset_elevation <- inset_elevation * 0 + 100
# base_raster <- elevation * 0 + 100
elevation <- merge(inset_elevation, base_raster)

# names(small_ras) <- c("r", "g", "b")
# img_r <- rayshader::raster_to_matrix(small_ras$r)
# img_g <- rayshader::raster_to_matrix(small_ras$g)
# img_b <- rayshader::raster_to_matrix(small_ras$b)
# img_rgb <- array(0, dim = c(nrow(small_ras), ncol(small_ras), 3))
# 
# img_rgb[,,1] <- img_r
# img_rgb[,,2] <- img_g
# img_rgb[,,3] <- img_b
# img_rgb <- aperm(img_rgb, c(2,1,3))
# plot_map(img_rgb)

elev_matrix <- raster_to_matrix(elevation)
zscale = 3
ray_shadow <- ray_shade(elev_matrix, sunaltitude = 30, zscale = zscale, multicore = TRUE)
ambient_shadow <- ambient_shade(elev_matrix, zscale = zscale)

elev_matrix %>%
  sphere_shade() %>%
  #add_overlay(map_overlay,rescale_original = TRUE) %>%
  add_overlay(satview,rescale_original = TRUE) %>%
  add_shadow(ray_shadow, max_darken = 0.1) %>%
  add_shadow(ambient_shadow, 0.25) %>%
  plot_3d(elev_matrix,zscale = zscale)
  # plot_map()

# texture from http://matthewkling.github.io/media/rayshader/
texture <- create_texture("red", "darkgreen",
                          "khaki", "khaki", "khaki")





# view bounding box.
leaflet() %>%
  addTiles() %>% 
  addRectangles(
    lng1 = full_extent@xmin, lat1 = full_extent@ymin,    
    lng2 = full_extent@xmax, lat2 = full_extent@ymax,
    fillColor = "transparent"
  ) %>%
  addRectangles(
    lng1 = full_extent@xmin, lat1 = full_extent@ymin,    
    lng2 = full_extent@xmax, lat2 = full_extent@ymax,
    fillColor = "transparent"
  ) %>%
  fitBounds(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
    lng2 = bbox$p2$long, lat2 = bbox$p2$lat
  )

major_dim <- 600
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

elev_img <- get_elev_raster(cropped_ras,z=7)
# -------------------------------------------------------------

#load overlays
#overlay_img2 <- png::readPNG(overlay_file2)
#overlay_img <- png::readPNG(overlay_file)
# load elevation data

# change extent to visible map and crop
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

# flatten border
elev_matrix <- elev_matrix %>% raster::raster() %>% 
   zero_out_border(full_extent,borderless_extent)

# calculate rayshader layers
zscale = 200
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
#  add_shadow(ambmat, max_darken = 0.6) %>%
  plot_3d(elev_matrix, zscale = zscale, windowsize = c(1200, 1000),
          water = FALSE, soliddepth = "auto", wateralpha = 0,
          theta = 0, phi = 30, zoom = 0.5, fov = 60)

frame_path = "frames/71/zones"

render_label(elev_matrix,text="Lauingen",x=lauingen_pos$x,y=lauingen_pos$y,z=4000,zscale=zscale,relativez=FALSE,freetype = FALSE)
render_snapshot()
frame_count = 0
azimuth = 90
for (zoom in 80:30){
#  render_camera(phi = 90, theta = 0,zoom = zoom/100 ,fov=80)
  frame_count <- frame_count + 1
#  render_snapshot(paste0(frame_path,str_pad(frame_count,3,pad="0"),".png"))
}

for (azimuth in 90:15){
  render_camera(phi = azimuth, theta = 0,zoom = 0.3,fov=80)
  frame_count <- frame_count + 1
  render_snapshot(paste0(frame_path,str_pad(frame_count,3,pad="0"),".png"))
}


for (theta in 1:360){
  render_camera(phi = 15, theta = theta,zoom = 0.3,fov=80)
  frame_count <- frame_count + 1
  render_snapshot(paste0(frame_path,str_pad(frame_count,3,pad="0"),".png"))
}

#Run this command in the command line using ffmpeg to stitch together a video:
# using frame file name instead of "frame"
#ffmpeg -framerate 60 -i "frame%03d.png" -vcodec libx264 raymovie.mp4

#And run this command to convert the video to post to the web:
#ffmpeg -i raymovie.mp4 -pix_fmt yuv420p -profile:v baseline -level 3 -vf scale=-2:-2 rayweb.mp4


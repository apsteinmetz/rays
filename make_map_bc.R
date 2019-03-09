library(raster)
# devtools::install_github("tylermorganwall/rayshader")
library(rayshader)
library(tidyverse)
library(rgdal)
library(sf)
library(leaflet)
source("utilities_ray.r")

major_dim <- 1200
zscale = 80
latlon_bc <- list(lat=50.4132014,long=-125.5049464)
latlon_cr <- list(lat=50.0359846,long=-125.2600229)
# texture from http://matthewkling.github.io/media/rayshader/
texture <- create_texture("green3", "darkgreen",
                          "royalblue3", "khaki", "khaki")
lat_width <- 0.40
lon_width <- 0.40

bbox <- list(
  p1 = list(long=latlon_bc$long-lon_width, lat = latlon_bc$lat+lat_width),
  p2 = list(long=latlon_bc$long+lon_width, lat = latlon_bc$lat-lat_width)
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

bc_pos <- find_image_coordinates(latlon_bc$long,
                                    latlon_bc$lat,
                                    bbox = bbox,
                                    image_width=image_size$width,
                                    image_height=image_size$height)

cr_pos <- find_image_coordinates(latlon_cr$long,
                                    latlon_cr$lat,
                                    bbox = bbox,
                                    image_width=image_size$width,
                                    image_height=image_size$height)

elev_file <- file.path("data", "cdem_dem_092K.tif")
overlay_file <- "img/bc_map.png"
#overlay_file2 <- "img/salida_map2.png"



# Crop
crop_img <- function(elev_img,bbox){
  new_extent <- unlist(bbox) %>% matrix(nrow=2,ncol=2) %>% extent()
  elev_img <- elev_img %>% crop(new_extent)
  return(elev_img)
}

# -------------------------------------------------------------
# Download external data

#get_usgs_elevation_data(bbox, size = image_size$size, file = elev_file,
#                        sr_bbox = 4326, sr_image = 4326)

# plot 2D# fetch overlay image
get_arcgis_map_image(bbox, map_type = "World_Imagery", file = overlay_file,
                     width = image_size$width, height = image_size$height, 
                     sr_bbox = 4326)

# plot 2D# fetch overlay image
#get_arcgis_map_image(bbox, map_type = "World_Street_Map", file = overlay_file2,
#                     width = image_size$width, height = image_size$height, 
#                     sr_bbox = 4326)

# -------------------------------------------------------------

#load overlays
#overlay_img2 <- png::readPNG(overlay_file2)
overlay_img <- png::readPNG(overlay_file)
# load elevation data
elev_img <- raster::raster(elev_file) %>% crop_img(bbox)
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

# ---------------------------------------------------------------------

downsample_elev <- function(elev_matrix,target_image_size){
  spacing_w = dim(elev_matrix)[1] / target_image_size$width
  spacing_h = dim(elev_matrix)[2] / target_image_size$height
  # downsample but truncate items if rounding returns more points than target
  # this breaks if rounding dimensions LESS than target_image_size
  sample_w <- round(seq(1,dim(elev_matrix)[1], by = spacing_w))
  sample_h <- round(seq(1,dim(elev_matrix)[2], by = spacing_h))
  return(elev_matrix[sample_w,sample_h])
  
}

elev_matrix <- elev_matrix %>% downsample_elev(image_size)

# calculate rayshader layers
ambmat <- ambient_shade(elev_matrix, zscale = zscale)
raymat <- ray_shade(elev_matrix, anglebreaks=seq(20,30,1),zscale = zscale, lambert = TRUE)
watermap <- detect_water(elev_matrix)



elev_matrix %>%
  sphere_shade(texture = "imhof4", zscale = zscale) %>%
  add_water(watermap, color = "imhof4") %>%
  add_shadow(raymat, max_darken = 0.4) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  add_overlay(overlay_img, alphalayer = 0.7) %>%
  #add_overlay(overlay_img2, alphalayer = 0.2) %>%
  plot_map()


rgl::clear3d()
elev_matrix %>% 
  sphere_shade(texture = "imhof4") %>% 
  add_water(watermap, color = "imhof4") %>%
  add_overlay(overlay_img, alphalayer = 0.8) %>%
  # add_overlay(overlay_img2, alphalayer = 0.4) %>%
  add_shadow(raymat, max_darken = 0.4) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  plot_3d(elev_matrix, zscale = zscale, windowsize = c(1200, 1000),
          water = TRUE, soliddepth = "auto", wateralpha = 0,
          theta = 25, phi = 30, zoom = 0.5, fov = 60)

render_label(elev_matrix,text="Blind Channel",x=bc_pos$x,y=bc_pos$y,z=5000,zscale=zscale,relativez=FALSE,freetype = FALSE)
render_label(elev_matrix,text="Campbell River",x=cr_pos$x,y=cr_pos$y,z=5000,zscale=zscale,relativez=FALSE,freetype = FALSE)
render_snapshot()

frame_count = 0
for (zoom in 80:30){
  #render_camera(phi = 90, theta = 0,zoom = zoom/100 ,fov=80)
  frame_count <- frame_count + 1
  #render_snapshot(paste0("frames/salida",str_pad(frame_count,3,pad="0"),".png"))
}

for (azimuth in 90:15){
  #render_camera(phi = azimuth, theta = 0,zoom = 0.3,fov=80)
  frame_count <- frame_count + 1
  #render_snapshot(paste0("frames/salida",str_pad(frame_count,3,pad="0"),".png"))
}
for (i in 1:360){
  render_camera(phi = 15, theta = i,zoom = 0.3,fov=80)
  frame_count <- frame_count + 1
  render_snapshot(paste0("frames/salida",str_pad(frame_count,3,pad="0"),".png"))
}

#Run this command in the command line using ffmpeg to stitch together a video:
#ffmpeg -framerate 60 -i frame%d.png -vcodec libx264 raymovie.mp4

#And run this command to convert the video to post to the web:
#ffmpeg -i raymovie.mp4 -pix_fmt yuv420p -profile:v baseline -level 3 -vf scale=-2:-2 rayweb.mp4


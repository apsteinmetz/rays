library(raster)
# devtools::install_github("tylermorganwall/rayshader")
library(rayshader)
library(tidyverse)
library(reshape2)
library(rgdal)
library(sf)
library(leaflet)
source("utilities_ray.r")

major_dim <- 1440
zscale = 50
latlon_bc <- list(lat=50.4132014,long=-125.5049464)
latlon_cr <- list(lat=50.0359846,long=-125.2600229)
# texture from http://matthewkling.github.io/media/rayshader/
texture <- create_texture("darkolivegreen", "darkgreen",
                          "royalblue3", "khaki", "khaki")
lat_width <- 0.25
lon_width <- 0.35

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



# load elevation data
elev_file <- file.path("data", "cdem_dem_092K.tif")
depth_file <- file.path("data", "bathymetric_bc.tif")

elev_img <- raster::raster(elev_file)  %>% crop_img(bbox)
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img))

depth_img <- raster::raster(depth_file)  %>% crop_img(bbox)
depth_matrix <- matrix(
  raster::extract(depth_img, raster::extent(depth_img), buffer = 1000), 
  nrow = ncol(depth_img), ncol = nrow(depth_img))

#if elev_matrix is less than major_dim, reduce major_dim
major_dim <- dim(depth_matrix) %>% max() %>% min(major_dim)
image_size <- define_image_size(bbox, major_dim = major_dim)
# depth matrix will be smaller. downsize elev_matrix
elev_matrix <- elev_matrix %>% downscale_elev(image_size)

# join elev and depth
elev_depth <- full_join(melt(elev_matrix,value.name = "elev"),
                        melt(depth_matrix,value.name = "depth")) %>% 
  as_tibble()

#use watermap to supplement detection of missing bathymetry
# if is water set elev to 0 so fake bathymetry gets added
watermap <- detect_water(elev_matrix,zscale,min_area = 1000) 
watermap_df <- watermap %>% 
  melt() %>% 
  rename(is_water = value)
# combine into one conforming matrix
# replace all NAs and values greater than zero in depth matrix with elev data and use that column
elev_depth_matrix <- elev_depth %>% 
  full_join(watermap_df) %>% 
  # set detected water to elev zero
  # mutate(elev = ifelse(is_water==1,0,elev)) %>%
  # set missing bathymetry to elevation
  mutate(depth = ifelse(is.na(depth),elev,depth)) %>%
  # let elevation < zero override bathymetry
  mutate(depth = ifelse(elev > 0, elev, depth)) %>% 
  # fake missing bathymetry by setting zero elevation to depth of minus 3 meters
  #mutate(depth = ifelse(depth == 0, -3 , depth)) %>% 
  pull(depth) %>% 
  matrix(nrow = image_size$width,ncol = image_size$height) %>% 
  # fake missing bathymetry by setting zero depth values to depth based in distance to shore
  # depth step is in meters per unit of distance to shore
  fake_depth(depth_step=5) %>% 
  {.}

# ---------------------------------------------------------------------


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

overlay_file <- "img/bc_map.png"

# -------------------------------------------------------------
# Download external data

# plot 2D# fetch overlay image
# get_arcgis_map_image(bbox, map_type = "World_Imagery", file = overlay_file,
#                      width = image_size$width, height = image_size$height, 
#                      sr_bbox = 4326)

# plot 2D# fetch overlay image
#get_arcgis_map_image(bbox, map_type = "World_Street_Map", file = overlay_file2,
#                     width = image_size$width, height = image_size$height, 
#                     sr_bbox = 4326)

# -------------------------------------------------------------


#load overlays
#overlay_img2 <- png::readPNG(overlay_file2)
overlay_img <- png::readPNG(overlay_file)

# chose matrix to use
# calculate rayshader layers
ambmat <- ambient_shade(elev_depth_matrix, zscale = zscale)
raymat <- ray_shade(elev_depth_matrix, anglebreaks=seq(1,10,1),zscale = zscale, lambert = TRUE)

elev_depth_matrix %>%
  sphere_shade(texture = texture, zscale = zscale) %>%
  add_water(watermap, color = "imhof4") %>%
  add_shadow(raymat, max_darken = 0.4) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  #add_overlay(overlay_img, alphalayer = 0.7) %>%
  #add_overlay(overlay_img2, alphalayer = 0.2) %>%
  plot_map()


rgl::clear3d()
elev_depth_matrix %>% 
  sphere_shade(texture = "imhof1") %>% 
  #add_water(watermap, color = "imhof4") %>%
  # add_overlay(overlay_img, alphalayer = 0.7) %>%
  # add_overlay(overlay_img2, alphalayer = 0.4) %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  plot_3d(elev_depth_matrix, zscale = zscale, windowsize = c(1200, 1000),
          theta = 25, phi = 30, zoom = 0.5, fov = 0, 
          soliddepth = "auto", 
          water = TRUE, 
          wateralpha = 0.5,
          waterdepth = 0,
          watercolor = "lightblue",
          waterlinecolor = "white",
          waterlinealpha = 0.0
          )

# render_label(elev_matrix,text="Blind Channel",x=bc_pos$x,y=bc_pos$y,z=5000,zscale=zscale,relativez=FALSE,freetype = FALSE)
# render_label(elev_matrix,text="Campbell River",x=cr_pos$x,y=cr_pos$y,z=5000,zscale=zscale,relativez=FALSE,freetype = FALSE)
render_snapshot()
#render_camera(phi = 15, theta = 0,zoom = 0.15 ,fov=80)

frame_count = 0
for (zoom in 80:20){
  render_camera(phi = 90, theta = 0,zoom = zoom/100 ,fov=80)
  frame_count <- frame_count + 1
  render_snapshot(paste0("frames/blindchannel",str_pad(frame_count,3,pad="0"),".png"))
}

for (azimuth in 90:30){
  render_camera(phi = azimuth, theta = 0,zoom = 0.2,fov=80)
  frame_count <- frame_count + 1
  render_snapshot(paste0("frames/blindchannel",str_pad(frame_count,3,pad="0"),".png"))
}
for (i in 1:360){
  render_camera(phi = 30, theta = i,zoom = 0.2,fov=80)
  frame_count <- frame_count + 1
  render_snapshot(paste0("frames/blindchannel",str_pad(frame_count,3,pad="0"),".png"))
}

#Run this command in the command line using ffmpeg to stitch together a video:
#ffmpeg -framerate 10 -i frame%d.png -vcodec libx264 raymovie.mp4

#And run this command to convert the video to post to the web:
#ffmpeg -i raymovie.mp4 -pix_fmt yuv420p -profile:v baseline -level 3 -vf scale=-2:-2 rayweb.mp4

# rgl::clear3d()
# elev_matrix_2 %>% 
# sphere_shade(zscale=10,texture = "imhof1") %>% 
#   add_shadow(ambmat,0.5) %>%
#   add_shadow(raymat) %>%
#   plot_3d(elev_matrix_2,zscale=50,fov=0,theta=-45,phi=45,windowsize=c(1000,800),zoom=0.75,
#           water=TRUE, waterdepth = 0, wateralpha = 0.5,watercolor = "lightblue",
#           waterlinecolor = "white",waterlinealpha = 0.5)
# render_snapshot()

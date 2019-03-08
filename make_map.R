library(raster)
# devtools::install_github("tylermorganwall/rayshader")
library(rayshader)
library(tidyverse)
library(rgdal)
library(sf)
library(leaflet)
source("utilities_ray.r")

latlon_alder<-list(lat=38.374304,long=-106.1165398)
latlon_salida<-list(lat=38.53,long=-105.9922354)
latlon_mid <- list(lat=38.442294,long= -106.033139)
lat_width <- 0.12
lon_width <- 0.12
major_dim <- 1200
zscale = 20
latlon_house <- list(lat=38.513622,long=-106.0157)
latlon_ranch <- list(lat=38.38096,long=-106.0711)
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

# -------------------------------------------------------------
# Download external data

elev_file <- file.path("data", "salida.tif")
get_usgs_elevation_data(bbox, size = image_size$size, file = elev_file,
                        sr_bbox = 4326, sr_image = 4326)

# plot 2D# fetch overlay image
overlay_file <- "img/salida_map.png"
get_arcgis_map_image(bbox, map_type = "World_Imagery", file = overlay_file,
                     width = image_size$width, height = image_size$height, 
                     sr_bbox = 4326)
overlay_img <- png::readPNG(overlay_file)

# plot 2D# fetch overlay image
overlay_file2 <- "img/salida_map2.png"
get_arcgis_map_image(bbox, map_type = "World_Street_Map", file = overlay_file2,
                     width = image_size$width, height = image_size$height, 
                     sr_bbox = 4326)
overlay_img2 <- png::readPNG(overlay_file2)

# -------------------------------------------------------------
# load elevation data
elev_img <- raster::raster(elev_file)
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

# calculate rayshader layers
ambmat <- ambient_shade(elev_matrix, zscale = zscale)
raymat <- ray_shade(elev_matrix, anglebreaks=seq(20,30,1),zscale = zscale, lambert = TRUE)
# watermap <- detect_water(elev_matrix)



elev_matrix %>%
  sphere_shade(texture = texture, zscale = zscale) %>%
  # add_water(watermap, color = "imhof4") %>%
  add_shadow(raymat, max_darken = 0.4) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  add_overlay(overlay_img, alphalayer = 0.7) %>%
  add_overlay(overlay_img2, alphalayer = 0.2) %>%
  plot_map()


rgl::clear3d()
elev_matrix %>% 
  sphere_shade(texture = texture) %>% 
  # add_water(watermap, color = "imhof4") %>%
  add_overlay(overlay_img, alphalayer = 0.9) %>%
  add_overlay(overlay_img2, alphalayer = 0.4) %>%
  add_shadow(raymat, max_darken = 0.4) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  plot_3d(elev_matrix, zscale = zscale, windowsize = c(1200, 1000),
          water = FALSE, soliddepth = "auto", wateralpha = 0,
          theta = 25, phi = 30, zoom = 0.5, fov = 60)

render_label(elev_matrix,text="House",x=house_pos$x,y=house_pos$y,z=4000,zscale=zscale,relativez=FALSE,freetype = FALSE)
render_label(elev_matrix,text="Ranch",x=ranch_pos$x,y=ranch_pos$y,z=4000,zscale=zscale,relativez=FALSE,freetype = FALSE)
render_snapshot()
for (azimuth in 90:15){
  render_camera(phi = azimuth, theta = 179,zoom = 0.3,fov=80)
}
for (i in 1:90){
  render_camera(phi = 15, theta = 179+i,zoom = 0.3,fov=80)
}

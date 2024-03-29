# qgis georeferenced tif w/o elev data

library(tidyverse)
library(rayshader)
library(raster)
library(rgdal)
library(sf)
library(leaflet)
source("utilities_ray.r")

geoTIFF_name <- "data/spanish_peaks.tif"
map_img <- raster::stack(geoTIFF_name)
full_extent <- extent(map_img)


#reduce size for quicker plotting
small_ras <- raster::aggregate(map_img,fact=5)

# change extent to visible map and crop
#borderless_extent <- extent(c(-74.5,-74.25,41.00,41.25))
#cropped_ras <- crop(small_ras,borderless_extent)

#hillshade_img <- as.array(cropped_ras/255)
hillshade_img <- as.array(small_ras/255)


bbox <- extent_to_bbox(full_extent)
#bbox2 <- extent_to_bbox(borderless_extent)

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

major_dim <- 600
image_size <- define_image_size(bbox, major_dim)

# -------------------------------------------------------------
# Download external elevation data
# get elevation data


image_size <- define_image_size(extent_to_bbox(full_extent), major_dim = 600)

elev_file = "data/sp.tif"
get_usgs_elevation_data(extent_to_bbox(map_img), 
                        size = image_size$size, file = elev_file,
                        sr_bbox = 4326, sr_image = 4326)

elev_img <- raster::raster(elev_file)
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

#elev_matrix <- elev_matrix %>% 
#  zero_out_border(full_extent,borderless_extent)

zscale = 7

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
  plot_3d(elev_matrix,zscale=zscale,zoom = )

rgl::close3d()
#Render snapshot with depth of field
render_depth(focus=0.982,focallength = 4000)




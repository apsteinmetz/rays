# render historical georeferenced map with elev data
# specifically usgs topos

library(tidyverse)
library(rayshader)
library(raster)
library(suncalc)

source("utilities_ray.r")

#get sun angles
sun_positions <- 1:47 %>% map(function(h) 
  getSunlightPosition(date = as.POSIXct("2017-11-01 00:00:00", tz = "EST") + 
                        h * 3600/2,
                      lat=41,lon = -74
                      )) %>% 
  bind_rows() %>% 
  transmute(date,
            altitude = altitude *180/pi,
            azimuth = azimuth *180/pi) %>%
  group_by(altitude) %>% 
  mutate(azimuth = ifelse(azimuth <= 0,azimuth + 180, 180 + azimuth)) %>% 
  mutate(altitude = max(altitude,0)) %>% 
  filter(altitude > 0) # daytime

#wgs84
village_extent_longlat <- extent(c(-74.33,-74.25,41.16,41.25))
bbox_village <- extent_to_bbox(village_extent_longlat) 

# process all topo map image files ------------------------
years = c(1891,1903,1910,1943,1954,2020)

#length(hillshades[[1]])
ref_size <- 9719640
hillshades <- years %>% 
  set_names() %>% 
  map(function(y){
  cat(y)
  geoTIFF_name <- glue::glue("data/gwl/{y}c.tif")
  map_img <- raster::stack(geoTIFF_name)
  # change extent to visible map and crop
  cropped_ras <- crop(map_img,village_extent_longlat)
  # sized_ras <- raster::aggregate(cropped_ras,fact = length(cropped_ras)/ref_size)
  hillshade_img <- as.array(cropped_ras/255)
  return(hillshade_img)
})

# get old close-up map
geoTIFF_name <- "img/Greenwood Lake village 1903_modified.tif"
map_img <- raster::stack(geoTIFF_name)
# change extent to visible map and crop
#cropped_small_ras <- crop(small_ras,village_extent)
small_ras <- raster::aggregate(map_img,fact=5)
cropped_ras <- raster::extend(small_ras,village_extent_longlat,value=255)
village_1903_img <- as.array(cropped_ras/255)



# Download imagery -----------------------------------------
# plot 2D# fetch overlay image
#overlay_file ="img/gwl_vill_sat.png"
#get_arcgis_map_image(bbox_village, map_type = "World_Imagery", file = overlay_file,
#                     width = image_size$width, height = image_size$height, 
#                     sr_bbox = 4326)

# plot 2D# fetch overlay image
#overlay_file2 ="img/gwl_vill_road.png"
#get_arcgis_map_image(bbox_village, map_type = "World_Street_Map", file = overlay_file2,
#                     width = image_size$width, height = image_size$height, 
#                     sr_bbox = 4326)

# -------------------------------------------------------------
# Download external elevation data
# get elevation data

# determine how fine grained the shadow map is
# higher will slow rendering time
# does not affect imagery
shadow_pixels <- 600

image_size <- define_image_size(extent_to_bbox(village_extent_longlat), 
                                major_dim = shadow_pixels)

elev_file = "data/gwl_vill.tif"
get_usgs_elevation_data(bbox_village, 
                        size = image_size$size, file = elev_file,
                        sr_bbox = 4326, sr_image = 4326)

elev_img <- raster::raster(elev_file)
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

# ----------------------------------------------------
# calculate shadow maps for each daylight hour

zscale = 20

ambmat <- ambient_shade(elev_matrix, 
                        zscale = zscale,
                        multicore = TRUE)


h = 11
raymat <- ray_shade(elev_matrix, 
                     sunaltitude = sun_positions$altitude[h],
                     sunangle = sun_positions$azimuth[h],
                     zscale = zscale, 
                     lambert = TRUE,
                     multicore = TRUE)


# RUN ONCE ----------------------------------------------------
# pre compute list of rayshades for saving

ray_shades <- 1:nrow(sun_positions) %>% 
  set_names() %>% 
  map(function(h) {
    print(h)
    ray_shade(
      elev_matrix,
      sunaltitude = sun_positions$altitude[h],
      sunangle = sun_positions$azimuth[h],
      zscale = zscale,
      lambert = TRUE,
      multicore = TRUE
    )
  })
save(ray_shades,file="data/ray_shades.rdata")
# -------------------------------------------------------------------
load("data/ray_shades.rdata")

# TEST --------------------------------------------------------------
hillshade_img <- hillshades[[5]]
# show one view
hillshade_img %>% 
  add_shadow(ray_shades[[6]], max_darken = 0.02) %>%
  plot_map()

#Plot in 3D
hillshade_img %>%
  add_shadow(ray_shades[[6]],max_darken = 0.0) %>%
  plot_3d(elev_matrix,
          theta= 355,
          phi = 45,
          zscale=zscale,zoom = 0.4)
render_label(ray_shades[[3]],text="1954",
             textsize=8,
             textcolor = "darkblue",
             x=325,y=550,z=10)
rgl::clear3d()

# # just village 
# village_1903_img %>%
#   add_shadow(ray_shades[[5]],max_darken = 0.0) %>%
#   plot_3d(elev_matrix,
#           theta= 355,
#           phi = 35,
#           zscale=zscale,zoom = 0.2)
# 
# rgl::close3d()




#Plot in 3D
hillshade_img %>%
  add_shadow(ray_shades[[11]],max_darken = 0.0) %>%
  plot_3d(elev_matrix,
          theta= 355,
          phi = 45,
          zscale=zscale,zoom = 0.4)

rgl::close3d()

#Plot in 3D
for (h in 1:11){

hillshade_img %>%
  add_shadow(ray_shades[[h]],max_darken = 0.0) %>%
  plot_3d(elev_matrix,
          theta= 355,
          phi = 45,
          zscale=zscale,zoom = 0.4)

}  
rgl::close3d()


# MAKE MOVIE -------------------------------------------
rgl::clear3d()
frame_count = 0
azimuth = 45
#for (zoom in 80:30){
#  #  render_camera(phi = 90, theta = 0,zoom = zoom/100 ,fov=80)
#  frame_count <- frame_count + 1
  #  render_snapshot(paste0("frames/denison",str_pad(frame_count,3,pad="0"),".png"))
#}

for (y in 1:length(years)){
  print(years[y])
  for (h in 1:nrow(sun_positions)){
    print(h)
    hillshades[[y]] %>%
      add_shadow(ray_shades[[h]],max_darken = 0.0) %>%
      plot_3d(elev_matrix,
              theta= 355,
              phi = 45,
              zscale=zscale,zoom = 0.4)
    
    render_label(ray_shades[[y]],text=years[y],
                 textsize=8,
                 textcolor = "darkblue",
                 x=350,y=550,z=10)
    # render_snapshot()  
    render_camera(phi = 45, theta = 355,zoom = 0.4,fov=0)
    frame_count <- frame_count + 1
    render_snapshot(paste0("frames/gwl/gwl",str_pad(frame_count,3,pad="0"),".png"))
    rgl::clear3d()
  }
}



# for (theta in 1:360){
#   render_camera(phi = 15, theta = theta,zoom = 0.3,fov=80)
#   frame_count <- frame_count + 1
#   render_snapshot(paste0("frames/denison",str_pad(frame_count,3,pad="0"),".png"))
# }

# ------------------------------------------------------





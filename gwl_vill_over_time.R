# render historical georeferenced map with elev data
# specifically usgs topos

library(tidyverse)
library(rayshader)
library(raster)
library(suncalc)

source("utilities_ray.r")
load(file="data/crs_wgs84.rdata")

#get sun angles
sun_positions <- 7:17 %>% map(function(h) 
  getSunlightPosition(date = as.POSIXct(glue::glue("2017-11-12 {h}:00:00",tz = "EST")),
                      lat=41,lon = -74)) %>% 
  bind_rows() %>% 
  transmute(altitude = altitude *180/pi,
            azimuth = azimuth *180/pi) %>%
  mutate(azimuth = ifelse(azimuth >= 0,azimuth + 180, 180 + azimuth))


geoTIFF_name <- "data/gwl/1891c.tif"
map_img <- raster::stack(geoTIFF_name)

full_extent <- extent(map_img)

#reduce size for quicker plotting
small_ras <- raster::aggregate(map_img,fact=5)

# change extent to visible map and crop

convert_extent <- function(longlat_extent,zone = "18T"){
  sp_geo <- SpatialPoints(longlat_extent, CRS("+proj=longlat +datum=WGS84")) 
  sp_utm <- spTransform(sp_geo,CRS(glue::glue("+proj=utm +zone={zone} +datum=WGS84"))) 
  return(extent(sp_utm))
  
}

#wgs84
village_extent_longlat <- extent(c(-74.33,-74.25,41.16,41.25))
#nad83 used by usgs topos
#village_extent <- convert_extent(village_extent_longlat)

bbox_village <- extent_to_bbox(village_extent_longlat) 
#cropped_small_ras <- crop(small_ras,village_extent)
cropped_ras <- crop(map_img,village_extent_longlat)

hillshade_img <- as.array(cropped_ras/255)
#hillshade_img <- as.array(small_ras/255)


# Download imagery -----------------------------------------
# plot 2D# fetch overlay image
overlay_file ="img/gwl_vill_sat.png"
get_arcgis_map_image(bbox_village, map_type = "World_Imagery", file = overlay_file,
                     width = image_size$width, height = image_size$height, 
                     sr_bbox = 4326)

# plot 2D# fetch overlay image
overlay_file2 ="img/gwl_vill_road.png"
get_arcgis_map_image(bbox_village, map_type = "World_Street_Map", file = overlay_file2,
                     width = image_size$width, height = image_size$height, 
                     sr_bbox = 4326)

# -------------------------------------------------------------
# Download external elevation data
# get elevation data

# determine how fine grained the shadow map is
# higher will slow rendering time
# does not affect imagery
shadow_pixels <- 400

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

#elev_matrix <- elev_matrix %>% 
#  zero_out_border(full_extent,borderless_extent)

# ----------------------------------------------------
# calculate shadow maps for each daylight hour

zscale = 20

#ambmat <- ambient_shade(elev_matrix, 
#                        zscale = zscale,
#                        multicore = TRUE)


raymat <- ray_shade(elev_matrix, 
                    sunaltitude = 35,
                    sunangle = 90,
                    zscale = zscale, 
                    lambert = TRUE,
                    multicore = TRUE)


ray_shades <- map(1:11, function(h) {
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

  

# show one view
hillshade_img %>% 
#sphere_shade(elev_matrix) %>% 
#  add_overlay(array_img, alphalayer = 1.0) %>%
  add_shadow(raymat, max_darken = 0.0) %>%
#  add_shadow(ambmat, max_darken = 0.5) %>%
  plot_map()



#Plot in 3D
hillshade_img %>%
  add_shadow(ray_shades[[1]],max_darken = 0.0) %>%
  plot_3d(elev_matrix,
          theta= 13,
          phi = 45,
          zscale=zscale,zoom = 0.4)

rgl::close3d()

#Plot in 3D
for (h in 1:11){
  
h = 1  
hillshade_img %>%
  add_shadow(ray_shades[[h]],max_darken = 0.0) %>%
  plot_3d(elev_matrix,
          theta= 13,
          phi = 45,
          zscale=zscale,zoom = 0.4)

}  
rgl::close3d()






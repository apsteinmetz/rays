# render historical georeferenced map with elev data
# specifically usgs topos

library(tidyverse)
library(rayshader)
library(raster)
source("utilities_ray.r")

geoTIFF_name <- "data/MT_ennis.tif"
map_img <- raster::stack(geoTIFF_name)
full_extent <- extent(map_img)
crs(map_img)
newcrs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#reduce size for quicker plotting
small_ras <- raster::aggregate(map_img,fact=3)
small_ras <- projectRaster(small_ras,crs = newcrs)
full_extent <- extent(small_ras)

#crop 
crop_extent <- extent(-dms_to_dec(111,29),-dms_to_dec(111,17),
                      dms_to_dec(45,13),dms_to_dec(45,18))
small_ras_crop <- raster::crop(small_ras,crop_extent)


hillshade_img <- as.array(small_ras_crop)
rgb_scaler <- max(hillshade_img,na.rm = TRUE)
hillshade_img <- hillshade_img/rgb_scaler

# get house pos
lot_pos$long <- -dms_to_dec(111,20,45.26)
lot_pos$lat <- dms_to_dec(45,14,28.26)
bbox = extent_to_bbox(crop_extent)


# -------------------------------------------------------------
# Download external elevation data
# get elevation data
image_size <- define_image_size(extent_to_bbox(crop_extent), major_dim = 600)

elev_file = "data/ennis_topo.tif"
get_usgs_elevation_data(extent_to_bbox(small_ras_crop), 
                        size = image_size$size, file = elev_file,
                        sr_bbox = 4326, sr_image = 4326)



elev_img <- raster::raster(elev_file)

elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)


zscale = 30

ambmat <- ambient_shade(elev_matrix, zscale = zscale,
                        multicore = TRUE)
raymat <- ray_shade(elev_matrix, sunaltitude = 45,zscale = zscale, 
                    lambert = TRUE,
                    multicore = TRUE)

# show one view
hillshade_img %>% 
  add_shadow(raymat, max_darken = 0.1) %>%
  add_shadow(ambmat, max_darken = 0.1) %>%
  plot_map()


#Plot in 3D
rgl::clear3d()
hillshade_img %>%
  add_shadow(raymat,0.3) %>%
#  add_shadow(ambmat,0) %>%
  plot_3d(elev_matrix,zscale=zscale,zoom = .5)
render_label(elev_matrix,
             text = "Greg and Arika's Lot",
             lat = lot_pos$lat,long = lot_pos$long,
             extent = extent(small_ras_crop),
             zscale = zscale,
             altitude = 3000)


render_snapshot()
rgl::close3d()
#Render snapshot with depth of field
#render_depth(focus=0.982,focallength = 4000)




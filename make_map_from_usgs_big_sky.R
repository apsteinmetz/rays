# render historical georeferenced map with elev data
# specifically usgs topos

library(tidyverse)
library(rayshader)
library(raster)
library(suncalc)
library(lubridate)
source("utilities_ray.r")


geoTIFF_name <- "data/MT_ennis.tif"
map_img <- raster::stack(geoTIFF_name)
full_extent <- extent(map_img)
crs(map_img)
newcrs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#reduce size for quicker plotting
#small_ras <- raster::aggregate(map_img,fact=1)
small_ras <- map_img
small_ras <- projectRaster(small_ras,crs = newcrs)
full_extent <- extent(small_ras)

#crop 
crop_extent <- extent(-dms_to_dec(111,29),-dms_to_dec(111,17),
                      dms_to_dec(45,11),dms_to_dec(45,18))
small_ras_crop <- raster::crop(small_ras,crop_extent)


hillshade_img <- as.array(small_ras_crop)
rgb_scaler <- max(hillshade_img,na.rm = TRUE)
hillshade_img <- hillshade_img/rgb_scaler

# get house pos
lot_pos <- NULL
lot_pos$long <- -dms_to_dec(111,20,45.26)
lot_pos$lat <- dms_to_dec(45,14,28.26)
bbox = extent_to_bbox(crop_extent)



# get sun positions at minute intervals
get_sun_positions <- function(date=format(Sys.time(),"%Y-%m-%d"), 
                              zone = "EST",
                              minute_increment = 60,
                              lat=41,lon=-74) {
  date_time = as.POSIXct(date,tz = zone)
  full_day <- seq.POSIXt(from = date_time,
             by=paste(minute_increment,"min"),
             length.out = round((24*60)/minute_increment))
  
  getSunlightPosition(
        date =full_day,
        lat = lat,lon = lon) %>% 
    transmute(time = hms::as_hms(date),
      altitude = altitude * 180 / pi,
      azimuth = azimuth * 180 / pi
    ) %>%
    mutate(azimuth = ifelse(azimuth >= 0, azimuth + 180, 180 + azimuth)) %>%
    filter(altitude > 0) %>% 
    {.}
}



winter_sun <- get_sun_positions("2022-12-20",zone="MST",
                                minute_increment = 15,
                                lat = lot_pos$lat,
                                lon = lot_pos$long)
    
summer_sun <- get_sun_positions("2022-06-20",zone="MST",
                                minute_increment = 15,
                                lat = lot_pos$lat,
                                lon = lot_pos$long)

# -------------------------------------------------------------
# Download external elevation data
# get elevation data
image_size <- define_image_size(extent_to_bbox(crop_extent), major_dim = 1200)

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

#ambmat <- ambient_shade(elev_matrix, zscale = zscale,
#                        multicore = TRUE)
raymat <- ray_shade(elev_matrix, sunaltitude = 5,zscale = zscale, 
                    lambert = TRUE,
                    multicore = TRUE)

# show one view
hillshade_img %>% 
  add_shadow(ray_shades_w[[11]], max_darken = 0.0) %>%
#  add_shadow(ambmat, max_darken = 0.1) %>%
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


# shadows over the course of a day
ray_shades_s <- 1:nrow(summer_sun) %>% 
  set_names() %>% 
  map(function(h) {
    print(h)
    ray_shade(
      elev_matrix,
      sunaltitude = max(0,summer_sun$altitude[h]),
      sunangle = summer_sun$azimuth[h],
      zscale = zscale,
      lambert = TRUE,
      multicore = TRUE
    )
  })
save(ray_shades_s,file="data/ray_shades_big_sky_s.rdata")

ray_shades_w <- 1:nrow(winter_sun) %>% 
  set_names() %>% 
  map(function(h) {
    print(h)
    ray_shade(
      elev_matrix,
      sunaltitude = max(0,winter_sun$altitude[h]),
      sunangle = winter_sun$azimuth[h],
      zscale = zscale,
      lambert = TRUE,
      multicore = TRUE
    )
  })
save(ray_shades_w,file="data/ray_shades_big_sky_w.rdata")
# MAKE MOVIE -------------------------------------------
rgl::clear3d()
frame_count = 0

for (h in 1:nrow(winter_sun)){
    print(h)
  hillshade_img %>%
      add_shadow(ray_shades_w[[h]],max_darken = 0.0) %>%
      plot_3d(elev_matrix,
              zscale=zscale,
              zoom = 0.5)
    
  render_label(elev_matrix,
               text = "Greg and Arika's Lot",
               lat = lot_pos$lat,long = lot_pos$long,
               extent = extent(small_ras_crop),
               zscale = zscale,
               altitude = 3000,
               linecolor = "red",
               textcolor = "red")
  # render_snapshot()  
  # render_camera(phi = 45, theta = 355,zoom = 0.4,fov=0)
    frame_count <- frame_count + 1
    render_snapshot(paste0("frames/bigsky/bigsky_w",str_pad(frame_count,3,pad="0"),".png"))
    rgl::clear3d()
  }



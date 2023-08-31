# render historical georeferenced map with elev data
# specifically usgs topos
library(tidyverse)
library(rayshader)
library(raster)
library(elevatr)
library(png)
library(sf)
library(terra)
library(osrm)
devtools::install_github("riatelab/maptiles")
library(maptiles)


start_point <- c(-74.2516694,41.0288009) # where skyline drive meets i-287
end_point <- c(-74.28869990,41.22772724) # north end of windermere ave. in Greenwood Lake
map_ras <- terra::rast("img/gwl_trip.tif") |> 
  terra::project("EPSG:3857")

# Use this as our common extent#
full_extent <- terra::ext(map_ras)

# get basemap tiles
gwl_basemap <- maptiles::get_tiles(map_ras,
                    provider="Thunderforest.Outdoors",
                    crop = TRUE,
                    zoom = 14,
                    apikey = Sys.getenv("THUNDERFOREST_KEY"))


terra::writeRaster(gwl_basemap,"img/gwl_basemap.png")
gwl_png <- readPNG("img/gwl_basemap.png")

# get water
# https://hub.arcgis.com/datasets/esri::usa-detailed-water-bodies/explore
# water_bodies = st_read("data/gwl/water/USA_Detailed_Water_Bodies.shp")

gwl_route_detailed <- osrm::osrmRoute(start_point,end_point,overview = "full") |> 
  st_transform(crs(map_ras))
gwl_route_simple <- osrm::osrmRoute(start_point,end_point,overview = "simplified") |> 
  st_transform(crs(map_ras))

plot(st_geometry(gwl_route_detailed), col = "red", lwd = 2)

raw_elevation <- get_elev_raster(raster(map_ras),z = 12)
elevation <- raw_elevation |> 
  rast() |> 
  terra::crop(map_ras)

elev_matrix <- elevation |> 
  raster_to_matrix()

route_overlay <- generate_line_overlay(gwl_route_detailed,
                                       map_ras,
                                       heightmap = elev_matrix,
                                       color = "red",
                                       linewidth = 2)

shadow_lamb <- lamb_shade(elev_matrix)
shadow_ray <- ray_shade(elev_matrix,lambert=FALSE)
shadow_ambient <- ambient_shade(elev_matrix)
water_overlay <- detect_water(elev_matrix,min_area = length(elev_matrix)/4000)

elev_matrix |> 
  sphere_shade() |> 
  # height_shade(texture = (grDevices::colorRampPalette(c("#6AA85B", "orange2")))(256)) |> 
#  add_water(water_overlay,color="steelblue") |> 
  add_overlay(gwl_png,rescale_original = TRUE) |> 
  add_shadow(shadow_ray) |> 
  add_shadow(shadow_lamb) |> 
  add_shadow(shadow_ambient) |> 
  add_overlay(route_overlay) |> 
#  plot_map()
  plot_3d(elev_matrix,zscale = 7)

render_path(gwl_route_detailed, 
            altitude = 500,
            extent = full_extent, 
            heightmap = elev_matrix, 
            color="red", linewidth = 4, 
            zscale=7)


camera_coords <- 
  convert_path_to_animation_coords(
  gwl_route_simple,
  extent = full_extent, 
  heightmap = elev_matrix, 
  zscale = 7,
  # not needed for single geometry
  # reorder = TRUE,
  #reorder_merge_tolerance = 0.45, 
  # reorder_duplicate_tolerance = 0.20,
  resample_path_evenly = TRUE,
  offset = 10, 
  offset_lookat = 10,
  altitude = 400,
  follow_camera = TRUE,
  follow_distance = 100,
  follow_fixed = FALSE,
#  follow_fixed_offset = c(10,10,10),
  follow_angle = 15,
  damp_motion = TRUE, 
  damp_magnitude = 0.5,
   frames = 300,

)

# render_camera()
# rgl::lines3d(camera_coords[,1:3], color="black", tag = "path3d")

render_highquality(samples=128, line_radius=0.5, sample_method = "sobol_blue")

render_highquality(animation_camera_coords = camera_coords, width=800, height=800,
                   samples=128, line_radius=0.5, sample_method = "sobol_blue",
                   ambient_light = TRUE,
                   filename="frames/gwl_")
# external program
# ffmpeg -framerate 5 -i gwl_%d.png -pix_fmt yuv420p roadtrip.mp4

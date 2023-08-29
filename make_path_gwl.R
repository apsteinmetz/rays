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


start_point <- c(-74.2516694,41.0288009) # where skyline drive meets i-287
end_point <- c(-74.28869990,41.22772724) # north end of windermere ave. in Greenwood Lake
map_ras <- raster::stack("img/gwl_trip.tif")

# Use this as our common extent#
full_extent <- extent(map_ras)

# get water
# https://hub.arcgis.com/datasets/esri::usa-detailed-water-bodies/explore
# water_bodies = st_read("data/gwl/water/USA_Detailed_Water_Bodies.shp")

gwl_route_detailed <- osrm::osrmRoute(start_point,end_point,overview = "full")
gwl_route_simple <- osrm::osrmRoute(start_point,end_point,overview = "simplified")
plot(st_geometry(gwl_route_detailed), col = "red", lwd = 2)

raw_elevation <- get_elev_raster(raster(map_ras), prj = "+proj=longlat +datum=WGS84 +no_defs",z = 12)
elevation <- crop(raw_elevation,extent(map_ras))

elev_matrix <- elevation |> 
  raster_to_matrix()

route_overlay <- generate_line_overlay(gwl_route_detailed,
                                       full_extent,
                                       heightmap = elev_matrix,
                                       color = "black",
                                       linewidth = 2)

shadow_lamb <- lamb_shade(elev_matrix)
shadow_ray <- ray_shade(elev_matrix,lambert=FALSE)
shadow_ambient <- ambient_shade(elev_matrix)
water_overlay <- detect_water(elev_matrix,min_area = length(elev_matrix)/4000)

elev_matrix |> 
  #sphere_shade() |> 
  height_shade(texture = (grDevices::colorRampPalette(c("#6AA85B", "orange2")))(256)) |> 
  add_water(water_overlay,color="steelblue") |> 
  add_shadow(shadow_ray) |> 
  add_shadow(shadow_lamb) |> 
  add_shadow(shadow_ambient) |> 
  add_overlay(route_overlay) |> 
  plot_3d(elev_matrix,zscale = 8)

render_path(gwl_route_simple, 
            extent = full_extent, 
            heightmap = elev_matrix, 
            color="red", linewidth = 4, 
            zscale=8, offset = 50)

camera_coords = convert_path_to_animation_coords(
  gwl_route_simple,
  extent = full_extent, 
  offset_lookat = 1,
  heightmap = elev_matrix, 
  zscale = 8,
  offset = 50, 
  # not needed for single geometry
  # reorder = TRUE,
  #reorder_merge_tolerance = 0.45, 
  # reorder_duplicate_tolerance = 0.20,
  resample_path_evenly = TRUE,
  frames = 1080, 
  follow_camera = TRUE,
  follow_fixed = TRUE,
  follow_fixed_offset = c(10,10,10),
  damp_motion = TRUE, 
  damp_magnitude = 0.5
)

render_highquality(animation_camera_coords = camera_coords[nrow(camera_coords):1,], width=800, height=800,
                   samples=128, line_radius=0.10, sample_method = "sobol_blue", 
                   filename="frames/gwl_")

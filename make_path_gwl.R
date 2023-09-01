library(rayshader)
library(elevatr)
library(png)
library(sf)
library(terra)
library(raster)
library(osrm)
library(maptiles)
library(tidyverse)


start_point <- c(-74.2548014,41.0248253) # where skyline drive meets i-287
end_point <- c(-74.28869990,41.22772724) # north end of windermere ave. in Greenwood Lake
full_extent <- ext(-8278678.63106571, -8264262.03289493, 5015662.67345158, 5049556.38996736)
map_ras <- rast(extent = full_extent, crs="EPSG:3857")

# get basemap tiles
gwl_basemap <- maptiles::get_tiles(map_ras,
                                   provider="Thunderforest.Outdoors",
                                   crop = TRUE,
                                   zoom = 14,
                                   apikey = Sys.getenv("THUNDERFOREST_KEY"))


terra::writeRaster(gwl_basemap,"img/gwl_basemap.png",overwrite=TRUE)
gwl_png <- readPNG("img/gwl_basemap.png")


gwl_route_detailed <- osrm::osrmRoute(start_point,end_point,overview = "full") |> 
  st_transform(crs(map_ras))

gwl_route_simple <- osrm::osrmRoute(start_point,end_point,overview = "simplified") |> 
  st_transform(crs(map_ras))

# get water
# https://hub.arcgis.com/datasets/esri::usa-detailed-water-bodies/explore
# water_bodies = st_read("data/gwl/water/USA_Detailed_Water_Bodies.shp")

# render historical georeferenced map with elev data
# specifically usgs topos
raw_elevation <- get_elev_raster(raster(map_ras),z = 12)
elevation <- raw_elevation |> 
  rast() |> 
  terra::crop(map_ras)

elev_matrix <- elevation |> 
  raster_to_matrix()

route_overlay <- generate_line_overlay(gwl_route_detailed,
                                       map_ras,
                                       width = 1510,
                                       height = 3549,
                                       color = "red",
                                       linewidth = 2)

shadow_lamb <- lamb_shade(elev_matrix)
shadow_ray <- ray_shade(elev_matrix,lambert=FALSE)
shadow_ambient <- ambient_shade(elev_matrix)

elev_matrix |> 
  sphere_shade() |> 
  add_overlay(gwl_png,rescale_original = TRUE) |> 
  add_shadow(shadow_ray) |> 
  add_shadow(shadow_lamb) |> 
  add_shadow(shadow_ambient) |> 
  add_overlay(route_overlay) |> 
  plot_3d(elev_matrix,zscale = 7)

camera_coords <-
  convert_path_to_animation_coords(
    gwl_route_simple,
    extent = full_extent,
    heightmap = elev_matrix,
    zscale = 7,
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
    damp_magnitude = 1,
    curvature_adjust = "bezier",
    frames = 600,
  )


tictoc::tic()
render_highquality(animation_camera_coords = camera_coords, width=800, height=800,
                   samples=128, line_radius=0.5, sample_method = "sobol_blue",
                   ambient_light = TRUE,
                   filename="frames/gwl_")
tictoc::tok()

# external program
# ffmpeg -framerate 5 -i gwl_%d.png -pix_fmt yuv420p roadtrip.mp4

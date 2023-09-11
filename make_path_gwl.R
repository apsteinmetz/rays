library(rayshader)
library(elevatr)
library(png)
library(sf)
library(terra)
library(raster)
library(smoothr)
library(osrm)
library(maptiles)
library(tidyverse)


start_point <- c(-74.2548014,41.0248253) # where skyline drive meets i-287
end_point <- c(-74.29600294,41.24158515) # top of Bellvale
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

gwl_route_very_simple <- gwl_route_detailed |> 
  st_line_sample(25) |> 
  st_cast("LINESTRING") |> 
  smoothr::smooth(method = "spline")

# change end point to top of mount peter, sort of
top_peter = st_point(c(-8269874.1,5047842.0))
route_points <- gwl_route_very_simple |> 
  st_cast("POINT",group_or_split = TRUE)
route_points[length(route_points)] <- top_peter
gwl_route_very_simple <- route_points |> 
  st_cast("POINT") |> 
  st_as_sf(agr = "constant") |> 
  summarise(do_union = FALSE) |> 
  st_cast("LINESTRING")


plot(st_geometry(gwl_route_detailed))
plot(st_geometry(gwl_route_very_simple))

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
                                       extent = map_ras,
                                       heightmap = elev_matrix,
                                       color = "green",
                                       linewidth = 2)

camera_path_overlay <- generate_line_overlay(gwl_route_very_simple,
                                             extent = map_ras,
                                       heightmap = elev_matrix,
                                       color = "red",
                                       linewidth = 4)

shadow_lamb <- lamb_shade(elev_matrix)
shadow_ray <- ray_shade(elev_matrix,lambert=FALSE)
shadow_ambient <- ambient_shade(elev_matrix)

# shaded base image
gwl <- elev_matrix |> 
  sphere_shade() |> 
  add_overlay(gwl_png,rescale_original = TRUE) |> 
  add_shadow(shadow_ray) |> 
  add_shadow(shadow_lamb) |> 
  add_shadow(shadow_ambient)

gwl |> 
  add_overlay(route_overlay) |> 
  add_overlay(camera_path_overlay) |> 
  plot_map()

gwl |> 
  # add_overlay(route_overlay) |> 
  add_overlay(camera_path_overlay) |> 
  plot_3d(elev_matrix,zscale = 7)
  
camera_coords <-
  convert_path_to_animation_coords(
    gwl_route_very_simple,
    extent = full_extent,
    heightmap = elev_matrix,
    zscale = 7,
    resample_path_evenly = TRUE,
    offset = 10,
    offset_lookat = 10,
    altitude = 500,
    follow_camera = TRUE,
    follow_distance = 100,
    follow_fixed = FALSE,
    #  follow_fixed_offset = c(10,10,10),
    follow_angle = 15,
    # follow_rotations = 3,
    # damp_motion = TRUE,
    # damp_magnitude = 1,
    frames = 300,
  )


tictoc::tic()
render_highquality(animation_camera_coords = camera_coords, width=800, height=800,
                   samples=128, line_radius=0.5, sample_method = "sobol_blue",
                   ambient_light = TRUE,
                   progress = TRUE,
                   verbose = TRUE,
                   filename="frames/gwl_")
tictoc::toc()

# make GIF 
file1 <- dir("frames/","gwl_[0-9].png")
file2 <- dir("frames/","gwl_[0-9]{2}.png")
file1_pad <- str_replace(file1,"gwl_","gwl_00")
file2_pad <- str_replace(file2,"gwl_","gwl_0")
file.rename(paste0("frames/",file1),paste0("frames/",file1_pad))
file.rename(paste0("frames/",file2),paste0("frames/",file2_pad))
fnames <- paste0("frames/",dir("frames/","*.png"))
gifski::gifski(fnames,delay = 0.3)

# external program
# ffmpeg -framerate 24 -i gwl_%d.png -pix_fmt yuv420p roadtrip.mp4
# ffmpeg -i roadtrip.mp4 -filter:v "minterpolate=fps=24:mi_mode=mci:mc_mode=aobmc:me_mode=bidir:vsbmc=1" roadtrip_interpolated.mp4
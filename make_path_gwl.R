# render historical georeferenced map with elev data
# specifically usgs topos
library(tidyverse)
library(rayshader)
library(raster)
library(elevatr)
library(png)
library(sf)



# Use this as our common extent#
full_extent <- extent(water_bodies)

# get water
# https://hub.arcgis.com/datasets/esri::usa-detailed-water-bodies/explore

water_bodies = st_read("data/gwl/water/USA_Detailed_Water_Bodies.shp")

# get roads
nj_passaic_roads = st_read("data/usa_roads/nj/tl_2022_34031_roads.shp") 
nj_bergen_roads = st_read("data/usa_roads/nj/tl_2022_34003_roads.shp") 
ny_orange_roads = st_read("data/usa_roads/ny/tl_2022_36071_roads.shp")

seg_1 <- nj_bergen_roads |> filter(FULLNAME %in% c("Skyline Dr")) |> st_union()
seg_2 <- nj_passaic_roads |> filter(FULLNAME %in% c("Greenwood Lake Tpke","Lakeside Rd","Skyline Dr"))  |> st_union()
seg_3 <- ny_orange_roads |> filter(FULLNAME %in% c("Jersey Ave","Windermere Ave")) |> st_union()

st_geometrycollection(seg_1,seg_2)
geometry = st_sfc(lapply(c(seg_1,seg_2,seg_3), function(x) st_geometrycollection()))


# map_ras <- raster::stack("img/Greenwood Lake-1910_modified.tif")
# cropped_ras <- raster::stack("img/Greenwood Lake-1910_cropped.tif")
# map_img <- readPNG("img/Greenwood Lake-1910_modified.png")

# full_extent <- extent(map_img)
#reduce size for quicker plotting
#small_ras <- raster::aggregate(map_ras,fact=5)

# change extent to visible map and crop

# -------------------------------------------------------------
# Download external elevation data
# get elevation data

raw_elevation <- get_elev_raster(raster(small_ras), z = 11)
elevation <- crop(raw_elevation,extent(map_ras))
crop_elevation <- crop(raw_elevation,extent(cropped_ras))

base_raster <- elevation * 0
elevation <- merge(crop_elevation, base_raster)



elev_matrix <- elevation |> 
  raster_to_matrix()


# r get roads
if (fs::file_exists('data/gwl_roads.rdata')) {
  load('data/gwl_roads.rdata')
} else {
  gwl_roads <- osmdata::opq(st_bbox(cropped_ras)) %>%
    osmdata::add_osm_feature("highway") %>%
    osmdata::osmdata_sf()
  save(gwl_roads, file = 'data/gwl_roads.rdata')
}

# Take just the road layer and transform coordinates to our existing projection
gwl_lines <- sf::st_transform(gwl_roads$osm_lines,
                                   crs = raster::crs(cropped_ras)) |> 
   st_crop(st_bbox(extent(cropped_ras)))





# gwl_lines |> ggplot(aes()) + geom_sf(color = "orange")
# # a rayshader object we pre-generate now for speed later
# road_overlay <-  generate_line_overlay(
#   gwl_lines,
#   color = "blue",
#   # dummy matrix to get roads at full resolution
#   # even though actual heightmap is lower res
#   heightmap = as.matrix(map_ras$Greenwood.Lake.1910_modified_1),
#   extent = extent(extent(map_ras)),
#   linewidth = 1
# )

# texture_shadow <- texture_shade(elev_matrix,detail=8/10,contrast=9,brightness = 11)

zscale = 12

# ambmat <- ambient_shade(elev_matrix, zscale = zscale,
#                        multicore = TRUE)
raymat <- ray_shade(elev_matrix, sunaltitude = 25,zscale = zscale, 
                    lambert = TRUE,
                    multicore = TRUE)

watermap <- detect_water(elev_matrix,min_area = length(elev_matrix)/1200)
# show one view
elev_matrix %>%
  sphere_shade(texture = "imhof1") |> 
  add_water(watermap) |> 
  # add_overlay(map_img, alphalayer = 1,rescale_original = FALSE) %>%
  # add_overlay(road_overlay) |> 
  # add_shadow(texture_shadow, max_darken = 0.1) %>%
  add_shadow(raymat, max_darken = 0.3) %>%
  # add_shadow(ambmat, max_darken = 0.5) %>%
  plot_map()
 

#Plot in 3D
elev_matrix %>% 
  sphere_shade() |> 
  add_water(watermap) |> 
  # add_overlay(map_img, alphalayer = 1,rescale_original = FALSE) %>%
  # add_overlay(road_overlay) |> 
  # add_shadow(raymat, max_darken = 0.3) %>%
  #add_shadow(ambmat, max_darken = 0.5) %>%
  plot_3d(elev_matrix,zscale = zscale)


rgl::close3d()
#Render snapshot with depth of field
render_depth(focus=0.982,focallength = 4000)




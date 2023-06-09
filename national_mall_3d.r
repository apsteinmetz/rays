#Load all the libraries needed
library(sf)
library(dplyr)
library(rayrender)
library(elevatr)
library(rayshader)
library(geojsonsf)
library(raster)
library(lidR)

#Use legacy sf behavior
sf_use_s2(FALSE)

#Load the 3D DC building dataset (from https://opendata.dc.gov/documents/buildings-in-3d/)
st_read("data/dc/BldgPly_3D.shp") -> buildings

#Set location of washington monument (to far too many decimal places)
washington_monument_location =  c(-77.03524937618121, 38.88946200298283)

#Transform the coordinate system from WGS84 (lat/long) to the DC building coordinate system
washington_monument_location |>
  st_point() |>
  st_sfc(crs = 4326) |>
  st_transform(st_crs(buildings)) ->
  monument_point_crs

#Need this to filter out only the MULTIPOLYGON entries
is_multipolygon = function(geometry) {
  return(unlist(lapply(geometry, inherits,"MULTIPOLYGON")))
}

#Calculate the centroids and only include buildings within 1000m of the monument
buildings |>
  filter(is_multipolygon(geometry)) |>
  mutate(centroid =  st_centroid(geometry)) |>
  mutate(dist = st_distance(centroid, monument_point_crs)) |>
  filter(dist < units::as_units(1000,"m")) ->
  national_mall_buildings

#Get the elevation data for the national mall
elevation_data = elevatr::get_elev_raster(locations = st_buffer(monument_point_crs,
                                                                dist=units::as_units(1000,"m")),
                                          z = 14)

#Get the bounding box for the scene
scene_bbox = st_bbox(national_mall_buildings)

#Crop the elevation data to that bounding box
cropped_data = raster::crop(elevation_data, scene_bbox)

#Use rayshader to convert that raster data to a matrix
dc_elevation_matrix = raster_to_matrix(cropped_data)

#Remove negative elevation data
dc_elevation_matrix[dc_elevation_matrix < 0] = 0

#Load road data
roads = st_read("Roads.shp")|>
  st_transform(st_crs(buildings))

#Load water data
waterbodies = st_read("Waterbodies.shp")|>
  st_transform(st_crs(buildings))

waterbody_bool = generate_polygon_overlay(waterbodies, extent=extent(cropped_data),
                                          heightmap = dc_elevation_matrix,
                                          palette = "dodgerblue3",
                                          linecolor = NA)

#Turn the elevation data into a 3D surface and plot using rayshader
dc_elevation_matrix |>
  height_shade()|>
  add_shadow(lamb_shade(dc_elevation_matrix), 0) |>
  add_overlay(generate_polygon_overlay(waterbodies, extent=extent(cropped_data),
                                       heightmap = dc_elevation_matrix,
                                       palette = "dodgerblue3",
                                       linecolor = NA)) |>
  add_overlay(generate_line_overlay(roads, extent=extent(cropped_data),
                                    heightmap = dc_elevation_matrix,
                                    color = "grey20", linewidth = 0.1)) |>
  plot_3d(dc_elevation_matrix,
          solid=T,zscale=4,water=F, waterdepth = 0.5,
          shadow=F, soliddepth=-50, windowsize = 800, fov=70,
          baseshape = "circle")

#Render the buildings--no lat or long data uses the raw coordinates in the OBJH file
render_multipolygonz(national_mall_buildings,
                     extent = extent(cropped_data), obj_zscale = TRUE,
                     clear_previous = TRUE, zscale = 4,
                     color = "grey80", heightmap = dc_elevation_matrix)

#I downloaded LIDAR data of the national mall: this finds all those data files.
# From: https://opendata.dc.gov/apps/lidar-grid-tiles/explore
#The files are "1914.las" "1915.las" "1916.las" "2014.las" "2015.las" "2016.las"
#"2114.las" "2115.las" "2116.las"
las_files_dc = list.files("~/Downloads", pattern="\\.las", full.names = T)

#Use the lidR package to load the data in
las_data = list()
for(i in 1:length(las_files_dc)) {
  las_data[[i]] = lidR::readLAS(las_files_dc[i])
}

#Remove many points to increase processing speed
thinned_las_data = list()
for(i in 1:length(las_files_dc)) {
  thinned_las_data[[i]] = lidR::decimate_points(las_data[[i]], lidR::homogenize(0.5,5))
}

#Use the lidR package to locate trees
tree_data = list()
for(i in 1:length(thinned_las_data)) {
  tree_data[[i]] = lidR::locate_trees(thinned_las_data[[i]],lmf(hmin = 10, ws = 10))
}

#Render the trees
for(i in 1:length(tree_data)) {
  tree_coord = st_coordinates(tree_data[[i]])
  tree_coord = tree_coord[tree_coord[,3] < 25,]
  render_tree(lat=tree_coord[,2], long = tree_coord[,1],
              zscale=4,
              canopy_height = tree_coord[,3],
              absolute_height = T, min_height = 2, baseshape="circle",
              extent= extent(cropped_data), heightmap = dc_elevation_matrix)
}
#Render a basic snapshot
render_camera(theta=-42,  phi=28,  zoom = 0.20, fov=70)

#Render a high quality image
render_highquality(sample_method="sobol_blue",samples=256)

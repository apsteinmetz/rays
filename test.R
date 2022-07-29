library(raster)
library(tidyverse)
library(rayshader)
library(rayrender)


if(interactive()) {

rgl::clear3d()
#Render the county borders as polygons in Monterey Bay
montereybay %>%
  sphere_shade(texture = "desert") %>%
  add_shadow(ray_shade(montereybay,zscale=50)) %>%
  plot_3d(montereybay,water=TRUE, windowsize=800, watercolor="dodgerblue")
render_camera(theta=140,  phi=55, zoom = 0.85, fov=30)

#We will apply a negative buffer to create space between adjacent polygons:
mont_county_buff = sf::st_simplify(sf::st_buffer(monterey_counties_sf,-0.003), dTolerance=0.001)
mont_county_buff = sf::st_simplify(monterey_counties_sf, dTolerance=0.1)

render_polygons(mont_county_buff, 
                extent = attr(montereybay,"extent"), top=10,
                parallel=TRUE)
render_snapshot()

#We can specify the bottom of the polygons as well. Here I float the polygons above the surface
#by specifying the bottom argument. We clear the previous polygons with `clear_previous = TRUE`.
render_camera(theta=-60,  phi=20, zoom = 0.85, fov=0)
render_polygons(mont_county_buff, 
                extent = attr(montereybay,"extent"), bottom = 190, top=200,
                parallel=TRUE,clear_previous=TRUE)
render_snapshot()

#We can set the height of the data to a column in the sf object: we'll use the land area.
#We'll have to scale this value because it's max value is 2.6 billion:
render_camera(theta=-60,  phi=60, zoom = 0.85, fov=30)
render_polygons(mont_county_buff, 
                extent = attr(montereybay,"extent"), data_column_top = "ALAND",
                scale_data = 300/(2.6E9), color="chartreuse4",
                parallel=TRUE,clear_previous=TRUE)
render_snapshot()        

#This function also works with `render_highquality()`
render_highquality(samples=400, clamp_value=10)
rgl::rgl.close()

}
[Package rayshader version 0.24.10 Index]
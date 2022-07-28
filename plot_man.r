# plot manhattan
library(tidyverse)
library(rayshader)


skyline <- as.array(man_raster)


elev_matrix <- matrix(
  raster::extract(
    man_raster,
    raster::extent(man_raster),
    buffer = 1000,
    fun = mean,
    na.rm = TRUE
  ),
  nrow = ncol(man_raster),
  ncol = nrow(man_raster)
)

zscale = 30

#ambmat <- ambient_shade(elev_matrix, zscale = zscale,
#                        multicore = TRUE)
raymat <- ray_shade(elev_matrix, sunaltitude = 45,zscale = zscale,
                    lambert = TRUE,
                    multicore = TRUE)

# show one view
elev_matrix %>%
#  add_overlay(array_img, alphalayer = 1.0) %>%
  add_shadow(raymat, max_darken = 0.9) %>%
#  add_shadow(ambmat, max_darken = 0.5) %>%
  plot_map()


#Plot in 3D
rgl::clear3d()
hillshade_img %>%
  add_shadow(raymat,0.3) %>%
  #  add_shadow(ambmat,0) %>%
  plot_3d(elev_matrix,zscale=zscale,zoom = .5)

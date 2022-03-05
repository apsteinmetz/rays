#Plot in 3D
crop_extent

lat = lot_pos$lat
long = lot_pos$long
heightmap <- small_ras_crop

if(!is.null(crop_extent)) {
  e = crop_extent
  x = (long-e@xmin)/(e@xmax - e@xmin) * nrow(heightmap)
  y = ncol(heightmap) - (lat-e@ymin)/(e@ymax - e@ymin) * ncol(heightmap)
}



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
             altitude = 5000)

montereybay %>%
  sphere_shade() %>%
  plot_3d(montereybay,zscale=50,water=TRUE, watercolor="#233aa1")
render_snapshot() 

## End(Not run)

santa_cruz = c(36.962957, -122.021033) 
#We want to add a label to Santa Cruz, so we use the x and y matrix coordinate (x=220 and y=330)
## Not run: 
render_label(montereybay,lat = santa_cruz[1], long = santa_cruz[2],
             extent = attr(montereybay, "extent"),
             altitude=12000, zscale=50, text = "Santa Cruz")
render_snapshot()

montereybay

getSunlightTimes(date = Sys.Date(), 
                 keep = c("sunrise", "sunset"), 
                 lat = lot_pos$lat, lon = lot_pos$long, 
                 tz = "MST")


getSunlightPosition(date = as.POSIXct(glue::glue("2022-03-04 7:00"),tz = "America/Denver"),
                    lat=lot_pos$lat,lon =lot_pos$long)
  
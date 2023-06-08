# KAKHOVKA DAM
library(tidyverse)
library(elevatr)
library(rayshader)
library(sf)
library(leaflet)
library(osmdata)

dms_to_dec <- function(deg=0, min=0, sec=0) {
  return(deg + min / 60 + sec / 3600)
}
#elevatr::set_opentopo_key(Sys.getenv("OPEN_TOPO"))

# bounding box
nw <- list(lat = dms_to_dec(46,49,00),long=dms_to_dec(32,09,00))
se <- list(lat = dms_to_dec(46,17,00),long=dms_to_dec(33,25,00))
ne <-list(lat = nw$lat,long = se$long)
sw <-list(lat = se$lat,long = nw$long)
bbox_full <- list(
  p1 = nw,
  p2 = se
)

bbox <- bbox_full

# view bounding box.
leaflet() %>%
  addTiles() %>% 
  addRectangles(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
    lng2 = bbox$p2$long, lat2 = bbox$p2$lat,
    fillColor = "transparent"
  )



kherson_dnipro <- data.frame(lat = c(dms_to_dec(46,49,00),dms_to_dec(46,17,00)),
                             lon= c(dms_to_dec(32,09,00),dms_to_dec(33,25,00)) )|> 
  st_as_sf(coords = c("lon","lat"),crs = 4326)


kherson_roads <- osmdata::opq(st_bbox(kherson_dnipro)) %>% 
  osmdata::add_osm_feature("highway") %>% 
  osmdata::osmdata_sf() 

# Transform coordinates to new projection
kherson_lines <- sf::st_transform(kherson_roads$osm_lines,
                                  crs = raster::crs(kherson_dnipro))

# View streets as a ggplot2 render 
ggplot(kherson_lines, aes(color = osm_id)) + 
  geom_sf() +
  theme(legend.position = "none") +
  labs(title = "Open Street Map `highway` attribute in Kherson")

elev <- get_elev_raster(kherson_dnipro, src = "gl1", clip = "bbox") 
plot(elev)
kh_elmat <- raster_to_matrix(elev)
full_extent <- extent(kherson_dnipro)


base_map <- kh_elmat|> 
  sphere_shade(texture = "imhof1")

plot_rising_water <- function(water_level = 0){
  flood_elmat <- ifelse(kh_elmat < water_level,0,kh_elmat)
  base_map |>
  add_water(detect_water(flood_elmat),color="desert") |>
  add_overlay(generate_line_overlay(kherson_lines,
                                    heightmap = kh_elmat,
                                    extent=full_extent,
                                    linewidth = 2)) |>

  save_png(filename =paste0("frames/kakhova/flood_",water_level),
           title_text = paste0("Flood Inundation of the Dnipro\nAfter Kakhova Dam Destruction\nWater Level:",
           water_level," Meters"),
           title_size = 60,
           title_bar_color = "white")
  
  #plot_map()
}

0:10 |> walk(plot_rising_water)

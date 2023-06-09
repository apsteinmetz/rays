# KAKHOVKA DAM
library(tidyverse)
library(elevatr)
library(rayshader)
library(sf)
library(raster)
library(leaflet)
library(osmdata)
library(gifski)

dms_to_dec <- function(deg=0, min=0, sec=0) {
  return(deg + min / 60 + sec / 3600)
}
kh_loc <- data.frame(lat = c(dms_to_dec(46,51,00),dms_to_dec(46,18,00)),
                             lon= c(dms_to_dec(32,09,00),dms_to_dec(33,25,00)) )


# show map
leaflet() |> 
  # setView(33,46.25,zoom = 10) |> 
  fitBounds(kh_loc$lon[1],kh_loc$lat[1],kh_loc$lon[2],kh_loc$lat[2]) |> 
  addProviderTiles(providers$Esri.WorldImagery) |> 
  addProviderTiles(providers$CartoDB.PositronOnlyLabels,
                   options = providerTileOptions(opacity = 1))
  


kherson_dnipro <- kh_loc |> 
  st_as_sf(coords = c("lon","lat"),crs = 4326)


# get the topy matrix
#kherson_elev <- get_elev_raster(kherson_dnipro, src = "gl1", clip = "bbox") 
#save(kherson_elev,file="data/kherson_elev.rdata")
load(file="data/kherson_elev.rdata")


pal <- colorRampPalette(c("darkblue","limegreen"))

plot(kherson_elev, col = pal(10))




kh_elmat <- raster_to_matrix(kherson_elev)
base_map <- kh_elmat|> 
  sphere_shade(texture = "imhof1")


base_map |> 
  add_water(detect_water(kh_elmat),color="desert") |> 
  plot_map()

# kherson_roads <- osmdata::opq(st_bbox(kherson_dnipro)) %>% 
#   osmdata::add_osm_feature("highway") %>% 
#   osmdata::osmdata_sf() 
# 
# save(kherson_roads,file="data/kherson_roads.rdata")
load(file="data/kherson_roads.rdata")
# Transform coordinates to new projection
kherson_lines <- sf::st_transform(kherson_roads$osm_lines,
                                  crs = raster::crs(kherson_dnipro))

# View streets as a ggplot2 render 
ggplot(kherson_lines, aes(color = osm_id)) + 
  geom_sf() +
  theme(legend.position = "none") +
  labs(title = "Kherson Roads from Open Street Map")

# -----------------------------------------------------------
plot_rising_water <- function(water_level = 0) {
  flood_elmat <- ifelse(kh_elmat < water_level, 0, kh_elmat)
  base_map |>
    add_water(detect_water(flood_elmat), color = "desert") |>
    add_overlay(
      generate_line_overlay(
        kherson_lines,
        heightmap = kh_elmat,
        extent = extent(extent(kherson_dnipro)),
        linewidth = 2
      )
    ) |>
    
    
    save_png(
      filename = paste0("frames/kakhovka/flood_", formatC(water_level, width = 3, flag = "0"), ".png"),
      title_text = paste0(
        "Flood Inundation of the Dnipro\nAfter Kakhovka Dam Destruction\nWater Level:",
        formatC(water_level, width = 3, flag = " "),
        " Meters"
      ),
      title_size = 60,
      title_bar_color = "white"
    )
  
  
}
# -----------------------------------------------------------
# generate frames
0:10 |> walk(plot_rising_water)

# make GIF
target_dir <- "frames/kakhovka"
fnames <- paste0("frames\\kakhovka\\", dir("frames/kakhovka"))
# add a pause at beginning and end by repeating frames
fnames <- c(rep(fnames[1],5),fnames,rep(fnames[length(fnames)],5))
gifski(fnames,"frames/flood.gif",delay = 1/5,loop = TRUE)

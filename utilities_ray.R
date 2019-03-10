M <- img[ , ]
M <- ifelse(M < 0.5, 0, 1)
image(M, col = c(1, 0))

# inpsired by flood fill
# https://en.wikipedia.org/wiki/Flood_fill
unfloodfill <- function(row, col,max_depth,depth) {
    if (max_depth == depth) return()
    if (M[row, col] > 0) return()
    M[row, col] <<- depth
    unfloodfill(row - 1, col    , tcol, rcol) # south
    unfloodfill(row + 1, col    , tcol, rcol) # north
    unfloodfill(row    , col - 1, tcol, rcol) # west
    unfloodfill(row    , col + 1, tcol, rcol) # east
    return("filling completed")
  }
  
  options(expressions = 10000)
  startrow <- 100; startcol <- 100
  floodfill(startrow, startcol, 0, 2)
  
  image(M, col = c(1, 0, 2))
  
}

# Crop raster image
crop_img <- function(elev_img,bbox){
  new_extent <- unlist(bbox) %>% matrix(nrow=2,ncol=2) %>% extent()
  elev_img <- elev_img %>% crop(new_extent)
  return(elev_img)
}

# Downscale elevation matrix
downscale_elev <- function(elev_matrix,target_image_size){
  spacing_w = dim(elev_matrix)[1] / target_image_size$width
  spacing_h = dim(elev_matrix)[2] / target_image_size$height
  # downsample but truncate items if rounding returns more points than target
  # this breaks if rounding dimensions LESS than target_image_size
  sample_w <- round(seq(1,dim(elev_matrix)[1], by = spacing_w))
  sample_h <- round(seq(1,dim(elev_matrix)[2], by = spacing_h))
  return(elev_matrix[sample_w,sample_h])
  
}

#rayshader utilities from Will Bishop @wcmbiship

#' Translate the given long/lat coordinates into an image position (x, y).
#'
#' @param long longitude value
#' @param lat latitude value
#' @param bbox bounding box coordinates (list of 2 points with long/lat values)
#' @param image_width image width, in pixels
#' @param image_height image height, in pixels
#'
#' @return named list with elements "x" and "y" defining an image position
#'
find_image_coordinates <- function(long, lat, bbox, image_width, image_height) {
  x_img <- round(image_width * (long - min(bbox$p1$long, bbox$p2$long)) / abs(bbox$p1$long - bbox$p2$long))
  y_img <- round(image_height * (lat - min(bbox$p1$lat, bbox$p2$lat)) / abs(bbox$p1$lat - bbox$p2$lat))
  list(x = x_img, y = y_img)
}


#' Define image size variables from the given bounding box coordinates.
#'
#' @param bbox bounding box coordinates (list of 2 points with long/lat values)
#' @param major_dim major image dimension, in pixels. 
#'                  Default is 400 (meaning larger dimension will be 400 pixels)
#'
#' @return list with items "width", "height", and "size" (string of format "<width>,<height>")
#'
#' @examples
#' bbox <- list(
#'   p1 = list(long = -122.522, lat = 37.707),
#'   p2 = list(long = -122.354, lat = 37.84)
#' )
#' image_size <- define_image_size(bbox, 600)
#' 
define_image_size <- function(bbox, major_dim = 400) {
  # calculate aspect ration (width/height) from lat/long bounding box
  aspect_ratio <- abs((bbox$p1$long - bbox$p2$long) / (bbox$p1$lat - bbox$p2$lat))
  # define dimensions
  img_width <- ifelse(aspect_ratio > 1, major_dim, major_dim*aspect_ratio) %>% round()
  img_height <- ifelse(aspect_ratio < 1, major_dim, major_dim/aspect_ratio) %>% round()
  size_str <- paste(img_width, img_height, sep = ",")
  list(height = img_height, width = img_width, size = size_str)
}




#' Download USGS elevation data from the ArcGIS REST API.
#'
#' @param bbox bounding box coordinates (list of 2 points with long/lat values)
#' @param size image size as a string with format "<width>,<height>"
#' @param file file path to save to. Default is NULL, which will create a temp file.
#' @param sr_bbox Spatial Reference code for bounding box
#' @param sr_image Spatial Reference code for elevation image
#' 
#' @details This function uses the ArcGIS REST API, specifically the 
#' exportImage task. You can find links below to a web UI for this
#' rest endpoint and API documentation.
#' 
#' Web UI: https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage
#' API docs: https://developers.arcgis.com/rest/services-reference/export-image.htm
#'
#' @return file path for downloaded elevation .tif file. This can be read with
#' \code{read_elevation_file()}.
#'
#' @examples
#' bbox <- list(
#'   p1 = list(long = -122.522, lat = 37.707),
#'   p2 = list(long = -122.354, lat = 37.84)
#' )
#' image_size <- define_image_size(bbox, 600)
#' elev_file <- get_usgs_elevation_data(bbox, size = image_size$size)
#' 
get_usgs_elevation_data <- function(bbox, size = "400,400", file = NULL, 
                                    sr_bbox = 4326, sr_image = 4326) {
  require(httr)
  
  # TODO - validate inputs
  
  url <- parse_url("https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage")
  res <- GET(
    url, 
    query = list(
      bbox = paste(bbox$p1$long, bbox$p1$lat, bbox$p2$long, bbox$p2$lat,
                   sep = ","),
      bboxSR = sr_bbox,
      imageSR = sr_image,
      size = size,
      format = "tiff",
      pixelType = "F32",
      noDataInterpretation = "esriNoDataMatchAny",
      interpolation = "+RSP_BilinearInterpolation",
      f = "json"
    )
  )
  
  if (status_code(res) == 200) {
    body <- content(res, type = "application/json")
    # TODO - check that bbox values are correct
    # message(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
    
    img_res <- GET(body$href)
    img_bin <- content(img_res, "raw")
    if (is.null(file)) 
      file <- tempfile("elev_matrix", fileext = ".tif")
    writeBin(img_bin, file)
    message(paste("image saved to file:", file))
  } else {
    warning(res)
  }
  invisible(file)
}



#' Download a map image from the ArcGIS REST API
#'
#' @param bbox bounding box coordinates (list of 2 points with long/lat values)
#' @param map_type map type to download - options are World_Street_Map, World_Imagery, World_Topo_Map
#' @param file file path to save to. Default is NULL, which will create a temp file.
#' @param width image width (pixels)
#' @param height image height (pixels)
#' @param sr_bbox Spatial Reference code for bounding box
#' 
#' @details This function uses the ArcGIS REST API, specifically the 
#' "Execute Web Map Task" task. You can find links below to a web UI for this
#' rest endpoint and API documentation.
#' 
#' Web UI: https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute
#' API docs: https://developers.arcgis.com/rest/services-reference/export-web-map-task.htm
#'
#' @return file path for the downloaded .png map image
#'
#' @examples
#' bbox <- list(
#'   p1 = list(long = -122.522, lat = 37.707),
#'   p2 = list(long = -122.354, lat = 37.84)
#' )
#' image_size <- define_image_size(bbox, 600)
#' overlay_file <- get_arcgis_map_image(bbox, width = image_size$width,
#'                                      height = image_size$height)
#' 
get_arcgis_map_image <- function(bbox, map_type = "World_Street_Map", file = NULL, 
                                 width = 400, height = 400, sr_bbox = 4326) {
  require(httr)
  require(glue) 
  require(jsonlite)
  
  url <- parse_url("https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute")
  
  # define JSON query parameter
  web_map_param <- list(
    baseMap = list(
      baseMapLayers = list(
        list(url = jsonlite::unbox(glue("https://services.arcgisonline.com/ArcGIS/rest/services/{map_type}/MapServer",
                                        map_type = map_type)))
      )
    ),
    exportOptions = list(
      outputSize = c(width, height)
    ),
    mapOptions = list(
      extent = list(
        spatialReference = list(wkid = jsonlite::unbox(sr_bbox)),
        xmax = jsonlite::unbox(max(bbox$p1$long, bbox$p2$long)),
        xmin = jsonlite::unbox(min(bbox$p1$long, bbox$p2$long)),
        ymax = jsonlite::unbox(max(bbox$p1$lat, bbox$p2$lat)),
        ymin = jsonlite::unbox(min(bbox$p1$lat, bbox$p2$lat))
      )
    )
  )
  
  res <- GET(
    url, 
    query = list(
      f = "json",
      Format = "PNG32",
      Layout_Template = "MAP_ONLY",
      Web_Map_as_JSON = jsonlite::toJSON(web_map_param))
  )
  
  if (status_code(res) == 200) {
    body <- content(res, type = "application/json")
    message(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
    if (is.null(file)) 
      file <- tempfile("overlay_img", fileext = ".png")
    
    img_res <- GET(body$results[[1]]$value$url)
    img_bin <- content(img_res, "raw")
    writeBin(img_bin, file)
    message(paste("image saved to file:", file))
  } else {
    message(res)
  }
  invisible(file)
}


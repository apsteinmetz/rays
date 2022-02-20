# test raster plotting
library(raster)
library(tidyverse)

m1 <- matrix(1,ncol=100,nrow=120)

r1 <- raster::raster(m1)

full_extent <- extent(c(1,100,1,120))
extent(r1) <- orig_extent
# looks as we expect, all 1
plot(r1)

map_only_extent <- extent(1,100,1,110)
extent(r1)

r2 <- raster::crop(r1,map_only_extent) %>% 
  raster::extend(full_extent,value=0)

r2
m2 <- as.matrix(r2)



# set first, most southerly row, to zero
m1[1,] = 0

plot(r2)
# top, most northerly row is displayed as zero


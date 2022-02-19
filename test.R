# test raster plotting
library(raster)

m1 <- matrix(1,nrow=10,ncol=10)

r1 <- raster::raster(m1)
r1 <- setExtent(r1,c(1,10,1,10))
r1
# looks as we expect, all 1
plot(r1)

# set first, most southerly row, to zero
m1[1,] = 0

r2 <- raster::raster(m1)
r2 <- setExtent(r2,extent(r1))
r2
plot(r2)
# top, most northerly row is displayed as zero


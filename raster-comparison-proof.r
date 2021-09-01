library(raster)
library(ggplot2)
library(rasterVis)

#generate raster
r1 <- raster(xmn = 0,   # set minimum x coordinate
             xmx = 5,    # set maximum x coordinate
             ymn = 0,     # set minimum y coordinate
             ymx = 5,     # set maximum y coordinate
             res = c(1,1)) # resolution in c(x,y) direction
dat <- seq(from = 1, to = ncell(r1),by = 1)
r1[] <- dat

#generate second raster by transposing first
r2 <- t(r1)

#visualise these rasters
gplot(r1) + 
  geom_tile(aes(x, y, fill = value))
gplot(r2) + 
  geom_tile(aes(x, y, fill = value))


m <- c(1,5,1,5,10,2,10,15,3,15,20,4,20,25,5)
rclmat <- matrix(m, ncol=3,byrow=T)
rclmat

r1  <- reclassify(r1, rclmat)
r2  <- reclassify(r2, rclmat)
gplot(r1) + 
  geom_tile(aes(x, y, fill = value)) + 
  geom_text(aes(label=as.character(values(r1))))
gplot(r2) + 
  geom_tile(aes(x, y, fill = value)) +
  geom_text(aes(label=as.character(values(r2))))

matches <- r1 == r2
gplot(matches) + 
  geom_tile(aes(x, y, fill = value)) + 
  geom_text(aes(label=as.character(values(r1))))

ct <- crosstab(matches,r1)  #misses are 0s, hits are 1s
row.names(ct) <- c("dift","same") #set row.names
ct


matches1 <- r1 == r2 + 1
gplot(matches1) + 
  geom_tile(aes(x, y, fill = value)) + 
  geom_text(aes(label=as.character(values(r2))))

ct1 <- crosstab(matches1,r1)  #misses are 0s, hits are 1s
row.names(ct1) <- c("dift","same") #set row.names
ct1


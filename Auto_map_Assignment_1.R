# Author: Niene Boeijen
# November 2013
# requires rasta library
# Quick description of what the function does:
  ## Generates a map outomatically when giving country, and level. 

library(rasta)
path = "M:/My Documents/R_Lesson_1"   ## define path
map <- function(country, x){
  
  map <- raster::getData("GADM",country = country, level = x, 
                         download = TRUE, path = path)
  plot(map,bg = "dodgerblue", axes=T)
  plot(map, lwd = 15, border = "skyblue", add=T)
  plot(map, col = "green4", add = T)
  
  box(lwd = 5, col = "green4")
  mtext(side = 3, line = 1, paste("Nice map of", country), cex = 2)
  mtext(side = 1, "Longitude", line = 2.5, cex=1.1)
  mtext(side = 2, "Latitude", line = 2.5, cex=1.1)
  text(122.08, 13.22, "Projection: Geographic Coordinate System: WGS 1984 Data Source: GADM.org", adj = c(0, 0), cex = 0.7, col = "grey20")
}

# test:
map("Belgium",1)
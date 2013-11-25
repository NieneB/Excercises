# assignment 7
# Niene Boeijen
# 19-11-2013

# 0. Prepare R

library(rasta)
library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(rasterVis)
library(zoo)

# setwd("E:/R_Lesson_1/Lesson_7")

# 0.  preparing tura and sceneinfo
data(tura)
tura <- tura/10000
sceneID <-names(tura)
sceneinfo <- getSceneinfo(sceneID)
sceneinfo$year <- factor(substr(sceneinfo$date,1,4),
                         levels = c(1984:2013))
y <- freq(tura, value=NA) / ncell(tura) * 100
sceneinfo$nodata <- round(y, 2)

tura <- dropLayer(tura, which(sceneinfo$nodata == 100))
sceneinfo <- sceneinfo[which(sceneinfo$nodata != 100), ]

# 0. mean NDVI # from tura for years 2000, 2005, 2010
a <- sceneinfo$year == 2000
a <- which(a)
year2000 <- tura[[a]]

b <- sceneinfo$year == 2005
b <- which(b)
year2005 <- tura[[b]]

c <- sceneinfo$year == 2010
c <- which(c)
year2010 <- tura[[c]]

mean2000 <- calc(x = year2000, fun = mean, na.rm = TRUE)
mean2005 <- calc(x = year2005, fun = mean, na.rm = TRUE)
mean2010 <- calc(x = year2010, fun = mean, na.rm = TRUE)

# 0. new rasterBrick brick0510(mean2000, mean2005, mean2010)
brick0510 <- brick(mean2000, mean2005, mean2010)

# 1. plot of the 3 means from layers brick0510
levelplot(brick0510, names.attr =  c("Mean NDVI 2000","Mean NDVI 2005","Mean NDVI 2010"))

# 2. plot RGB
# produce RGB composite plotRGB(mean2000,mean2005, mean2010, stretch ='hist)
# use stretch ='hist' as argument
plotRGB(x=brick0510, 1,2,3, stretch ='hist', axes = TRUE, main = "RGM composite of mean NDVI, 2000, 2005, 2010")

# 3. indentify 2 change areas: 1. between 2000 -2005 2. between 2005-2010
# with drawExtent()
# e1 <- drawExtent()
# e2 <- drawExtent()
# print(e1)
# print(e2)
e1 <- extent(c(819766.5, 819999, 829640.8, 829852.2)) # REPLACE!
e2 <- extent(c(821613, 821711.6, 829358.9, 829485.8)) # REPLACE!

# 3. print polygons on map. add = TRUE
plot(e1, add = TRUE, col ="white")
plot(e2, add = TRUE, col = "black")

# 3. use polygons as research area, to make mean NDVI for each layer of the tura brick
mean1 <- extract(x=tura, y= e1, fun = mean, na.rm = TRUE)
mean2 <- extract(x=tura, y= e2, fun = mean, na.rm = TRUE)

# 4a. add NDVi to sceneinfo. 
head(mean1)
head(mean2)
sceneinfo$mean1 <- mean1
sceneinfo$mean2 <- mean2
head(sceneinfo)

# 4b. Plot 2 time series
d1 <- data.frame(date = sceneinfo$date, 
                 sensor = sceneinfo$sensor, 
                 ndvi = as.numeric(sceneinfo$mean1), 
                 zone = "region1") 
d2 <- data.frame(date = sceneinfo$date, 
                 sensor = sceneinfo$sensor, 
                 ndvi = as.numeric(sceneinfo$mean2), 
                 zone = "region2") 
d3 <- rbind(d1,d2)
d3 <-  subset(d3, sensor!="TM")

ggplot(data= d3, aes(x = date, y = ndvi)) +
  geom_point() +
  geom_line() +
  facet_wrap(facets= "zone", nrow = 2) +
  theme_bw()

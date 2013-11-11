# Author: Niene Boeijen
# November 2013
# Excercise lesson 3
# required: raster


############ set working directory ################
setwd("/My Documents/R_Lesson_1/Lesson_3")
getwd()
datdir <- file.path("Data")
datdir <- "/My Documents/R_Lesson_1/Lesson_3/Data"

############ download data ########################
download.file("http://rasta.r-forge.r-project.org/kenyashape.zip",
              file.path(datdir, "kenyashape.zip"))
unzip(file.path(datdir, "kenyashape.zip"), exdir = datdir)
kenya <- readOGR(dsn = datdir, layer = "kenya")

library(rasta)
filepath <- system.file("extdata", "anom.2000.03.tiff", package ="rasta")
temperature.raster <- raster(filepath)
#plot(temperature.raster)
#plot(kenya, add = TRUE)


########### process data ###########################
temp.kenya <- crop(temperature.raster , kenya)
samples <- sampleRandom(temp.kenya, 30, na.rm=TRUE, ext= kenya, 
             cells=TRUE, rowcol=TRUE, xy=TRUE, sp=TRUE, asRaster=FALSE)
get.mean <- mean(samples@data$anom.2000.03)
specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

########## plot ######################################
plot(temp.kenya, main = paste( "The mean annual temperature = ", specify_decimal(get.mean,2)))
plot(samples, add = TRUE, col = "green", pch = 19, cex = 1)
pointLabel(samples$x, samples$y, as.character(specify_decimal(samples$anom.2000.03,2)),cex = 0.7, allowSmallOverlap = FALSE)
plot(kenya, add = TRUE)







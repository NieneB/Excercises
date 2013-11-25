# Assignment 6. 
# Niene Boeijen
# 18-11-2013
# required: Rasta Package

library(rasta)
setwd("E:/R_Lesson_1/Lesson_6")
getwd()


###########################################################

# loading data & preprocess
data(lulcGewata) # raster layer with training areas
data(LUTGewata)  # lookup table for lulcGewatata
lulc <- as.factor(lulcGewata)  # raster values to a factor from an integer
levels(lulc) <- LUTGewata  # assign raster attribute table (RAT) to integer raster
classes <- layerize(lulc) # making layers form the classes
names(classes) <- LUTGewata$Class
LUTGewata

# plot lulcGewata with a meaningful legend (see LUTGewata)
colors <- c("#6E8B3D", "#CC8C3C", "#FFD700", "#BDB76B", "dark green", "deepskyblue")  #http://research.stowers-institute.org/efg/R/Color/Chart/ColorChart.pdf
plot(lulc, col = colors, legend = FALSE , main = "Classes of Gewata") 
legend("topright", legend=LUTGewata$Class, fill=colors)

###### draw a SpatialPolygons object in area ######

# 1. cropland
plot(lulcGewata, col=colors, legend=FALSE)
cropland <- drawPoly(sp=TRUE)
# cropland <- gUnion(cropland, drawPoly(sp=TRUE))
projection(cropland) <- projection(lulcGewata)
cropland <- SpatialPolygonsDataFrame(cropland, data=data.frame(
  class="cropland"), match.ID=FALSE)

# 2. Coffee
plot(lulcGewata, col=colors, legend=FALSE)
coffee <- drawPoly(sp=TRUE)
#coffee <- gUnion(coffee, drawPoly(sp=TRUE))
projection(coffee) <- projection(lulcGewata)
coffee <- SpatialPolygonsDataFrame(coffee, data=data.frame(
  class="coffee"), match.ID=FALSE)

# 3. bare soil
plot(lulcGewata, col=colors, legend=FALSE)
e <- drawExtent() # zoom into a bare soil
plot(lulcGewata, col=colors, legend=FALSE, ext=e)
bareSoil <- drawPoly(sp=TRUE)
#bareSoil <- gUnion(bareSoil, drawPoly(sp=TRUE))
projection(bareSoil) <- projection(lulcGewata)
bareSoil <- SpatialPolygonsDataFrame(bareSoil, data=data.frame(
  class="bare soil"), match.ID=FALSE)

# 4. Bamboo
plot(lulcGewata, col=colors, legend=FALSE)
e <- drawExtent() # zoom into a coffee area
plot(lulcGewata, col=colors, legend=FALSE, ext=e)
bamboo <- drawPoly(sp=TRUE)
#bamboo <- gUnion(bamboo, drawPoly(sp=TRUE))
projection(bamboo) <- projection(lulcGewata)
bamboo <- SpatialPolygonsDataFrame(bamboo, data=data.frame(
  class="bamboo"), match.ID=FALSE)

# 5. forest
plot(lulcGewata, col=colors, legend=FALSE)
forest <- drawPoly(sp=TRUE)
#forest <- gUnion(forest, drawPoly(sp=TRUE))
projection(forest) <- projection(lulcGewata)
forest <- SpatialPolygonsDataFrame(forest, data=data.frame(
  class="forest"), match.ID=FALSE)

# 6. wetland
plot(lulcGewata, col=colors, legend=FALSE)
wetland <- drawPoly(sp=TRUE)
#wetland <- gUnion(wetland, drawPoly(sp=TRUE))
projection(wetland) <- projection(lulcGewata)
wetland <- SpatialPolygonsDataFrame(wetland, data=data.frame(
  class="wetland"), match.ID=FALSE)

### spechify IDs ###
cropland <- spChFIDs(cropland, "cropland")
bamboo <- spChFIDs(bamboo, "bamboo")
bareSoil <- spChFIDs(bareSoil, "bare soil")
coffee <- spChFIDs(coffee, "coffee")
forest <- spChFIDs(forest, "forest")
wetland <- spChFIDs(wetland, "wetland")

# now they can be bound (2 at a time) as one object using spRbind (maptools)
trainingPoly <- spRbind(cropland, bamboo)
trainingPoly <- spRbind(trainingPoly, bareSoil)
trainingPoly <- spRbind(trainingPoly, coffee)
trainingPoly <- spRbind(trainingPoly, forest)
trainingPoly <- spRbind(trainingPoly, wetland)

########## Plotting ##########
plot(lulc, col = colors, legend = FALSE , main = "Classes of Gewata") 
legend("topright", legend=LUTGewata$Class, fill=colors)
plot(trainingPoly, add=TRUE)

writeOGR(trainingPoly,"trainingPolygon.shp",layer = 'poly',driver = "ESRI Shapefile",overwrite_layer=TRUE)

########################################################################
############## Random Forest Classification ############################

# Gewata sattelite images band 2, 3 and 4
data(GewataB2) # 
data(GewataB3) # red
data(GewataB4) # infra-red

# NDVI 
NDVI <- overlay(GewataB4, GewataB3, fun = function(x,y){(x-y)/(x+y)})
NDVI <- calc(NDVI, fun = function(x) floor(x*10000))
dataType(NDVI) <- "INT2U"
names(NDVI) <- "NDVI"

# vegetation continuous Field/ etimate of tree cover %
data(vcfGewata)
vcfGewata[vcfGewata > 100] <- NA

# make brick
Gewata <- brick(GewataB2,GewataB3,GewataB4)
Gewata_5 <- addLayer(Gewata, NDVI, vcfGewata)

# make trainingpolygon to raster
reclass <- function(x){
  which(x==levels(trainingPoly@data$class))
}
trainingPoly@data$code <- sapply(trainingPoly@data$class, FUN= reclass)
classes <- rasterize(trainingPoly, NDVI, field = "code")
dataType(classes) <- "INT1U"
plot(classes, col = colors, legend = FALSE, main ="Training polygons")
legend("topright", legend =LUTGewata$Class, fill=colors)

# mask rasterBrick to training polygons
mask <- mask(Gewata_5, classes)
names(classes) <- "class"

trainingBrick <- addLayer(mask, classes)
plot(trainingBrick)

valueTable <- getValues(trainingBrick)
valueTable <- as.data.frame(valueTable)
valueTable <- valueTable[!is.na(valueTable$class),]
valueTable$class <- factor(valueTable$class, levels = c(1:6))
valueTable$label <- with(valueTable, ifelse(class==1, "cropland",
        ifelse(class==2, "bamboo",ifelse( class==3 ,"bare soil", 
        ifelse( class == 4, "coffee plantation", ifelse( class == 5, "forest", "wetland" 
        ))))))
valueTable <- na.omit(valueTable)
modelRF <- randomForest(x= valueTable[,c(1:5)], y = valueTable$class, importance = TRUE)


############# Predict values ################
predictLC <- predict(Gewata_5 , model = modelRF, na.rm = TRUE)
plot(predictLC, col = colors, legend=FALSE)
legend("bottomright", legend=LUTGewata$Class, fill=colors, bg="white")

############# Matrices #################
# OOB confusion matrix with accuracy per class 
modelRF$confusion
colnames(modelRF$confusion) <- c("cropland", "Coffee", "Bare Soil", "Bamboo", "wetland", "forest", "class.error")
rownames(modelRF$confusion) <- c("cropland", "Coffee", "Bare Soil", "Bamboo", "wetland", "forest")
modelRF$confusion
varImpPlot(modelRF)

# 1. Which classes have the highest accuracy? Lowest?
#   Wetland has the highest accuracy, a error of 0.02.(2% if the data) The lowest accuracy is 
#   bare soil with an error of 0.6. (60%)

# 2. Is the importance ranking of the input bands different in this case to the 3-class classi-
#   cation we did earlier? If so, how, and what has changed in this case?
#   Yes, band 2 and 4 seem to have the highest impact on accuracy. Which is different form 
#   the example before, where band 3 and 4 had the highest impact. 

# 3. Can you say something about class separability? Produce a series of facet wrap plots
# using ggplot() to compare the land cover classes. Are there any classes which show high
# overlap? Is this consistent with the confusion matrix derived as part of your Random
# Forest model?
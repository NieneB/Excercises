# Assignment 5. Writing a compositing function
# by: Niene Boeijen
# 15-11-13
# required: rasta 

#. preperation ##################################################
setwd("/My Documents/R_Lesson_1/Lesson_5")
getwd()
library(raster)
library(rasta)
library(rgdal)

#. writing functions ###########################################
ComposeLandsat<- function(a,b){
  # function for cloud mask
  generate_cloudMask <- function(a){  # x = data
    a <- calc(a[[9]],fun=QA2cloud)
    return(a)
  }
  # function to remove band 9
  remove_9 <- function(a){            # x = data
    a <- dropLayer(x=a, i=9)
    return(a)  
  }
  # function to make cloud NA 
  cloud2NA <- function(a,b){          # x = data, y = cloud mask
    a[b == 1] <- NA
    return(a)
  }
    # function for mean
  averaging <- function(a,b){
    average <- mean(x = c(a,b), na.rm = TRUE) 
    return(average)
  }
  # function for vectorizing
  vec <- function(a,b){
    out <- mapply(FUN = averaging,a, b)
    return(out)
  }
  
  # call functions
  clouda <- generate_cloudMask(a)
  cloudb <- generate_cloudMask(b)
  remove_9(a)
  remove_9(b)
  img1 <- overlay(a,clouda, fun =cloud2NA)
  img2 <- overlay(b,cloudb, fun=cloud2NA)
  opar <- par(mfrow = c(2,2))
  plotRGB(img1,5,4,3, main = mtext(side = 3, line = 1, "Map of A", cex = 2))
  plotRGB(img2,5,4,3, main = mtext(side = 3, line = 1, "Map of B", cex = 2))
  average <- overlay(img1, img2, fun = vec)
  plotRGB(average,4,5,3, text = mtext(side = 3, line = 1, "Average map between A and B", cex = 2))
  par(opar)
}

#. Import data #################################################
data(taravao)
data(taravao2)
#. Run the function ############################################
taravaoComposit <- ComposeLandsat(taravao,taravao2)
#. Plot the results ############################################
# plotRGB(taravaoComposit, 4, 5, 3)  ## Already in the function
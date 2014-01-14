# Assignment 4. Harest Tracks
# Niene Boeijen
# 15-11-13
# required: rasta package
################################################################

library(rasta)

# 0. Downloading data and Preperation.
download.file("http://rasta.r-forge.r-project.org/kroonven.csv", "kroonven.csv")
borne_data = read.table("kroonven.csv", sep = ",", header = TRUE)
coordinates(borne_data) <- c("lon.degr.", "lat.degr.")
prj_string_RD <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889
 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.2369,50.0087,465.658,
-0.406857330322398,0.350732676542563,-1.8703473836068,4.0812 +units=m +no_defs")
prj_string_WGS <- CRS("+proj=longlat +datum=WGS84")
borne_data@proj4string <- prj_string_WGS
all_rd <- spTransform(borne_data, prj_string_RD)
res <- dimnames(all_rd@coords)[[2]] <- c("x", "y")    
all_rd$datetime <- as.POSIXct(paste (paste(all_rd$year, all_rd$month, all_rd$day, sep="-")
                ,paste(all_rd$hr,all_rd$min,all_rd$sec, sep=":")), tz = "Europe/Andorra")
all_rd <- as.data.frame(all_rd)
all_rd <- all_rd[order(all_rd$datetime),]
sp_lines_df <- CreateHarvestTracks(all_rd, prj_string_RD)   # where is the explanation of this function?


# 1. Create harvest blocks
sp_polys <- gBuffer(sp_lines_df,byid=T, width=0.5*sp_lines_df$width,
                    capStyle="ROUND")

# 2. Fill holes
sp_polys <- gBuffer(sp_polys, byid=T,id=rownames(sp_polys), width = 2.0)
sp_polys <- gBuffer(sp_polys, byid=T,id=rownames(sp_polys), width = -2.0)
sp_polys_df <- SpatialPolygonsDataFrame(sp_polys, sp_lines_df@data)

tmp_lines <- sp_lines_df@lines # just a list with geometries /// WHAT DO YOU DO HERE?
for (i in 2:length(sp_lines_df)){
  tmline <- sp_lines_df[i,]$datim
  for (j in 1:(i-1)){
    tmpoly <- sp_polys_df[j,]$datim
    if (difftime(tmline, tmpoly, units = "secs") > 0){
      tmp_line <- SpatialLines(tmp_lines[i], prj_string_RD)
      if (gIntersects(tmp_line, sp_polys_df[j,])){
        # compute difference
        tmp_lines[[i]] <- gDifference(tmp_line, sp_polys_df[j,])@lines[[1]]
        tmp_lines[[i]]@ID <- sp_lines_df[i,]@lines[[1]]@ID
      }
    }
  }
}
tmp_lines <- SpatialLines(tmp_lines, prj_string_RD)
cln_lines_df <- SpatialLinesDataFrame(tmp_lines, sp_lines_df@data)

sp_polys2 <- gBuffer(cln_lines_df,byid=T, width=0.5*sp_lines_df$width,
                    capStyle="ROUND")
sp_polys2 <- gBuffer(sp_polys2, byid=T,id=rownames(sp_polys), width = 2.0)
sp_polys <- gBuffer(sp_polys2, byid=T,id=rownames(sp_polys), width = -2.0)
sp_polys_df <- SpatialPolygonsDataFrame(sp_polys, sp_lines_df@data)


# 3. compute yield per hectare
sp_polys_df$Area <- gArea(spgeom = sp_polys_df, byid = TRUE)
head(sp_polys_df)
sp_polys_df$Ha <- sp_polys_df$Area/10000
sp_polys_df$YieldHa <- sp_polys_df$loads/sp_polys_df$Ha

# 4. Plot yield per hactare for each block
spplot(sp_polys_df, zcol="YieldHa", colorkey=T, zlim=c(0,100),
        pch=19,cex=0.25, main="Yield per Ha per block")   # what about the options used?

# 5. Export to google earth
setwd("/My Documents/R_Lesson_1/Lesson_4")
getwd()
sp_polys_df <- spTransform(sp_polys_df, prj_string_WGS )
writeOGR(obj=sp_polys_df, file.path("Data", "HarvestBlocks.kml"), layer= "HarvestBlocks", 
         driver= "KML", overwrite_layer = TRUE )

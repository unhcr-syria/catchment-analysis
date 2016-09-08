
##################################################
## Load geodata for syria
adm1 <- readShapePoly('geo/adm1.shp', proj4string=CRS("+proj=longlat"))
adm2 <- readShapePoly('geo/adm2.shp', proj4string=CRS("+proj=longlat"))
adm3 <- readShapePoly('geo/adm3.shp', proj4string=CRS("+proj=longlat"))
adm4 <- readShapePoints('geo/adm4.shp', proj4string=CRS("+proj=longlat"))
#
#plot(adm1)
#View(adm1@data)

adm1$syr <- "syr"
# Merge polygons by ID
syria <- unionSpatialPolygons(adm1,adm1$syr)
plot(syria)

# Fortify them
adm1@data$id = rownames(adm1@data)
adm1_f <- fortify(adm1, region="id")
adm1_f <-join(adm1_f, adm1@data, by="id")

adm2@data$id = rownames(adm2@data)
adm2_f <- fortify(adm2, region="id")
adm2_f <-join(adm2_f, adm2@data, by="id")

adm3@data$id = rownames(adm3@data)
adm3_f <- fortify(adm3, region="id")
adm3_f <-join(adm3_f, adm3@data, by="id")

adm4@data$id = rownames(adm4@data)
##  ggplot2 doesn't know how to deal with data of class SpatialPointsDataFrame
adm4_f <- fortify(adm4, region="id")
adm4_f <-join(adm4_f, adm4@data, by="id")



coords <- cbind(datasp$long, datasp$lat)
datasp0 <- SpatialPointsDataFrame(coords, data= datasp, proj4string=CRS("+proj=longlat"))
datasp1 <- SpatialPoints(coords, proj4string=CRS("+proj=longlat"))

library(deldir)
## define extent to clip voronoi (xmin, xmax, ymin, ymax)
#syriaextent <- ("35.7270","42.3850","32.3106","37.3190")
bb <- bbox(adm1)
rwsyria <- as.numeric(t(bbox(adm1)))
#This creates the voronoi line segments
voronoi <- deldir(datasp$long, datasp$lat, rw=rwsyria, plotit=TRUE )


# Carson's Voronoi polygons function
## http://stackoverflow.com/questions/24236698/voronoi-diagram-polygons-enclosed-in-geographic-borders 
voronoipolygons <- function(x, poly) {
  require(deldir)
  if (.hasSlot(x, 'coords')) {
    crds <- x@coords  
  } else crds <- x
  bb = bbox(poly)
  rw = as.numeric(t(bbox(poly)))
  z <- deldir(crds[,1], crds[,2],rw=rw)
  w <- tile.list(z)
  polys <- vector(mode='list', length=length(w))
  require(sp)
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP <- SpatialPolygons(polys)
  
  #SpatialPolygonsDataFrame( SP, data.frame(x=crds[,1], y=crds[,2],  row.names=sapply(slot(SP, 'polygons'),  function(x) slot(x, 'ID'))))  
}

#Now we can go ahead and use this to create our Voronoi SpatialPolygons.
v <- voronoipolygons(datasp0, syria)
proj4string(v) <- proj4string(syria)

#All that's left to do now is intersect the two geometries - the bread and butter of rgeos:
centervoronoi <- gIntersection(syria, v, byid=TRUE)
plot(centervoronoi)

centervoronoi@data$id = rownames(centervoronoi@data)
centervoronoi_f <- fortify(centervoronoi, region="id")

###########################################################################################
#This creates the voronoi line segments
## function for voronoi polygon
# http://stackoverflow.com/questions/12156475/combine-voronoi-polygons-and-maps
###########################################################################################


voronoipolygons <- function(x,poly) {
  require(deldir)
  if (.hasSlot(x, 'coords')) {
    crds <- x@coords  
  } else crds <- x
  bb = bbox(poly)
  rw = as.numeric(t(bbox(district)))
  z <- deldir(crds[,1], crds[,2],rw=rw)
  w <- tile.list(z)
  polys <- vector(mode='list', length=length(w))
  require(sp)
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP <- SpatialPolygons(polys)  
  voronoi <- SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1],
                                                          y=crds[,2], row.names=sapply(slot(SP, 'polygons'), 
                                                                                       function(x) slot(x, 'ID'))))
  return(voronoi)  
}

vorototal <- voronoipolygons(coords,district)
proj4string(vorototal) <- '+proj=longlat'

writeOGR(vorototal,"out","vorototal",driver="ESRI Shapefile", overwrite_layer=TRUE)

vorototalover <- over(vorototal, datasp)
#vorototalover@data$id = as.numeric(rownames(vorototalover@data))
#vorototaloverover$id = as.numeric(rownames(vorototaloverover))
#vorototaloverall <-merge(x=spp, y=vorototaloverover, by="row.names")

#names(vorototaloverpall)


#keep only individual


#writeOGR(vorototal1,"out","vorototal1",driver="ESRI Shapefile", overwrite_layer=TRUE)


#gg <- spRbind(gg1, gg0) 
#gg <- gUnion(gg1, gg0) 
# gg <-rbind(gg,gg0, fix.duplicated.IDs=TRUE)

#rm(gg0)  

########################################################
## loop on each district

poly.data <- district[1,]
uid <- as.numeric("1")
disnumber <- length(district)
for (i in 1:disnumber)
{
  districti <- district[i,]
  assign(paste("gg",i,sep=""), gIntersection(districti,vorototal,byid=TRUE))
  temp.data <- gIntersection(districti,vorototal,byid=TRUE)
  n <- length(slot(temp.data, "polygons"))
  temp.data <- spChFIDs(temp.data, as.character(uid:(uid+n-1)))
  uid <- as.numeric( uid + n)
  poly.data <- spRbind(poly.data,temp.data)
  i <- i + 1;
}

#plot(poly.data)
#summary(poly.data)
#

## Convert SpatialPolygons in SpatialPolygonsDataFrame
IDs <- sapply(slot(poly.data, "polygons"), function(x) slot(x, "ID"))
df <- data.frame(rep(0, length(IDs)), row.names=IDs)

spp <- SpatialPolygonsDataFrame(poly.data,df)

writeOGR(spp,"out","voronoiall",driver="ESRI Shapefile", overwrite_layer=TRUE)

sppover <- over(spp, datasp)
spp@data$id = as.numeric(rownames(spp@data))
sppover$id = as.numeric(rownames(sppover))
sppall <-merge(x=spp, y=sppover, by="row.names")

names(sppall)
## put 0 instead of NA to ensure that individual will be parsed as numeric
sppall@data$individual[is.na(sppall@data$individual)] <- 0

#keep only individual
sppall1 <- sppall
sppall1 <-sppall1[,-(5)]
sppall1 <-sppall1[,-(1:3)]

sppall1 <-sppall1[,-(2:3)]

#sppall1 <-sppall1[ , c("individual")]

writeOGR(sppall1,"out","voronoi",driver="ESRI Shapefile", overwrite_layer=TRUE)


#writePolyShape(gg, "voronoi")


# Load/run the main Python script
#python.load("regionalise.py")

## Unfinieshed Building -- Abandonned Building OpenSites

sum(data$Under.construction)


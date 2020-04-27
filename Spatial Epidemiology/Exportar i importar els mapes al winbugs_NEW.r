#EXPORTAR LES DADES DELS MAPES AL WINBUGS DES DEL R.

install.packages("maptools")
install.packages("spdep")
install.packages("sf")
library(sf)
library(spdep)
library(maptools)



#Cargar els paquets
library(foreign)
#library(spam)
#library(tripack)
library(boot)
library(Matrix)
library(lattice)
library(maptools)
library(sp)
library(spdep)

setwd("G:\\Master d'Estadistica i IO\\Lattice data\\Sessió 6")

##Exportar el mapa des del Splus al Winbugs.

#sp2WB:The function exports an sp SpatialPolygons object into a S-Plus map format to be import by Win-BUGS.
#sp2WB(map, filename, Xscale = 1, Yscale = Xscale, plotorder = FALSE)

#st_read::Read simple features or layers from file or database

nc <-sf::st_read(system.file("shape/nc.shp", package="sf"))
class(nc)
spd <- sf::as_Spatial(st_geometry(nc), IDs = as.character(1:nrow(nc)))

class(spd)
# [1] "SpatialPolygons"
# attr(,"package")
# [1] "sp"

## grab the data from the sf object
df <- nc
df$geometry <- NULL
df <- as.data.frame(df)

## create the SpatialPolygonsDataFrame
nc.sids <- sp::SpatialPolygonsDataFrame(spd, data = df)

#C?lcul de la tasa
rate<-(1000)*nc.sids$SID74/nc.sids$BIR74

r<-sum(nc.sids$SID74)/sum(nc.sids$BIR74)
expected<-r*nc.sids$BIR74
SMR<-nc.sids$SID74/expected
hist(SMR)
nc.sids$expected<-expected
nc.sids$SMR<-SMR


#SP2WB: The function exports an sp SpatialPolygons object into a S-Plus map format to be import by WinBUGS.

sp2WB(nc.sids, filename="North_carolina.txt")

#Results

#The Splus import file is in three parts:

#The first line contains the key word 'map' (lower case) followed by a colon and an integer, N,
#where N is the number of distinct areas in the map (note that one area can consist of more than one polygon).
#The 2nd and 3rd lines are optional, and can be used to specify the units for the map scale.
#By default, GeoBUGS assumes that the polygon coordinates are measured in metres.
#If the coordinates are measured in kilometres, say, then specify Xscale and Yscale to be 1000.
#GeoBUGS will then multiply all polygon co-ordinates by Xscale and Yscale as appropriate before storing the map file.
#If Xscale and Yscale are not specified, then the default units (metres) are assumed.


#The next part of the import file is a 2 column list giving:

#(column 1) the numeric ID of the area - this must be a unique integer between 1 and N;
#the areas should be labelled in the same order as the corresponding data for that area appears in the model.
#(column 2) the area label - this must start with a character, and can be a maximum of 79 alphanumeric characters (no spaces allowed)

#The final part of the import file is a 3 column list giving the co-ordinates of the polygons. The format is:

#(col 1) the label of the area to which the polygon belongs
#(col 2) x-coordinate
#(col 3) y-xoordinate

#The polygon coordinates can be listed either clockwise or anticlockwise. Polygons should be separated by a row of NA's
#The import file should end with the key word:   END

##### Obrir el fitxer exportat en el winbugs i tot seguit Map- Import Splus.


# map:100
# Xscale:1
# Yscale:1
# 
# 1 area0
# 2 area1
# 3 area2
# 4 area3
#  ...............
# 
# area0 -79.2462 35.8682 
# area0 -79.2380 35.8372 
# area0 -79.5410 35.8370 
# area0 -79.5378 35.8910 
# area0 -79.5306 36.2362 
# area0 -79.5305 36.2461 
# area0 -79.2585 36.2357 
# area0 -79.2598 36.0479 
# area0 -79.2708 35.9046 
# area0 -79.2462 35.8682 
# NA NA NA
# area1 -81.1089 35.7719 
# area1 -81.1273 35.7890 
# area1 -81.1414 35.8233 
# area1 -81.3281 35.7951 
# ..........................
# 
# END

#ANAR AL WINBUGS I IMPORTAR SPLUS





###########Scottish data analysis########################################333
#Data is in winbugs needs to export into Rpakacge 

#The  rates of lip cancer in 56 counties in Scotland have been analysed 
#by Clayton and Kaldor (1987) and Breslow and Clayton (1993).  
#The form of the data includes the observed and expected cases (expected 
#numbers based on the population and its age and sex distribution in the county), 
#a covariate measuring the percentage of the population engaged in agriculture, 
#fishing, or forestry. 


N <-56
O <- c(9,39,11,9,15,8,26,7,6,20, 13,5,3,8,17,9,2,7,9,7,16,31,11,7,19,15,7,10,16,11, 5,3,7,8,11, 9,11, 8,6,4,
      10, 8,2,6,19,3,2,3,28,6,1,1,1,1,0,0)

E <- c( 1.4, 8.7, 3.0, 2.5, 4.3, 2.4, 8.1,2.3, 2.0, 6.6,4.4, 1.8, 1.1, 3.3,7.8, 4.6, 1.1,4.2,5.5, 4.4,
        10.5,22.7,8.8,5.6,15.5,12.5,6.0,9.0,14.4,10.2,4.8,2.9,7.0, 8.5,12.3,10.1,12.7,9.4,7.2,5.3,
        18.8,15.8,4.3,14.6,50.7,8.2,5.6,9.3,88.7,19.6,3.4,3.6,5.7,7.0,4.2,1.8)

X <- c(16,16,10,24,10,24,10, 7, 7,16, 7,16,10,24, 7,16,10, 7, 7,10, 7,16,10, 7, 1, 1, 7, 7,10,10,
        7,24,10, 7, 7, 0,10, 1,16, 0,1,16,16, 0, 1, 7, 1, 1, 0, 1,1, 0, 1, 1,16,10)

#Creation dataset.

data.lips<-data.frame(O,E,X=X/10)
########################Generalized linear models: Poisson #############################

glm(formula, family = gaussian, data, weights, subset,
    na.action, start = NULL, etastart, mustart,
    offset, control = glm.control(...), model = TRUE,
    method = "glm.fit", x = FALSE, y = TRUE, contrasts = NULL,
    ...)

results.lips<-glm(O~X,offset=log(E),family=poisson,data=data.lips)
summary(results.lips)
deviance(results.lips) /54
residuals(results.lips,type="deviance")
data.lips$residuals<-residuals(results.lips,type="deviance")

#The overdispersion is caused by spatial correlation


#ReadSplus Read exported WinBUGS maps.
#Exporto amb el winbugs el fitxer d'escocia.
#1. En el winbugs obrir el mapa a exportar i despr?s Map- Export Splus.
scotland<-readSplus("scotland.txt")
plot(scotland)
#Assigno names a les files del data.frame .
data.lips
row.names(data.lips) <- sapply(slot(scotland, "polygons"), slot, "ID")
nc_scotland <- SpatialPolygonsDataFrame(scotland,data=as(data.lips, "data.frame"))
slot(nc_scotland,"data")
nc_scotland$residuals<-residuals(results.lips,type="deviance")

#Spatial weights for neighbours lists

#Poly2nb:Construct neighbours list from polygon list based on contiguous boundaries
xxnb <- poly2nb(nc_scotland)
#nb2listw:The function supplements a neighbours list with spatial weights 
w.scotland<-nb2listw(xxnb, style="W", zero.policy=TRUE)

scotland.moran.mc<- moran.mc(nc_scotland$residuals, listw=w.scotland, nsim=100,zero.policy=TRUE)
scotland.moran.mc



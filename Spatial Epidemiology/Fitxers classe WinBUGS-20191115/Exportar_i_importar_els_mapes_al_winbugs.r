#EXPORTAR LES DADES DELS MAPES AL WINBUGS DES DEL R.

#Cargar els paquets
library(foreign)
library(spam)
library(tripack)
library(boot)
library(Matrix)
library(lattice)
library(maptools)
library(sp)
library(spdep)


#sp2WB:The function exports an sp SpatialPolygons object into a S-Plus map format to be import by Win-BUGS.
#sp2WB(map, filename, Xscale = 1, Yscale = Xscale, plotorder = FALSE)

nc.sids <- readShapePoly(system.file("etc/shapes/sids.shp", package="spdep")[1],proj4string=CRS("+proj=longlat +ellps=clrk66"))
plot(nc.sids, border="blue", axes=TRUE, las=1)

#SP2WB: The function exports an sp SpatialPolygons object into a S-Plus map format to be import by WinBUGS.

sp2WB(nc.sids, filename="I:\\datos\\North_carolina.txt")

#Results
map:100
Xscale:1
Yscale:1

1 area0
2 area1
3 area2
4 area3
 ...............

area1 -81.1089 35.7719
area1 -81.1273 35.7890
area1 -81.1414 35.8233
area1 -81.3281 35.7951
area1 -81.3372 35.8276
area1 -81.3307 35.8758
area1 -81.3396 35.9292
area1 -81.3293 35.9892
area1 -81.3219 35.9893
area1 -81.2360 36.0238
area1 -81.1575 36.0210
area1 -81.1241 36.0313
area1 -81.0841 36.0208
area1 -81.0206 36.0349
area1 -80.9953 35.9771
area1 -81.0491 35.8360
area1 -81.1089 35.7719
NA NA NA
area2 -81.2399 36.3654
area2 -81.2407 36.3794
..........................

END

#The Splus import file is in three parts:

#The first line contains the key word 'map' (lower case) followed by a colon and an integer, N, where N is the number of distinct areas in the map (note that one area can consist of more than one polygon).
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





###########Anàlisis dades escocia.
#AQUESTES DADES ESTAN AL WINBUGS PER TANT HE TINGUT QUE IMPORTAR AL R.

N <-56
O <- c(    9,   39,   11,    9,   15,    8,   26,    7,    6,   20, 13,    5,    3,    8,   17,    9,    2,    7,    9,    7,
16,   31,   11,    7,   19,   15,    7,   10,   16,   11, 5,    3,    7,    8,   11,    9,   11,    8,    6,    4,
10,    8,    2,    6,   19,    3,    2,    3,   28,    6, 1,    1,    1,    1,    0,    0)

E <- c( 1.4, 8.7, 3.0, 2.5, 4.3, 2.4, 8.1, 2.3, 2.0, 6.6,4.4, 1.8, 1.1, 3.3, 7.8, 4.6, 1.1, 4.2, 5.5, 4.4,
           10.5,22.7, 8.8, 5.6,15.5,12.5, 6.0, 9.0,14.4,10.2,
            4.8, 2.9, 7.0, 8.5,12.3,10.1,12.7, 9.4, 7.2, 5.3,
           18.8,15.8, 4.3,14.6,50.7, 8.2, 5.6, 9.3,88.7,19.6,
            3.4, 3.6, 5.7, 7.0, 4.2, 1.8)

X <- c(16,16,10,24,10,24,10, 7, 7,16,
              7,16,10,24, 7,16,10, 7, 7,10,
              7,16,10, 7, 1, 1, 7, 7,10,10,
              7,24,10, 7, 7, 0,10, 1,16, 0,
              1,16,16, 0, 1, 7, 1, 1, 0, 1,
              1, 0, 1, 1,16,10)

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


#ReadSplus Read exported WinBUGS maps.
#Exporto amb el winbugs el fitxer d'escocia.

scotland<-readSplus("I:\\datos\\scotland.txt")
plot(scotland)
#Assigno names a les files del data.frame .

row.names(data.lips) <- sapply(slot(scotland, "polygons"), slot, "ID")
nc_scotland <- SpatialPolygonsDataFrame(scotland,data=as(data.lips, "data.frame"))
slot(nc_scotland,"data")

#Spatial weights for neighbours lists
#Poly2nb:Construct neighbours list from polygon list based on contiguous boundaries

xxnb <- poly2nb(nc_scotland)
w.scotland<-nb2listw(xxnb, glist=NULL, style="W", zero.policy=TRUE)

lm.morantest(results.lips, w.scotland,zero.policy=TRUE)
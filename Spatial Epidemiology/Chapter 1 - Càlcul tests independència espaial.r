#Cargar els paquets
#install.packages("maptools") #no estadistico
#install.packages("spdep") #para hacer stats
#install.packages("sf") #para que R entienda datos geo
library(sf)
library(spdep)
library(maptools)



#legenda:colour palletes rainbow, grey.colours,heat.colours,terrain.colours topo.colours,cm.colours( cyan-magenta default)
library(class)
#install.packages("RColorBrewer")
library(RColorBrewer)     #Creates nice looking color palettes especially for thematic maps
#class intervals.
library(e1071)
library(classInt)

#*******************************************.
#Importar les dades de les regions  de North Carolina.
#Tres fitxes amb extensions:
#.shp (datos geomgraficos) 
#.shx  (indexos espaials)  
#.dbf (atributs de les dades)  

####################################################################.
#DATA: 100 COUNTIES OF NORTH CAROLINA INCLUDES COUNTS OF NUMBERS OF LIVER BIRTHS
#(ALSO NON-WHITE LIVE BIRTHS) AND NUMBERS OF SUDDEN INFANT DEATHS, FOR 1974-1978 AND 1979-1984.
#THE COUNTY SEAT LOCATION COORDINATES ARE GIVEN IN MILES.


#The nc.sids data frame has 100 rows and 21 columns. It contains data given in Cressie (1991,
#pp. 386-9), Cressie and Read (1985) and Cressie and Chan (1989) on sudden infant deaths in North
#Carolina for 1974-78 and 1979-84. The data set also contains the neighbour list given by Cressie
#and Chan (1989) omitting self-neighbours (ncCC89.nb), and the neighbour list given by Cressie
#and Read (1985) for contiguities (ncCR85.nb). The data are ordered by county ID number, not
#alphabetically as in the source tables sidspolys is a "polylist" object of polygon boundaries, and
#sidscents is a matrix of their centroids.
#Usage

#Format
#This data frame contains the following columns:
#SP_ID SpatialPolygons ID
#CNTY_ID county ID
#east eastings, county seat, miles, local projection
#north northings, county seat, miles, local projection
#L_id Cressie and Read (1985) L index
#M_id Cressie and Read (1985) M index
#names County names
#AREA County polygon areas in degree units
#PERIMETER County polygon perimeters in degree units
#CNTY_ Internal county ID
#NAME County names
#FIPS County ID
#FIPSNO County ID
#CRESS_ID Cressie papers ID
#BIR74 births, 1974-78
#SID74 SID deaths, 1974-78
#NWBIR74 non-white births, 1974-78
#BIR79 births, 1979-84
#SID79 SID deaths, 1979-84
#NWBIR79 non-white births, 1979-84


#st_read::Read simple features or layers from file or database

nc <-sf::st_read(system.file("shape/nc.shp", package="sf"))
class(nc)
#convertir para compatibilidad
spd <- sf::as_Spatial(st_geometry(nc), IDs = as.character(1:nrow(nc)))

#interesting web page: https://cran.r-project.org/web/packages/sf/vignettes/sf5.html 

class(spd)
# [1] "SpatialPolygons"
# attr(,"package")
# [1] "sp"

## grab the data from the sf object
df <- nc
df
df$geometry <- NULL #quito geometry, es info de poligonos que recuperaremos
df <- as.data.frame(df)

## create the SpatialPolygonsDataFrame
#le digo la coordenadas de los poligonos y la info que me interes
nc.sids <- sp::SpatialPolygonsDataFrame(spd, data = df )
nc.sids
summary(nc.sids)

slot(nc.sids,"data") #para acceder a los datos en el objeto de nc.sids

#Calcul de la tasa
rate<-(1000)*nc.sids$SID74/nc.sids$BIR74 #no hace falta usar slots para acceder
#ya no tengo un objeto con SpatialPolygons sinon un vector, lo puedo integrar otra vez
hist(rate)

#Freeman-Tukey Transfomation
#para arreglar asimetria
ft.SID74 <- sqrt(1000)*(sqrt(nc.sids$SID74/nc.sids$BIR74) + sqrt((nc.sids$SID74+1)/nc.sids$BIR74))
summary(ft.SID74)
shapiro.test(ft.SID74)
hist(ft.SID74)

#Adjuntar al fitxer 
#ahi agrego las variables agregadas al objeto nc-sids
nc.sids$ft.SID74<-ft.SID74   
nc.sids$rate<-rate
plot(nc.sids, border="blue", axes=TRUE, las=1)
text(coordinates(nc.sids),label=nc.sids$NAME,cex=0.5)
#plot solo pone la fronteras

#Plot of the ft.SID74
brks <- round(quantile(nc.sids$ft.SID74, probs=seq(0,1,0.2)), digits=2)   
#breaks
colours <- c("yellow", "orange2", "red3", "brown", "black")
#voy a pintar los poligonos segun si se ubican en primer, segundo...quintile
plot(nc.sids, col=colours[findInterval(nc.sids$ft.SID74, brks,all.inside=TRUE)])
#findInterval encuentra en qu? intervalo se ubica cada dato
legend(x=c(-84, -80), y=c(33, 34.5), legend=leglabs(brks),fill=colours, bty="n",cex=0.5)
#leglabs de map tools  makes character strings from the same break points
title(main=paste("Rate (Transformed FT) in North Carolina","For the 1974-1978 period"))
text(coordinates(nc.sids),label=as.factor(nc.sids$NAME),cex=0.5)

#Utilitzant la paleta 

colours<-gray.colors(5,0.95,0.2)
#alternativas a gray
colours<- rev(terrain.colors(5))
colours<- terrain.colors(5)

plot(nc.sids, col=colours[findInterval(nc.sids$ft.SID74, brks,all.inside=TRUE)])
legend(x=c(-84, -80), y=c(33, 34.5), legend=leglabs(brks),fill=colours, bty="n",cex=0.5)
invisible(title(main=paste("Rate (Transformed FT) in North Carolina","For the 1974-1978 period")))
text(coordinates(nc.sids),label=as.factor(nc.sids$NAME),cex=0.5)


#Poly2nb:Construct neighbours list from polygon list based on contiguous boundaries

xxnb <- poly2nb(nc.sids) #he function builds a neighbours list based on regions with contiguous boundaries
plot(nc.sids)#, border="grey")   
plot(xxnb, coordinates(nc.sids), add=TRUE, col="blue")
nc.sids$NAME[1]
xxnb[[1]] #da los vecinos de la region 1
nc.sids$NAME[xxnb[[1]]] #da los nombres de los vecinos de la region 1

cards <- card(xxnb)  #Contar la cantidad de vecinos de cada region
sort(cards)
summary.nb(xxnb)
which(cards == max(cards))
maxconts <- which(cards == max(cards))[1]
nc.sids$NAME[maxconts]
nc.sids$FIPSNO[maxconts]
#if(length(maxconts) > 1) maxconts <- maxconts[1]
fg <- rep("grey", length(cards)) #vector de strings grey
fg[maxconts] <- "red" #salvo el maximo que tiene red
fg[xxnb[[maxconts]]] <- "green" #green para los vecinos del maximo
plot(nc.sids, col=fg)
text(coordinates(nc.sids), label=nc.sids$NAME,cex=0.5)
title(main="Region with largest number of contiguities")

maxconts <- which(cards == max(cards))[2]
nc.sids$NAME[maxconts]
#if(length(maxconts) > 1) maxconts <- maxconts[1]
fg <- rep("grey", length(cards))
fg[maxconts] <- "red"
fg[xxnb[[maxconts]]] <- "green"
plot(nc.sids, col=fg)
text(coordinates(nc.sids), label=nc.sids$NAME,cex=0.5)
title(main="Region with largest number of contiguities")

#nb2listw:The function supplements a neighbours list with spatial weights 
#for the chosen coding scheme.

#B:Binary (1: neighbour, 0:otherwise)
#W: standardised to sum unity row.
class(xxnb)
#necesita objeto de tipo nb como xxnb
w.sids<-nb2listw(xxnb, glist=NULL, style="W",  zero.policy=TRUE)   #Spatial weights for neighbours lists
b.sids<-nb2listw(xxnb, glist=NULL, style="B",  zero.policy=TRUE)   #Spatial weights for neighbours lists


w.sids$weights
summary(unlist(w.sids$weights))
summary(sapply(w.sids$weights,sum))

b.sids$weights
summary(unlist(b.sids$weights))
summary(sapply(b.sids$weights,sum))

#General spatial weights.
#glist:  argument (belive that the strenght of neighbour relationship attenuates with the distance.
#weights to be proportional to the inverse distance between points representing the areas

dsts<-nbdists(xxnb,coordinates(nc.sids))  #Distancies entre veins.
dsts
dsts[[1]]
#idw<-lapply(dsts,function(x) 1/(x/10))    #Inversa de la distancia 
idw<-lapply(dsts,function(x) 1/(x))    #Inversa de la distancia 

idw[[1]]
d.sids<-nb2listw(xxnb, glist=idw,style="B")
attributes(d.sids)

unlist(d.sids$weights)[1:6]
unlist(w.sids$weights)[1:6]

#siempre se usa estandarizado por fila, es lo m?s com?n
#no se cambian tanto los pesos

 #Moran's I test for spatial autocorrelation

sids.moran<-moran.test(nc.sids$ft.SID74 ,w.sids)
sids.moran 

sids.moran2<-moran.test(nc.sids$ft.SID74 ,b.sids)
sids.moran2 


sids.moran3<-moran.test(nc.sids$rate ,w.sids)
sids.moran3 
#la funcion da el estadistico I, y el estandarizado I-E(I)/SD(I)
#contraste unilateral por default

#Permutation test for Moran's I statistic
#el test de Moran supone que la variable es normal, por eso hicimos una transformacion
# para no transformar podemos usar un test de permutacion

nsim <- 1000
set.seed(1234)

sids.moran.mc<-moran.mc(nc.sids$rate, listw=w.sids, nsim=nsim)
sids.moran.mc
#la permutation rompe la estructura
sids.moran.mc$res #el valor dado en cada permutation
mean(sids.moran.mc$res[1:nsim])
var(sids.moran.mc$res[1:nsim])
hist(sids.moran.mc$res)
max(sids.moran.mc$res[1:nsim])
#el estadistico tomar valor 0.23 en nuesta muestra
#en le test de permutacion, rompiendo la estructura dio un maximo de 0.17 con 1001 permutaciones
#estamos muy arriba de eso


#geary.test Geary's C test for spatial autocorrelation
#va al reves del Moran's, spatial indepedenc es alrdedor del 1

sids.geary<-geary.test(nc.sids$ft.SID74, w.sids)
sids.geary
sids.geary.mc<-geary.mc(nc.sids$rate, w.sids, nsim)
#son los valores de geary original antes de la trasnformacion
#para interpretar
sids.geary.mc
#ac? te dice que el valor obtenido es menor a todos los obtenidos con la permutaciones
#observed rank =1
sids.geary.mc$res

mean(sids.geary.mc$res[1:nsim])
var(sids.geary.mc$res[1:nsim])
hist(sids.geary.mc$res)
min(sids.geary.mc$res[1:nsim])
#Arriba de 1 spatial correlation negativa
#Debajo de 1 spatial correlation positiva


#Scatterplot moran Tests
st.ft.SID74<-(nc.sids$ft.SID74-mean(nc.sids$ft.SID74))/sd(nc.sids$ft.SID74) #estandarizo
xx<-moran.plot(st.ft.SID74,w.sids ,labels=as.factor(nc.sids$NAME), pch=19)
#linea es el coeficiente de correlacion de Morgan
#marca potenciales outliers  en particular en los cuadrantes superior izquierdo y inferior derecha
#indican correlacion negativa 
xx<-moran.plot(nc.sids$ft.SID74,w.sids ,labels=as.factor(nc.sids$NAME), pch=19)


load("session1.RData")
#Local indicators.

sids.local.moran<-localmoran(nc.sids$ft.SID74, w.sids)
head(sids.local.moran)
data.frame(nc.sids$NAME,round(sids.local.moran,4))  #canviat avissar

#Adjunto resultats I moran al spatial polygon dataframe

slot(nc.sids,"data")<-cbind(slot(nc.sids,"data"),sids.local.moran)
slot(nc.sids,"data")

#Plot els I 
#creación  d'intervals
#Definición  amb amb els quartils
#I.q5<-classIntervals(round(slot(nc.sids,"data")$Ii,digits=3),n=5,style="quantile",digits=2)
l.inf<-round(min(nc.sids$Ii),digits=2)
l.sup<-round(max(nc.sids$Ii),digits=2)
l.inf
l.sup
library(classInt)

#I.q5<-classIntervals(nc.sids$Ii,style="fixed",fixedBreaks=c((l.inf-0.1),-0.5,0,1.6,(l.sup+0.1)),digits=4)

#I.q5<-findInterval(nc.sids$Ii,c((l.inf-0.1),-0.5,0,1.6,(l.sup+0.1)))

#hizo los intervalos a gusto
I.q4<-findInterval(nc.sids$Ii,c((l.inf-0.1),-0.5,0,1.6,(l.sup+0.1)))
I.q4
breakI<-c((l.inf-0.1),-0.5,0,1.6,(l.sup+0.1))
breakI #dio cuatro intervalos

#creación paleta de colors.
#pal.I<-gray.colors(5,0.95,0.55)
pal.I<- c("yellow", "orange2", "red3", "brown")
#Assingación dels colors en funci? dels intervals
windows()

plot(nc.sids, col=pal.I[I.q4])
legend("topright", legend=leglabs(breakI),fill=pal.I, bty="n")

#lo que preocupa son los amarillos, porque no correlcacionan con sus vecinos

#Plot els p-valores
Ip.2<-findInterval(nc.sids$"Pr(z > 0)",c(0,0.05, 1))
breaksp<-c(0,0.05, 1)
pal.p<- gray.colors(2,0.4,0.95)

plot(nc.sids, col=pal.p[Ip.2])
legend("topright", legend=leglabs(breaksp),fill=pal.p, bty="n")


#Els dos gráfics junts.
windows()
par(mfrow=c(2,1),mai=c(0.5, 0.5, 0.5, 0))
plot(nc.sids, col=pal.I[I.q4])
legend("bottomright", legend=leglabs(breakI),fill=pal.I, bty="n")
plot(nc.sids, col=pal.p[Ip.2])
legend("bottomright", legend=leglabs(breaksp),fill=pal.p, bty="n")

#los amarillos preocupan, pero no es significativa la correlacion negativa
# marones->clusters de correlacion espacial. Agregacion de regiones con tasas más parecidas
# el nivel de significacion global enorme, problema de multiplicidad, tomar con pinza. no hice
# correcion por Bonferroni. Correcion Scheffe no existe por eso.

#El gráfic de les taxes.
par(mfrow=c(1,1))
windows()
plot(nc.sids, col=colours[findInterval(nc.sids$ft.SID74, brks,all.inside=TRUE)])
legend(x=c(-84, -80), y=c(33, 34.5), legend=leglabs(brks),fill=colours, bty="n")
text(coordinates(nc.sids),label=as.factor(nc.sids$names),cex=0.55)
invisible(title(main=paste("Rate (Transformed FT) in North Carolina","For the 1974-1978 period")))





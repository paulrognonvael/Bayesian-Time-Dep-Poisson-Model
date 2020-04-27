#install.packages("spdep")
#install.packages("maptools")
install.packages("sf")
rm(list=ls())
library(sf)
library(foreign)
library(spdep)
library(sp)
library(maptools)
library(MASS)


######################################################################
#New York leukemia data taken from the data sets supporting Waller and Gotway 2004


# A data frame with 281 observations on the following 12 variables, and the binary coded spatial weights used in the source.
# AREANAME: name of census tract
# AREAKEY:unique FIPS code for each tract
# X:x-coordinate of tract centroid (in km)
# Y:y-coordinate of tract centroid (in km)
# POP8:population size (1980 U.S. Census)
# TRACTCAS:number of cases 1978-1982
# PROPCAS:proportion of cases per tract
# PCTOWNHOME:percentage of people in each tract owning their own home
# PCTAGE65P:percentage of people in each tract aged 65 or more
# Z:transformed propoprtions
# AVGIDIST:average distance between centroid and TCE sites (TCE:trichloroethylene is a halocarbon commonly used as an industrial solvent.)
# PEXPOSURE:"exposure potential": inverse distance between each census tract centroid and the nearest TCE site, IDIST, transformed via log(100*IDIST)


#############################################################################################################
    
#Ajust de models: SAR i CAR.
#spautolm: estima els par?metres d?un model SAR o CAR per m?xima versemblan?a .
#Els par?metres d?entrada m?s importants de la funci?:
#listw: definici? de ve?ns.
#formula: indica el model a ajustar en la tend?ncia. Es fa igual que en la funci? lm.
#family: model SAR o CAR.

#install.packages("spdep")
#install.packages("maptools")


library(spdep)

data(NY_data)

#Creaci? matriu de veins:
nydata <- read.dbf(system.file("etc/misc/nydata.dbf", package="spdep")[1])

head(nydata)
coordinates(nydata) <- c("X", "Y")
plot(nydata)
nyadjmat <- as.matrix(read.dbf(system.file("etc/misc/nyadjwts.dbf",package="spdep")[1])[-1])

ID <- as.character(names(read.dbf(system.file("etc/misc/nyadjwts.dbf",package="spdep")[1]))[-1])
identical(substring(ID, 2, 10), substring(as.character(nydata$AREAKEY), 2, 10))

# Convert a square spatial weights matrix to a weights list object
#para que lo entienda el R, matriz de vecinos, matriz de linkaje
nyadjlw <- mat2listw(nyadjmat, as.character(nydata$AREAKEY))

#Spatial weights for neighbours lists
listB_NY <- nb2listw(nyadjlw$neighbours, style="B") #B: Binary, 0 no vecino, 1 si vecino

#Ajust d?un model amb tend?ncia.  (Z:transformed log(1000(Y+1)/n))
hist(nydata$Z)
nylm<-lm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME,data=nydata)
summary(nylm)

#PEXPOSURE no asociado, el resto sí

#Test I Moran als residus del model anterior.
lm.morantest(nylm, listB_NY)
#no importa sacar la variable no significativa porque total el modelo es incorrecto porque hay
#autocorrelacion espacial. No independencia. Entonces no puedo interpretar el p-valor

#No testeo la tendencia en x y y de centroides porque los valores variable estudio son agregados
#no representan incidencia en una coordenada

#rechazo ausencia de autocorrelacion

#Matrix pero canviant els pesos:row standardized
listW_NY <- nb2listw(nyadjlw$neighbours, style="W") #W:row standardized
lm.morantest(nylm, listW_NY)
#rechazo ausencia de autocorrelacion

#Model SARerror:
#para hacer una regresion tomando en cuenta la autocorrelacion
nysarB<- spautolm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, listw= listB_NY ,family="SAR",data=nydata)
#veamos si ajusta bien el modelo
summary(nysarB)
#Hace LR test para saber si da matriz cov simetrica
summary(nylm)
#resultados cambian tomando cuenta la autocorrelación

#con otra matrix de varianza
#la estandarizada por filas, está mejor
nysarW<- spautolm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, listw= listW_NY ,family="SAR",data=nydata)
summary(nysarW)
#comparo los AIC

#Model CARerror:
nycarB<- spautolm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, listw= listB_NY ,family="CAR",data=nydata)
summary(nycarB)
nycarW<- spautolm(Z~PEXPOSURE+PCTAGE65P+PCTOWNHOME, listw= listW_NY ,family="CAR",data=nydata)
summary(nycarW)
 
# Como decidir entre SAR y CAR? Muy parecidos. Uno más para economia, otro para salud
# Importa mas la matriz de vecinos- estandarizados mejor porque mas interpretable


##########################################################.
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
# east eastings, county seat, miles, local projection
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

#Read shapefile into Map object; the file should be given including its ".shp" extension, and the
#function will reconstruct the names of the database (dbf) file and the index (shx) file from these.
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
hist(rate)


###Calcul esperats
r<-sum(nc.sids$SID74)/sum(nc.sids$BIR74)
expected<-r*nc.sids$BIR74
SMR<-nc.sids$SID74/expected
hist(SMR)
nc.sids$expected<-expected
nc.sids$SMR<-SMR

result.pois<-glm(SID74~1+offset(log(expected)),data=nc.sids,family="poisson") #cuidado que el offset es log(expected)
summary(result.pois) #no coeff, solo para ver overdispersion 203/99=approx 2, mean is two times the variance, it should be one
residuals(result.pois,type="deviance")
nc.sids$residuals<-result.pois$residuals

#La varianza es algo mas de dos veces la media. La estimacion del parametro de
#dispersion no es mas que la suma de los residuos de Pearson dividida por los grados
#de libertad residuales.
#The quasibinomial and quasipoisson families differ from the binomial and poisson families only in that the dispersion parameter is not fixed at one, so they can model over-dispersion. 

log.fit.over <- glm(SID74~1+offset(log(expected)), family = quasipoisson(link = log),data = nc.sids) 
#quasi poisson para estimar overdispersion, obtenemos mas o menos la misma, modelamos la phi en Var(y)=phi*E(y)
summary(log.fit.over)
summary.glm(log.fit.over)$dispersion
summary(result.pois)

###############################################
#Binomial Negative.

result.BN <- glm.nb(SID74~1+offset(log(expected)), data = nc.sids)
summary(result.BN)
summary(result.pois)

106.46/99
####################################################
#Zero inflated poisson.
install.packages("pscl")
library(pscl)

result.zero.inf<-zeroinfl(SID74~1+offset(log(expected)), data = nc.sids,dist = "poisson", link = "logit")
summary(result.zero.inf)   
summary(result.BN)
summary(result.pois)

##############################.
#Calculate test de moran from residuals of glm model. model.frame()
#########################################3
#Poly2nb:Construct neighbours list from polygon list based on contiguous boundaries

#para pintar los vecinos
xxnb <- poly2nb(nc.sids)    
plot(nc.sids)#, border="grey")   
plot(xxnb, coordinates(nc.sids), add=TRUE, col="blue")


#nb2listw:The function supplements a neighbours list with spatial weights 
#for the chosen coding scheme.

#B:Binary (1: neighbour, 0:otherwise)
#W: standardised to sum unity row.

#compute the spatial weights
w.sids<-nb2listw(xxnb, glist=NULL, style="W",  zero.policy=TRUE)   #Spatial weights for neighbours lists
sids.moran.mc<- moran.mc(nc.sids$SMR, listw=w.sids, nsim=100) #mc porque no normal distributed (creo)

sids.moran.mc #YES there is spatail distribution
xx<-moran.plot(nc.sids$residuals,w.sids ,labels=as.factor(nc.sids$names), pch=19)






library(devtools)
install_github("mdjahan/3PGHydro/rpackage.3PGHydro/")
library(rpackage.3PGHydro)
?run_3PGhydro
#
setwd("C:/Users/jfelb/Documents/GitHub/3PGHydro/example/")

#climate data
climate <- read.csv("Hornberg_climate.csv")
climate$date <- as.Date(climate$date,format="%d/%m/%Y")

#load 3PG parameters
Parameter <- read.csv("parameter_pabies.csv") #P.abies from Forrester et al. 2021; F. sylvatica from Augustynczik et al 2017
#select species
p <- Parameter[,2]

#Site & Stand characteristics
lat <- 48.2
StartDate <- "01/01/1960"
StandAgei <- 30
EndAge <- 81
StemNoi <- 1436
WSi <- 99
WFi <- 15
WRi <- 15
CO2Concentration <- "Historical"
FR <- 0.4
HeightEquation <-1
SVEquation <- 1
#Soil
SoilClass <- 2
EffectiveRootZoneDepth <- 0.6
DeepRootZoneDepth <- 3
RocksER <- 0.2 
RocksDR <- 0.4

#Management
thinAges <- c(35,40,45,50,55,60,65,70,75,80)
thinVals <- c(1271,1106,941,776,710,644,578,513,481,449)
thinWF <- rep(1,10)
thinWR <- rep(1,10)
thinWS <- rep(1,10)

#Yearly Output
OutputRes <- "yearly"
out_yearly <-  run_3PGhydro(climate,p,lat,StartDate,StandAgei,EndAge,WFi,WRi,WSi,StemNoi,CO2Concentration,FR,HeightEquation,SVEquation,SoilClass,EffectiveRootZoneDepth,DeepRootZoneDepth,RocksER,RocksDR,thinAges,thinVals,thinWF,thinWR,thinWS,OutputRes)
#Some Plots:
par(mfrow=c(2,2))
plot(y=out_yearly$Height,x=out_yearly$StandAge,main="Height",type="l",lwd=2)
points(y=c(17,24,28,29),x=c(31,50,70,80),col="blue",cex=2,pch=20)
plot(y=out_yearly$avDBH,x=out_yearly$StandAge,main="DBH",type="l",lwd=2)
points(y=c(17,26,33,37),x=c(31,50,70,80),col="blue",cex=2,pch=20)
plot(y=out_yearly$StandVol,x=out_yearly$StandAge,main="Standing Volume",type="l",lwd=2)
points(y=c(274,454,530,540),x=c(31,50,70,80),col="blue",cex=2,pch=20)
plot(y=out_yearly$StemNo,x=out_yearly$StandAge,main="Stems",type="l",lwd=2)








#Yearly Output
OutputRes <- "yearly"
gridE <- seq(0.01,0.61, by=0.01)
gridE
years <- c(30,40,50,60)


deepP_E <- data.frame(matrix(ncol = length(1), nrow = length(gridE)))

colnames(deepP_E) <- c("return")

watery_E <- deepP_E
harvestVol_E <- deepP_E


index = 0
round(c(StemNoi*(1-gridE),StemNoi*(1-gridE)^2,StemNoi*(1-gridE)^3,StemNoi*(1-gridE)^4,StemNoi*(1-gridE)^5,StemNoi*(1-gridE)^6,StemNoi*(1-gridE)^7,StemNoi*(1-gridE)^8,StemNoi*(1-gridE)^9,StemNoi*(1-gridE)^10))



index = index+1
d <- numeric()
w <- numeric()
h <- numeric()
for(fall in gridE){
  stand = StemNoi-fall
  
  
  #Management
  thinAges <- c(35,40,45,50,55,60,65,70,75,80)
  thinVals <- ceiling(c(StemNoi*(1-fall),StemNoi*(1-fall)^2,StemNoi*(1-fall)^3,StemNoi*(1-fall)^4,StemNoi*(1-fall)^5,StemNoi*(1-fall)^6,StemNoi*(1-fall)^7,StemNoi*(1-fall)^8,StemNoi*(1-fall)^9,StemNoi*(1-fall)^10))
  
  thinWF <- rep(1,10)
  thinWR <- rep(1,10)
  thinWS <- rep(1,10)
  print(stand)
  
  out_yearly <-  run_3PGhydro(climate,p,lat,StartDate,StandAgei,EndAge,WFi,WRi,WSi,StemNoi,CO2Concentration,FR,HeightEquation,SVEquation,SoilClass,EffectiveRootZoneDepth,DeepRootZoneDepth,RocksER,RocksDR,thinAges,thinVals,thinWF,thinWR,thinWS,OutputRes)
  d<- c(d,sum(out_yearly$DeepPercolation,na.rm=TRUE))
  w<- c(w,sum(out_yearly$DeepPercolation,na.rm=TRUE) + sum(out_yearly$RunOff,na.rm=TRUE) )
  
  h <- c(h, sum(out_yearly$Harvest_Vol, na.rm = TRUE) + out_yearly$StandVol[[EndAge - StandAgei ]] )
  print(sum(out_yearly$DeepPercolation,na.rm=TRUE))
}
thinVals

deepP_E$return <- d
watery_E$return <- w
harvestVol_E$return <-h





####plotshttp://127.0.0.1:34067/graphics/plot_zoom_png?width=1920&height=1017
par(mfrow=c(2,3))

plot(gridE, deepP_E$return , type = "o", col = 1,main="deep percolation")


plot(gridE, watery_E$return , type = "o", col = 1, main="Water yield")


plot(gridE, harvestVol_E$return , type = "o", col = 1, main="Harvest Vol.")

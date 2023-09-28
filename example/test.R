#library(devtools)
#install_github("mdjahan/3PGHydro/rpackage.3PGHydro/")
install.packages("C:/Users/jfelb/Documents/GitHub/3PGHydro/rpackage.3PGHydro", 
                 repos = NULL, 
                 type = "source")
library(rpackage.3PGHydro)
?run_3PGhydro
#
getwd()


setwd("C:/Users/jfelb/Documents/GitHub/3PGHydro/example/")

#climate data
climate <- read.csv("climate.csv")
climate$date <- as.Date(climate$date,format="%d/%m/%Y")

#load 3PG parameters
Parameter <- read.csv("parameter.csv") #P.abies from Forrester et al. 2021; F. sylvatica from Augustynczik et al 2017
#select species
p <- Parameter$Picea.abies

#Site & Stand characteristics
lat <- 47.5
StartDate <- "01/01/1960"
StandAgei <- 30
EndAge <- 80
StemNoi <- 1300
WSi <- 135
WFi <- 8
WRi <- 15
CO2Concentration <- "Historical"
FR <- 0.5
HeightEquation <- 1 
SVEquation <- 1
#Soil
SoilClass <- 2
EffectiveRootZoneDepth <- 1
DeepRootZoneDepth <- 3
RocksER <- 0.2 
RocksDR <- 0.4 



##Output Resolution
#OutputRes <- "daily"
#
##RUN: 3PG-Hydro
#out <- run_3PGhydro(climate,p,lat,StartDate,StandAgei,EndAge,WFi,WRi,WSi,StemNoi,CO2Concentration,FR,HeightEquation,SVEquation,SoilClass,EffectiveRootZoneDepth,DeepRootZoneDepth,RocksER,RocksDR,thinAges,thinVals,thinWF,thinWR,thinWS,OutputRes)
                 
#Some Plots:
# par(mfrow=c(2,2))
# plot(y=out$Height,x=out$Date,main="Height")
# plot(y=out$avDBH,x=out$Date,main="DBH")
# plot(y=out$StandVol,x=out$Date,main="Standing Volume")
# plot(y=out$StemNo,x=out$Date,main="Stems")
# 
# par(mfrow=c(2,2))
# plot(out$GPP[730:1095],x=out$Date[730:1095],type="l",main="GPP")
# plot(out$volWCer[730:1095],x=out$Date[730:1095],type="l",main="vol WC ER")
# plot(out$DeepPercolation[730:1095],x=out$Date[730:1095],type="l",main="Deep xPercolation")
# plot(out$LAI[730:1095],x=out$Date[730:1095],type="l",main="LAI")
# 


#Yearly Output
OutputRes <- "yearly"
gridE <- seq(99,StemNoi, by=25)
gridE
years <- c(30,40,50,60)


deepP_E <- data.frame(matrix(ncol = length(years), nrow = length(gridE)))

colnames(deepP_E) <- years
watery_E <- deepP_E
harvestVol_E <- deepP_E


index = 0

for(y in years){
  print(paste("Thinning year",y))
  index = index+1
  d <- numeric()
  w <- numeric()
  h <- numeric()
for(fall in gridE){
  stand = StemNoi-fall
  
  
  #Management
  thinAges <- c(y)
  if (stand == 0){
    thinAges <- NULL #c(30,40,50,60)
    thinVals <- NULL #c(450,400,350,300)
    thinWF <- NULL #c(1,1,1,1)
    thinWR <- NULL #c(1,1,1,1)
    thinWS <- NULL #c(1,1,1,1)
  } else {
    stand = c(stand)
    thinVals <- stand
    thinWF <- c(1)
    thinWR <- c(1)
    thinWS <- c(1)
    
  }
  print(stand)
  
  out_yearly <-  run_3PGhydro(climate,p,lat,StartDate,StandAgei,EndAge,WFi,WRi,WSi,StemNoi,CO2Concentration,FR,HeightEquation,SVEquation,SoilClass,EffectiveRootZoneDepth,DeepRootZoneDepth,RocksER,RocksDR,thinAges,thinVals,thinWF,thinWR,thinWS,OutputRes)
  d<- c(d,sum(out_yearly$DeepPercolation,na.rm=TRUE))
  w<- c(w,sum(out_yearly$DeepPercolation,na.rm=TRUE) + sum(out_yearly$RunOff,na.rm=TRUE) )
  
  h <- c(h, sum(out_yearly$Harvest_Vol, na.rm = TRUE) + out_yearly$StandVol[[EndAge - StandAgei ]] )
  print(sum(out_yearly$DeepPercolation,na.rm=TRUE))
}

  
  deepP_E[as.character(y)] <- d
  watery_E[as.character(y)] <- w
  harvestVol_E[as.character(y)] <-h
  


}





for(ye in 1:length(years)){
plot(y=deepP_E[[ye]],x=gridE,main="deep percolation")
plot(y=watery_E[[ye]],x=gridE,main="Water yield")

plot(y=harvestVol_E[[ye]],x=gridE,main="Harvest Vol.")
}

matplot(gridE, cbind(harvestVol_E))


# test = data.frame(watery_E)
# harvestVol_E
# test
harvestVol_E


####plotshttp://127.0.0.1:34067/graphics/plot_zoom_png?width=1920&height=1017
par(mfrow=c(2,3))

plot(gridE, deepP_E$"30" , type = "o", col = 1, ylim = c(0, 15000),main="deep percolation")
lines(gridE, deepP_E$"40", type = "o", col = 2)
lines(gridE, deepP_E$"50", type = "o", col = 3)
lines(gridE,deepP_E$"60", type = "o", col = 4)

plot(gridE, watery_E$"30" , type = "o", col = 1, ylim = c(0, 15000), main="Water yield")
lines(gridE, watery_E$"40", type = "o", col = 2)
lines(gridE, watery_E$"50", type = "o", col = 3)
lines(gridE, watery_E$"60", type = "o", col = 4)

plot(gridE, harvestVol_E$"30" , type = "o", col = 1, ylim = c(0, 1000), main="Harvest Vol.")
lines(gridE, harvestVol_E$"40", type = "o", col = 2)
lines(gridE, harvestVol_E$"50", type = "o", col = 3)
lines(gridE, harvestVol_E$"60", type = "o", col = 4)

harvestVol_E$"30"

gridE
length(harvestVol_E["30"])
length(deepP_E)
deepP_E[1]
a<- list()
a[[1]] = 123
a[[1]]
a[1]

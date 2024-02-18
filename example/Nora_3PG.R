library(devtools)
library(rBDAT)
install_github("mdjahan/3PGHydro/rpackage.3PGHydro/")
library(rpackage.3PGHydro)
?run_3PGhydro
#
setwd("C:/Users/jfelb/Documents/GitHub/3PGHydro/example/")
getwd()
#path home
#setwd("C:/Users/jfelb/Documents/GitHub/3PGHydro/example/")
#path office
#getwd("C:/Users/Joaquin Felber/Documents/GitHub/3PGHydro_new/example/")



####Valuation functions
getDiameterClass <- function(x, output) {
  if (x < 7) {
    output = -21
  } else if (x < 10) {
    output = -14
  } else if (x < 15) {
    output = 23
  } else if (x < 20) {
    output = 43
  } else if (x < 25) {
    output = 52
  } else if (x < 30) {
    output = 59
  } else if (x < 35) {
    output = 60
  } else if (x < 40) {
    output = 61
  } else if (x < 50) {
    output = 59
  } else if (x < 60) {
    output = 51
  } else if (x >= 60) {
    output = 43
  }
}



moneyMaker <- function(x, output) {
  
  s <- x[3]
  h <- x[4]
  d <- as.numeric(x[6])
  vol <- as.numeric(x[8])
  print(d)
  print(vol)
  
  price <- switch(s, "X"=0, "Sth"=getDiameterClass(d), "Ind"=14, "nvDh"= 14, 0)
  print(price)
  output = vol*price
  
  
}


###


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

#valuation params
endHarvest = TRUE

# #Yearly Output
# OutputRes <- "yearly"
# out_yearly <-  run_3PGhydro(climate,p,lat,StartDate,StandAgei,EndAge,WFi,WRi,WSi,StemNoi,CO2Concentration,FR,HeightEquation,SVEquation,SoilClass,EffectiveRootZoneDepth,DeepRootZoneDepth,RocksER,RocksDR,thinAges,thinVals,thinWF,thinWR,thinWS,OutputRes)
# #Some Plots:
# par(mfrow=c(2,2))
# plot(y=out_yearly$Height,x=out_yearly$StandAge,main="Height",type="l",lwd=2)
# points(y=c(17,24,28,29),x=c(31,50,70,80),col="blue",cex=2,pch=20)
# plot(y=out_yearly$avDBH,x=out_yearly$StandAge,main="DBH",type="l",lwd=2)
# points(y=c(17,26,33,37),x=c(31,50,70,80),col="blue",cex=2,pch=20)
# plot(y=out_yearly$StandVol,x=out_yearly$StandAge,main="Standing Volume",type="l",lwd=2)
# points(y=c(274,454,530,540),x=c(31,50,70,80),col="blue",cex=2,pch=20)
# plot(y=out_yearly$StemNo,x=out_yearly$StandAge,main="Stems",type="l",lwd=2)








#Yearly Output
OutputRes <- "yearly"
gridE <- seq(0.01,0.61, by=0.01)
gridE
years <- c(30,40,50,60)
 

deepP_E <- data.frame(matrix(ncol = length(1), nrow = length(gridE)))

colnames(deepP_E) <- c("return")

watery_E <- deepP_E
harvestVol_E <- deepP_E
profits_E <- deepP_E
standVol_E <- deepP_E
harvestStems <- deepP_E
index = 0
asdef = floor(c(StemNoi*(1-gridE),StemNoi*(1-gridE)^2,StemNoi*(1-gridE)^3,StemNoi*(1-gridE)^4,StemNoi*(1-gridE)^5,StemNoi*(1-gridE)^6,StemNoi*(1-gridE)^7,StemNoi*(1-gridE)^8,StemNoi*(1-gridE)^9,StemNoi*(1-gridE)^10))


index = index+1
d <- numeric()
w <- numeric()
h <- numeric()
h_ <- numeric()
stV <- numeric()
p_e <- numeric()

for(fall in gridE){
  

  
  
  #Management
  thinAges <- c(35,40,45,50,55,60,65,70,75,80)
  thinVals <- rep(fall,   length(thinAges))#ceiling(c(StemNoi*(1-fall),StemNoi*(1-fall)^2,StemNoi*(1-fall)^3,StemNoi*(1-fall)^4,StemNoi*(1-fall)^5,StemNoi*(1-fall)^6,StemNoi*(1-fall)^7,StemNoi*(1-fall)^8,StemNoi*(1-fall)^9,StemNoi*(1-fall)^10))
  
  thinWF <- rep(1,  length(thinAges))
  thinWR <- rep(1,  length(thinAges))
  thinWS <- rep(1,  length(thinAges))

  
  out_yearly <-  run_3PGhydro(climate,p,lat,StartDate,StandAgei,EndAge,WFi,WRi,WSi,StemNoi,CO2Concentration,FR,HeightEquation,SVEquation,SoilClass,EffectiveRootZoneDepth,DeepRootZoneDepth,RocksER,RocksDR,thinAges,thinVals,thinWF,thinWR,thinWS,OutputRes)
  print("flag 1")
  d<- c(d,sum(out_yearly$DeepPercolation,na.rm=TRUE))
  w<- c(w,sum(out_yearly$DeepPercolation,na.rm=TRUE) + sum(out_yearly$RunOff,na.rm=TRUE) )
  h_ <-c(h_, sum(out_yearly$Harvest_Stems, na.rm = TRUE) )
  h <- c(h, sum(out_yearly$Harvest_Vol, na.rm = TRUE) )
  stV <- c(stV,out_yearly$StandVol[[EndAge - StandAgei ]])
  

  #print(sum(out_yearly$DeepPercolation,na.rm=TRUE))
  
  #Estimate harvest value:
  HH =out_yearly$Harvest_Height
  HD = out_yearly$Harvest_DBH
  #extract harvest years
  HH = HH[thinAges-29]
  HD = HD[thinAges-29]
  #ignore years with 0 harvest
  HH = HH[!HH == 0][ !is.na(HH)]
  HD = HD[!HD == 0][ !is.na(HD)]
  HD
  HD[HD > 100] = 100 #max 100 dbh, otherwise no rbdat values
  
  if (length(HD) != 0){
    print("flag 2")

    tree <- list(spp = rep(1,length(HD)), D1 = HD, H = HH)

    res <- buildTree(tree = tree)
    #getSpeciesCode(inSp = c("Bu", "Fi"))
    
    
    
    
    
    
    assortments = getAssortment(res)
    assortments[is.na(assortments)] <- 0
    assortments["p"] =apply(assortments, 1, moneyMaker, output = "profitpT")
    
    
    
    h_interval= 5
    start = 0
    profits <- numeric()
    assortments
    profits
    assortments$p[(6):(10)]
    sum(assortments$p, na.rm = TRUE)
    for (hyear in seq_along(thinAges)) {
      harvestNo = out_yearly$Harvest_Stems[thinAges[hyear]-29] #out_yearly$StemNo[thinAges[hyear]-30] - thinVals[hyear]
      print(harvestNo)
      profits <- c(profits, harvestNo*sum(assortments$p[(start+1):(start + h_interval)], na.rm = TRUE))
      start = start + h_interval
 

    }
    
    if (endHarvest){
      print("flag 3")
      lastHH =tail(out_yearly$Height, n=1)
      lastHD = min(100, tail(out_yearly$avDBH, n=1))  #max 100 dbh, otherwise no rbdat values
      lastTree <- list(spp = rep(1,length(lastHD)), D1 = lastHD, H = lastHH)
      
      lastRes <- buildTree(tree = lastTree)
      #getSpeciesCode(inSp = c("Bu", "Fi"))
      
      
      
      lastAssortments = getAssortment(lastRes)
      lastAssortments[is.na(lastAssortments)] <- 0
      lastAssortments["p"] =apply(lastAssortments, 1, moneyMaker, output = "profitpT")
      
      profits <- c(profits, tail(out_yearly$StemNo, n=1)*sum(lastAssortments$p, na.rm = TRUE))
    }
    
    p_e <- c(p_e, sum(profits, na.rm = TRUE))

    
  } else {
    p_e <- c(p_e, 0)
  }
  

}


standVol_E$return <- stV
deepP_E$return <- d
watery_E$return <- w
harvestVol_E$return <- h
profits_E$return <- p_e
harvestStems$return <- h_



####plotshttp://127.0.0.1:34067/graphics/plot_zoom_png?width=1920&height=1017
par(mfrow=c(2,3))

plot(gridE, deepP_E$return , type = "o", col = 1,main="deep percolation")


plot(gridE, watery_E$return , type = "o", col = 1, main="Water yield")


plot(gridE, harvestVol_E$return , type = "o", col = 1, main="Harvest Vol.")

plot(gridE, profits_E$return/1000, type = "o", col = 1, main="Harvest profit Euro")

plot(gridE, deepP_E$return*3.05*10/1000 ,  col = 2, main="Water profit Euro")



profits_E


lastHH = tail(out_yearly$Height, n=1)
lastHD = min(100, tail(out_yearly$avDBH, n=1))
lastTree <- list(spp = rep(1,length(lastHD)), D1 = lastHD, H = lastHH)

lastRes <- buildTree(tree = lastTree)
#getSpeciesCode(inSp = c("Bu", "Fi"))

lastHD

lastAssortments = getAssortment(lastRes)
lastAssortments[is.na(lastAssortments)] <- 0
lastAssortments["p"] =apply(lastAssortments, 1, moneyMaker, output = "profitpT")

 tail(out_yearly$StemNo, n=1)*sum(lastAssortments$p, na.rm = TRUE)

 lastAssortments
 
 lastHH
 lastHD

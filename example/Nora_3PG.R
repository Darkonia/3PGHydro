library(devtools)
library(rBDAT)
install_github("mdjahan/3PGHydro/rpackage.3PGHydro/")
library(rpackage.3PGHydro)


#home
#load_all("C:/Users/jfelb/Documents/GitHub/3PGHydro/rpackage.3PGHydro/")
#office
#load_all("C:/Users/Joaquin Felber/Documents/GitHub/3PGHydro/rpackage.3PGHydro/")
#
#?run_3PGhydro
#
setwd("C:/Users/jfelb/Documents/GitHub/3PGHydro/example/")
getwd()
#path home
setwd("C:/Users/jfelb/Documents/GitHub/3PGHydro/example/")
#path office
#setwd("C:/Users/Joaquin Felber/Documents/GitHub/3PGHydro_new/example/")


if(!require('decisionSupport')) {
  install.packages('decisionSupport')
  library('decisionSupport')
}



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


#estimate economic profit/value of harvest based on the average prices and costs between 2000 and 2015 from the FVA (forest research institute Baden-W?rttemberg).
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

# #Management
thinAges <- c(35,40,45,50,55,60,65,70,75,80)
# thinVals <- c(1271,1106,941,776,710,644,578,513,481,449)
# thinWF <- rep(1,10)
# thinWR <- rep(1,10)
# thinWS <- rep(1,10)

#valuation params
endHarvest = TRUE
discount_rates <- c(0,5,10)



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
gridE <- seq(0.01,0.61, by=0.1)
gridE
years <- c(30,40,50,60)
 

deepP_E <- data.frame(matrix(ncol = length(gridE), nrow = -StandAgei + EndAge ))


watery_E <- deepP_E
harvestVol_E <- data.frame(matrix(ncol = length(gridE), nrow = length(gridE)))
colnames(harvestVol_E) <- c("return")
harvestVol_E
harvestVol_E_ <- harvestVol_E
profits_E <- data.frame(matrix(ncol = length(gridE), nrow = length(thinAges)))
profits_E_ <- data.frame(matrix(ncol = length(gridE), nrow = length(thinAges)))
#standVol_E <- deepP_E
#harvestStems <- deepP_E


index = 0

index = index+1
d <- numeric()
w <- numeric()
h <- numeric()
h_ <- numeric()
stV <- numeric()
p_e <- numeric()



idx = 0
for (fall in gridE){
  idx = idx + 1
  
  #Management
  thinAges <- c(35,40,45,50,55,60,65,70,75,80)
  thinVals <- rep(fall,   length(thinAges))#ceiling(c(StemNoi*(1-fall),StemNoi*(1-fall)^2,StemNoi*(1-fall)^3,StemNoi*(1-fall)^4,StemNoi*(1-fall)^5,StemNoi*(1-fall)^6,StemNoi*(1-fall)^7,StemNoi*(1-fall)^8,StemNoi*(1-fall)^9,StemNoi*(1-fall)^10))
  thinVals1 <- NULL #rep(fall,   length(thinAges))
  thinWF <- rep(1,  length(thinAges))
  thinWR <- rep(1,  length(thinAges))
  thinWS <- rep(1,  length(thinAges))
 
  
  out_yearly <-  run_3PGhydro(climate,p,lat,StartDate,StandAgei,EndAge,WFi,WRi,WSi,StemNoi,CO2Concentration,FR,HeightEquation,SVEquation,SoilClass,EffectiveRootZoneDepth,DeepRootZoneDepth,RocksER,RocksDR,thinAges,thinVals,thinWF,thinWR,thinWS,OutputRes)
  print("flag 1")
  deepP_E[idx] = out_yearly$DeepPercolation
  watery_E[idx]<- out_yearly$DeepPercolation + out_yearly$RunOff
  h_ <-c(h, sum(out_yearly$Harvest_Vol + tail(out_yearly$StandVol, n=1), na.rm = TRUE) )
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
    profits_E[idx] <-   profits
  }
    
  ###Harvest end valuation
  print("flag 3")
  lastHH =tail(out_yearly$Height, n=1)
  lastHD = min(100, tail(out_yearly$avDBH, n=1))  #max 100 dbh, otherwise no rbdat values
  lastTree <- list(spp = rep(1,length(lastHD)), D1 = lastHD, H = lastHH)
  
  lastRes <- buildTree(tree = lastTree)
  #getSpeciesCode(inSp = c("Bu", "Fi"))
 
  
  
  lastAssortments = getAssortment(lastRes)
  lastAssortments[is.na(lastAssortments)] <- 0
  lastAssortments["p"] =apply(lastAssortments, 1, moneyMaker, output = "profitpT")
  
  
  profits[length(thinAges)] =  profits[length(thinAges)] + tail(out_yearly$StemNo, n=1)*sum(lastAssortments$p, na.rm = TRUE)
  
  profits_E_[idx] <-  profits



}


harvestVol_E


harvestVol_E$return <- h
harvestVol_E_$return <- h_



watery_E
profits_E
harvestVol_E
harvestVol_E_

####plotshttp://127.0.0.1:34067/graphics/plot_zoom_png?width=1920&height=1017
par(mfrow=c(1,2))
#par(mfrow=c(1,1))

# plot(gridE, deepP_E$return , type = "o", col = 1,main="deep percolation")
# 
# 



plot(gridE, harvestVol_E$return , type = "o", col = 1, main="Harvest Vol.", xlab = "%-per-thinning",
     ylab = "Timber Production [m^3]") 

### water yield


w <- numeric()
for (i in seq_along(gridE)){
  #print(r)
  watery = sum(deepP_E[i], na.rm = TRUE )
  w <- c(w,watery)
}
  
w
plot(gridE, w , type = "o", col = 1, main="Water yield", xlab = "%-per-thinning",
     ylab = "Deep Percolation [m^3]")  

# plot(gridE, profits_E[1]/1000, type = "o", col = 1, main="Harvest profit Euro")
# 
# plot(gridE, (deepP_E$return - deepP_E$return[1])*3.05*10/1000 ,  col = 2, main="Water profit Euro")
# 
# plot(gridE, profits_E$return/1000 + (deepP_E$return - deepP_E$return[1])*3.05*10/1000, type = "o", col = 1, main="total profit Euro")
# 



 
 ###NPV Harvest
npv_harvest_E = data.frame(matrix(ncol = length(discount_rates), nrow = length(gridE)))
idx = 0
for (r in discount_rates){
  r = ((1+(r/100))**5-1)*100
 idx = idx+1
 npv <- numeric()
 for (i in seq_along(gridE)){
   print(r)
   npv <- c(npv,discount(unlist(profits_E[i]),r ,calculate_NPV=TRUE))
 }
 npv_harvest_E[idx] = npv
}
 npv_harvest_E


 
 m <- matrix(c(1,2,3,4,5,5),nrow = 3,ncol = 2,byrow = TRUE)
 
 layout(mat = m,heights = c(0.5,0.5,0.2))
 
 par(mar = c(4,4,2,2))
     
 plot(gridE, npv_harvest_E$X1/1000, main= "Timber Harvest", type = "o", col = 1, ylim=c(0,50), xlab = "%-per-thinning",
      ylab = "NPV [1000€]") 
 lines(gridE,npv_harvest_E$X2/1000, type = "o", col = 2)
 lines(gridE,npv_harvest_E$X3/1000, type = "o", col = 3) 
 npv_harvest_E
 ###NPV Harvest endharvest

 
 
 npv_harvest_E_ = data.frame(matrix(ncol = length(discount_rates), nrow = length(gridE)))
 idx = 0
 for (r in discount_rates){
   r = ((1+(r/100))**5-1)*100
   idx = idx+1
   npv <- numeric()
   for (i in seq_along(gridE)){
     print(r)
     npv <- c(npv,discount(unlist(profits_E_[i]),r ,calculate_NPV=TRUE))
   }
   npv_harvest_E_[idx] = npv
 }
 npv_harvest_E_

 plot(gridE, npv_harvest_E_$X1/1000, main= "Timber Harvest with end harvest", type = "o", col = 1, ylim=c(0,50), xlab = "%-per-thinning",
      ylab = "NPV [1000€]") 
 lines(gridE,npv_harvest_E_$X2/1000, type = "o", col = 2)
 lines(gridE,npv_harvest_E_$X3/1000, type = "o", col = 3) 
 #lines(gridE,npv_harvest_E$X2/1000, type = "o", col = 4)
 #lines(gridE,npv_harvest_E$X3/1000, type = "o", col = 5) 
 
 
 ###NPV water
 npv_water_E = data.frame(matrix(ncol = length(discount_rates), nrow = length(gridE)))
 idx = 0
 for (r in discount_rates){
   idx = idx+1
   npv <- numeric()
   for (i in seq_along(gridE)){
     #print(r)
     profit_water = (deepP_E[i]-unlist(deepP_E[1])[2])*3.05*10/1000
     profit_water[is.na(profit_water)]= 0
     npv <- c(npv,discount(unlist(profit_water),r ,calculate_NPV=TRUE))
   }
   npv_water_E[idx] = npv
 }

 plot(gridE, npv_water_E$X1, type = "o", main="Water Yield", col = 1, ylim=c(0,500), xlab = "%-per-thinning",
      ylab = "NPV [1000€]")  
 lines(gridE,npv_water_E$X2, type = "o", col = 2)
 lines(gridE,npv_water_E$X3, type = "o", col = 3) 
 
 
 plot(gridE, npv_harvest_E$X1/1000+ npv_water_E$X1, main="Total Profits", type = "o", col = 1, ylim=c(0,500), xlab = "%-per-thinning",
      ylab = "NPV [1000€]")  
 lines(gridE,npv_harvest_E$X2/1000+ npv_water_E$X2, type = "o", col = 2)
 lines(gridE,npv_harvest_E$X3/1000+ npv_water_E$X3, type = "o", col = 3) 

 plot(gridE, npv_harvest_E_$X1/1000+ npv_water_E$X1, main="Total Profits with end harvest", type = "o", col = 1, ylim=c(0,500), xlab = "%-per-thinning",
      ylab = "NPV [1000€]")  
 lines(gridE,npv_harvest_E_$X2/1000+ npv_water_E$X2, type = "o", col = 2)
 lines(gridE,npv_harvest_E_$X3/1000+ npv_water_E$X3, type = "o", col = 3) 
 plot(1, type = "n", axes=FALSE, xlab="", ylab="")
 
 legend(x = "top",
        inset = 0, # You will need to fine-tune the
        # first value depending on the windows size
        legend = c("0% discount rate", "5%", "10%"), 
        lty = c(1, 1,1),
        col = c(1, 2, 3),
        lwd=5, 
        cex=1.7,
        xpd = TRUE,   # You need to specify this to add
        # the legend to put the legend outside the plot
        horiz = TRUE)

 
 plot(gridE, npv_harvest_E_$X1/1000+ npv_water_E$X1, main="Total Profits with end harvest", type = "o", col = 1, ylim=c(0,500), xlab = "%-per-thinning",
      ylab = "NPV [1000€]")  
 lines(gridE,npv_harvest_E_$X2/1000+ npv_water_E$X2, type = "o", col = 2)
 lines(gridE,npv_harvest_E_$X3/1000+ npv_water_E$X3, type = "o", col = 3) 
 
 profit_water
 
 library(ggplot2)
 
 ggplot(df, aes(x=gridE, y=npv_water_E$X1)) +
   geom_point()
 

 #export results to public good game analysis
 library(RJSONIO)
 
 typeof(npv_water_E$X1)
 exportJson <- toJSON(npv_water_E)
 write(exportJson, "npv_water.json")
 
 exportJson <- toJSON(npv_harvest_E)
 write(exportJson, "npv_harvest.json")
 exportJson
 
 
 # fit2degree <- function(x, fit, output) {
 #   output = fit$coefficients[1] + fit$coefficients[2]*x + fit$coefficients[3]*x*x#+ fit$coefficients[4]*x*x*x
 # }
 # 
 # 
 # fit = lm(unlist(npv_water_E$X1) ~ poly(gridE, 2, raw=TRUE) ) 
 # 
 # 
 # plot(gridE, fit2degree(gridE,fit), type = "o", col = 1, ylim=c(0,1000)) 
 # 
 # fit = lm(unlist(npv_harvest_E$X2) ~ poly(gridE, 2, raw=TRUE) ) 
 # fit
 # plot(gridE, fit2degree(gridE,fit), type = "o", col = 1,)# ylim=c(0,1000)) 
 
print("Success")

### Projekt ###

yearBorough <- read.table("year_borough_grocery.csv", header = TRUE, sep = ",")
yearBorough
boroughArea <- yearBorough$area_sq_km
boroughPopulation <- yearBorough$population

yearLsoa <- read.table("year_lsoa_grocery.csv", header = TRUE, sep =",")
yearLsoa
lsoaArea <- yearLsoa$area_sq_km
lsoaPopulation <- yearLsoa$population

yearMsoa <- read.table("year_msoa_grocery.csv", header = TRUE, sep =",")
yearMsoa
msoaArea <- yearMsoa$area_sq_km
msoaPopulation <- yearMsoa$population

yearOsward <- read.table("year_osward_grocery.csv", header = TRUE, sep =",")
yearOsward
oswardArea <- yearOsward$area_sq_km
oswardPopulation <- yearOsward$population


sum(boroughArea)
sum(lsoaArea)     ## alle gleich bis auf lsoaArea
sum(msoaArea)
sum(oswardArea)

sum(boroughPopulation)
sum(lsoaPopulation)     ##wieder alle gleich bis auf lsoaPopulation
sum(msoaPopulation)
sum(oswardPopulation)


for (i in seq_len(length(boroughArea))) {
  print((max(boroughArea)-boroughArea[i])/max(boroughArea)*100)
}

for (i in seq_len(length(lsoaArea))) {
  print((max(lsoaArea)-lsoaArea[i])/max(lsoaArea)*100)
}

for (i in seq_len(length(msoaArea))) {
  print((max(msoaArea)-msoaArea[i])/max(msoaArea)*100)
}

for (i in seq_len(length(oswardArea))) {
  print((max(oswardArea)-oswardArea[i])/max(oswardArea)*100)
}


for (i in seq_len(length(boroughPopulation))) {
  print((max(boroughPopulation)-boroughPopulation[i])/max(boroughPopulation)*100)
}

for (i in seq_len(length(lsoaPopulation))) {
  print((max(lsoaPopulation)-lsoaPopulation[i])/max(lsoaPopulation)*100)
}

for (i in seq_len(length(msoaPopulation))) {
  print((max(msoaPopulation)-msoaPopulation[i])/max(msoaPopulation)*100)
}

for (i in seq_len(length(oswardPopulation))) {
  print((max(oswardPopulation)-oswardPopulation[i])/max(oswardPopulation)*100)
}

### Dataframe ###

numberOfAreasBorough <- length(boroughArea)
numberOfAreasLsoa <- length(lsoaArea)
numberOfAreasMsoa <- length(msoaArea)
numberOfAreasOsward <- length(oswardArea)

avgSurvaceBorough <- sum(yearBorough$area_sq_km) / numberOfAreasBorough
avgSurvaceLsoa <- sum(yearLsoa$area_sq_km) / numberOfAreasLsoa
avgSurvaceMsoa <- sum(yearMsoa$area_sq_km) / numberOfAreasMsoa
avgSurvaceOsward <- sum(yearOsward$area_sq_km) / numberOfAreasOsward

avgPopulationBorough <- sum(boroughPopulation) / numberOfAreasBorough
avgPopulationLsoa <- sum(lsoaPopulation) / numberOfAreasLsoa
avgPopulationMsoa <- sum(msoaPopulation) / numberOfAreasMsoa
avgPopulationOsward <- sum(oswardPopulation) / numberOfAreasOsward

medianPopLsoa <- median(yearLsoa$population)
medianPopMsoa <- median(yearMsoa$population)
medianPopOsward <- median(yearOsward$population)
medianPopBorough <- median(yearBorough$population)

medianAreaLsoa <- median(yearLsoa$area_sq_km)
medianAreaMsoa <- median(yearMsoa$area_sq_km)
medianAreaOsward <- median(yearOsward$area_sq_km)
medianAreaBorough <- median(yearBorough$area_sq_km)


tab1Dataframe <- data.frame(
  Area = c("LSOA","MSOA","Ward","Borough"),
  NumberOfAreas = c(numberOfAreasLsoa,numberOfAreasMsoa,numberOfAreasOsward,numberOfAreasBorough),
  AvgSurfaces = c(avgSurvaceLsoa,avgSurvaceMsoa,avgSurvaceOsward,avgSurvaceBorough),
  AvgPopulation = c(avgPopulationLsoa,avgPopulationMsoa,avgPopulationOsward,avgPopulationOsward),
  MedianPopulation = c(medianPopLsoa,medianPopMsoa,medianPopOsward,medianPopBorough),
  MedianArea = c(medianAreaLsoa,medianAreaMsoa,medianAreaOsward,medianAreaBorough),
  stringsAsFactors = FALSE
)
tab1Dataframe

## Visualisieren
par(mfrow=c(1,2))
hist(yearLsoa$population, main = "Lsoa Bevoelkerung",xlab = "Bevoelkerung", ylab = "Haeufigkeit", col =3)
hist(yearLsoa$area_sq_km, main = "Lsoa Flaeche",xlab = "Flaeche", ylab = "Haeufigkeit", col =4)


#2
install.packages("sf")
library(sf)
boundariesLondon <- st_read("LSOA_2011_London_gen_MHW.shp")

class(boundariesLondon)
dim(boundariesLondon)
colnames(boundariesLondon)

mergeboundaries <- merge(boundariesLondon, yearLsoa, by.x = "LSOA11CD", by.y = "area_id")
class(mergeboundaries)
dim(mergeboundaries)    # zwei Reihen weniger, da diese in "yearLsoa" nicht vorhanden sind (weiß nicht, wie es gehört)
colnames(mergeboundaries)

install.packages("colorspace")
library("colorspace")


par(mfrow=c(1,2))
# des mit log geht vllt anders a nu
# mergeboundaries$logvals <- log(mergeboundaries$num_transactions)
plot(mergeboundaries["num_transactions"], logz = TRUE, main = "Number of Transactions", pal = terrain_hcl) #palette muss ma si nu anschauen

mergeboundaries$perresident <- mergeboundaries$num_transactions/mergeboundaries$population
plot(mergeboundaries["perresident"], logz = TRUE, main = "Transactions per resident", pal = terrain_hcl) #palette muss ma si nu anschauen

# fast kein Unterscheid zwischen den beiden


#3
sequ <- seq(0, 1, by = 0.01)
x <- numeric()
for(i in sequ){
  x <- append(x, nrow(yearLsoa[yearLsoa$representativeness_norm > i,])/nrow(yearLsoa))
}
lsoathreshold <- data.frame(seq = sequ, perc = x)

x <- numeric()
for(i in sequ){
  x <- append(x, nrow(yearMsoa[yearMsoa$representativeness_norm > i,])/nrow(yearMsoa))
}
msoathreshold <- data.frame(seq = sequ, perc = x)

x <- numeric()
for(i in sequ){
  x <- append(x, nrow(yearOsward[yearOsward$representativeness_norm > i,])/nrow(yearOsward))
}
wardthreshold <- data.frame(seq = sequ, perc = x)



plot(lsoathreshold, main = "Representativitaet", xlab = "Threshold")
lines(msoathreshold, lty = 1)
lines(wardthreshold, lty = 2)
legend("topright", c("lsoa", "msoa", "ward"), lty = c(NA,1,2), pch = c(1,NA,NA))



#4

energyFat <- yearMsoa$energy_fat
energyFibre <- yearMsoa$energy_fibre
energySat <- yearMsoa$energy_saturate
energyCarb <- yearMsoa$energy_carb
energySugar <- yearMsoa$energy_sugar
energyProtein <- yearMsoa$energy_protein

energyTot <- yearMsoa$energy_tot

energyProteinTot <- energySugarTot <- energyCarbTot <- energySatTot <- energyFibreTot <-  energyFatTot <- numeric(length(nrow(yearMsoa)))


  #energyFatTot <- (energyTot - energyFat) / energyTot
energyFatTot <- energyFat / energyTot

  #energyFibreTot <- (energyTot - energyFibre) / energyTot
energyFibreTot <- energyFibre / energyTot

  #energySatTot <- (energyTot - energySat) / energyTot
energySatTot <- energySat / energyTot


  #energyCarbTot <- (energyTot - energyCarb) / energyTot
energyCarbTot <- energyCarb / energyTot

  #energySugarTot <- (energyTot - energySugar) / energyTot
energySugarTot <- energySugar / energyTot

#energyProteinTot <- (energyTot - energyProtein) / energyTot
energyProteinTot <- energyProtein / energyTot


par(mfrow=c(2,3))
hist(energyCarbTot, main = NULL, xlab = "Energie von Kohlenhydraten", ylab = "Haeufigkeit",breaks=seq(0,0.6,length=100), col = "red", ylim = c(0,300))
hist(energyFatTot,main = NULL, xlab = "Energie von Fetten", ylab = "Haeufigkeit",breaks=seq(0,0.6,length=100), col = "purple", ylim = c(0,300))
hist(energySatTot, main = NULL, xlab = "Energie von Saturated", ylab = "Haeufigkeit",breaks=seq(0,0.6,length=100), col = "pink", ylim = c(0,300))
hist(energySugarTot, main = NULL, xlab = "Energie von Zucker", ylab = "Haeufigkeit",breaks=seq(0,0.6,length=100), col = "orange", ylim = c(0,300))
hist(energyProteinTot, main = NULL, xlab = "Energie von Eiweiß", ylab = "Haeufigkeit",breaks=seq(0,0.6,length=100), col = "green", ylim = c(0,300))
hist(energyFibreTot,main = NULL, xlab = "Energie von Ballaststoffe", ylab = "Haeufigkeit",breaks=seq(0,0.6,length=100), col = "lightgreen", ylim = c(0,300))



#5

diabetes <- read.table("diabetes_estimates_osward_2016.csv", header = TRUE, sep = ",")
head(diabetes)


londonWardX <- st_read("./statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Ward_CityMerged.shp")
londonWardY <- st_read("./London-wards-2014/London-wards-2014 (1)/London-wards-2014_ESRI/London_Ward_CityMerged.shp")

dim(londonWardX)
dim(diabetes)
dim(londonWardY)
merged <- st_join(diabets, londonWardY)
merged <- merge(diabetes, yearOsward, by.x = "area_id", by.y = "area_id")
dim(merged)
head(merged)

FUN <- function(var.x, var.y, ...){
  
  
}
class(londonWardX)[1] == "sf"

compare_data <- function(x, y, id.x, id.y = id.x, var.x = character(0), var.y = var.x, FUN = sum, ...) {
  if(!is.list(x) | !is.list(y))
    stop("Bitte überprüfen Sie Ihre Eingabe für x und y!")
  if(!is.character(id.x) | (!is.character(id.y) & !is.na(id.y)))
    stop("es muss eine korrekte ID angegeben werden!")
  if(!is.character(var.x) | (!is.character(var.y) & !is.na(var.y)))
    stop("es muss eine korrekte Variable angegeben werden!")
  
  if(class(x)[1] == "sf" & class(y)[1] == "sf"){
    #spezielle Behandlung bei merge von 2 sf Objekten (vllt st_join)
    mergedXandY <- merge(x %>% as.data.frame(), y %>% as.data.frame(), by.x = id.x, by.y = id.y)
    mergedXandY %>% st_sf(sf_column_name = 'geometry.x')
    mergedX <- merge(x %>% as.data.frame(), y %>% as.data.frame(), by.x = id.x, by.y = id.y, all.x = TRUE)
    mergedX %>% st_sf(sf_column_name = 'geometry.x')
    mergedY <- merge(x %>% as.data.frame(), y %>% as.data.frame(), by.x = id.x, by.y = id.y, all.y = TRUE)
    mergedY %>% st_sf(sf_column_name = 'geometry.y')
    
    sumXandY <- (FUN(get(paste(var.x, ".x",sep = ""), mergedXandY), ...))
    sumX <- (FUN(get(paste(var.x,".x",sep = ""), mergedX), ...))
    sumY <- (FUN(get(paste(var.x,".y",sep = ""), mergedY),...))
  }
  else{
    mergedXandY <- merge(x, y, by.x = id.x, by.y = id.y)
    mergedX <- merge(x, y, by.x = id.x, by.y = id.y, all.x = TRUE)
    mergedY <- merge(x, y, by.x = id.x, by.y = id.y, all.y = TRUE)
    
    sumXandY <- (FUN(get(var.x, mergedXandY), ...))
    sumX <- (FUN(get(var.x, mergedX), ...))
    sumY <- (FUN(get(var.x, mergedY),...))
  }
  
  
  df <- data.frame(mergeType = c("X und Y", "nur X", "nur Y"), 
                   anzahl = c(nrow(mergedXandY),nrow(mergedX)-nrow(mergedXandY), nrow(mergedY)-nrow(mergedXandY)),
                   hektar = c(round(sumXandY,0), round(sumX-sumXandY,0), round(sumY-sumXandY,0)))
  
  if(var.x != var.y){
    popXandY <- (FUN(get(var.y, mergedXandY), ...))
    popX <- (FUN(get(var.y, mergedX), ...))
    popY <- (FUN(get(var.y, mergedY), ...))
    df$hektar <- df$hektar*100
    df <- cbind(df, bevölkerung = c(popXandY, popX-popXandY, popY-popXandY))
  }
 
  return(df)
}

compare_data(londonWardX, londonWardY, "GSS_CODE", "GSS_CODE", "HECTARES", "HECTARES", na.rm = TRUE)
compare_data(diabetes, londonWardY, "area_id", "GSS_CODE", "HECTARES", na.rm= TRUE)
compare_data(diabetes, yearOsward, "area_id", "area_id", "area_sq_km", "population", na.rm = TRUE)


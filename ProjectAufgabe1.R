### Projekt ###

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

yearBorough <- read.table("year_borough_grocery.csv", header = TRUE, sep = ",")
yearBorough
boroughArea <- yearBorough$area_sq_km
boroughPopulation <- yearBorough$population

areas <- list(lsoa = lsoaArea, msoa =  msoaArea,  ward = oswardArea, borough = boroughArea)
population <- list(lsoa = lsoaPopulation, msoa = msoaPopulation, ward = oswardPopulation, borough = boroughPopulation)

lapply(areas, sum)     ## alle gleich bis auf lsoaArea
lapply(population, sum)     ## wieder alle gleich bis auf lsoaPopulation

# relative Abstände
relDif <- function(var){
  x <- numeric()
  for (i in seq_along(var)){
    x <- append(x, round((max(var)-var[i])/max(var)*100,2))
  }
  return(x)
}

lapply(areas, relDif)
lapply(population, relDif)


### Dataframe ###

noAreas <- lapply(areas, length) 

tab1Dataframe <- data.frame(
  Area = c("LSOA","MSOA","Ward","Borough"),
  NumberOfAreas = unlist(noAreas),
  AvgSurfaces = round(unlist(Map("/", lapply(areas, sum), noAreas)),2),
  AvgPopulation = round(unlist(Map("/", lapply(population, sum), noAreas)),0),
  MedianPopulation = unlist(lapply(population, median)),
  MedianArea = unlist(lapply(areas, median)),
  stringsAsFactors = FALSE
)
rownames(tab1Dataframe) <- NULL
tab1Dataframe

## Müss ma si nu anschaun, ob ma des so lassen
## Visualisieren
par(mfrow=c(1,2))
hist(lsoaPopulation, main = "Lsoa Bevoelkerung", xlab = "Bevoelkerung", ylab = "Haeufigkeit",breaks= seq(min(lsoaPopulation),max(lsoaPopulation)+100, 200), col =3, freq = FALSE)
hist(lsoaArea, main = "Lsoa Flaeche",xlab = "Flaeche", ylab = "Haeufigkeit", breaks= seq(min(lsoaArea),max(lsoaArea)+0.5, 0.5), col =4, freq = FALSE)



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


#6

merged <- merge(yearOsward, diabetes, by= "area_id")


# zusammenhang spearman
corr <- cor.test(merged$estimated_diabetes_prevalence, merged$energy_tot, method = "spearman")
corr  # 0.58

par(mfrow=c(1,1))
plot(merged$estimated_diabetes_prevalence, merged$energy_tot, main = "Streudiagramm", xlab = "geschätzte Diabetes-Prävalenz", ylab = "Energie der Nährstoffe")

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

relD <- lapply(areas, relDif)
str(relD)
relD <- lapply(population, relDif)
str(relD)



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


## Visualisieren
par(mfrow=c(1,2))
hist(lsoaPopulation, main = "Lsoa Bevoelkerung", xlab = "Bevoelkerung", ylab = "Dichte",breaks= seq(min(lsoaPopulation),max(lsoaPopulation)+100, 100), col =3, freq = FALSE, xlim = c(min(lsoaPopulation),quantile(lsoaPopulation,probs = 0.99)))
hist(lsoaArea, main = "Lsoa Flaeche",xlab = "Flaeche", ylab = "Dichte", breaks= seq(min(lsoaArea),max(lsoaArea)+0.5, 0.1), col =4, freq = FALSE, xlim = c(min(lsoaArea),quantile(lsoaArea,probs = 0.99)))


#2
install.packages("sf")
library(sf)
boundariesLondon <- st_read("./statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/LSOA_2011_London_gen_MHW.shp")

class(boundariesLondon)
dim(boundariesLondon)
colnames(boundariesLondon)

mergeboundaries <- merge(boundariesLondon, yearLsoa, by.x = "LSOA11CD", by.y = "area_id")
class(mergeboundaries)
dim(mergeboundaries)    # zwei Reihen weniger, da diese in "yearLsoa" nicht vorhanden sind (weiß nicht, wie es gehört)
colnames(mergeboundaries)

#install.packages("colorspace")
library("colorspace")

par(mfrow=c(1,2))
# des mit log geht vllt anders a nu
# mergeboundaries$logvals <- log(mergeboundaries$num_transactions)
mergeboundaries$perresident <- mergeboundaries$num_transactions/mergeboundaries$population
par(mfrow=c(1,2))
plot(mergeboundaries["num_transactions"], logz = TRUE, main = "Number of Transactions", pal = terrain_hcl) #palette muss ma si nu anschauen
plot(mergeboundaries["perresident"], logz = TRUE, main = "Transactions per resident", pal = terrain_hcl) #palette muss ma si nu anschauen

# fast kein Unterscheid zwischen den beiden


#3

sequ <- seq(0, 1, by = 0.01)

dat <- list(lsoa = yearLsoa, msoa = yearMsoa, ward = yearOsward)

repr <- function(var){
  x <- numeric()
  for(i in sequ){
    x <- append(x, nrow(var[var$representativeness_norm > i,])/nrow(var))
  }
  return(list(seq = sequ, perc = x))
}

normRepr <- lapply(dat, repr)

par(mfrow=c(1,1))
plot(as.data.frame(normRepr$lsoa), main = "Representativitaet", xlab = "Threshold")
lines(as.data.frame(normRepr$msoa), lty = 1)
lines(as.data.frame(normRepr$ward), lty = 2)
legend("topright", c("lsoa", "msoa", "ward"), lty = c(NA,1,2), pch = c(1,NA,NA))



#4
energyTot <- yearMsoa$energy_tot
energyFatTot <- yearMsoa$energy_fat / energyTot
energyFibreTot <- yearMsoa$energy_fibre / energyTot
energySatTot <- yearMsoa$energy_saturate / energyTot
energyCarbTot <- yearMsoa$energy_carb / energyTot
energySugarTot <- yearMsoa$energy_sugar / energyTot
energyProteinTot <- yearMsoa$energy_protein / energyTot

par(mfrow=c(2,3))

h <- hist(energyCarbTot, breaks=seq(0,0.6,length=75), plot = F)
h$density <- h$counts/sum(h$counts)
plot(h, main = NULL, xlab = "Energie von Kohlenhydraten",xlim = c(0,0.6), ylim = c(0,0.6), col = "red", freq = F)

h <- hist(energyFatTot, breaks=seq(0,0.6,length=75), plot = F)
h$density <- h$counts/sum(h$counts)
plot(h, main = NULL, xlab = "Energie von Fetten",xlim = c(0,0.6), ylim = c(0,0.6), col = "purple", freq = F)

h <- hist(energySatTot, breaks=seq(0,0.6,length=75), plot = F)
h$density <- h$counts/sum(h$counts)
plot(h, main = NULL, xlab = "Energie von Saturated",xlim = c(0,0.6), ylim = c(0,0.6), col = "pink", freq = F)

h <- hist(energySugarTot, breaks=seq(0,0.6,length=75), plot = F)
h$density <- h$counts/sum(h$counts)
plot(h, main = NULL, xlab = "Energie von Zucker",xlim = c(0,0.6), ylim = c(0,0.6), col = "orange", freq = F)

h <- hist(energyProteinTot, breaks=seq(0,0.6,length=75), plot = F)
h$density <- h$counts/sum(h$counts)
plot(h, main = NULL, xlab = "Energie von Eiweiß",xlim = c(0,0.6), ylim = c(0,0.6), col = "green", freq = F)

h <- hist(energyFibreTot, breaks=seq(0,0.6,length=75), plot = F)
h$density <- h$counts/sum(h$counts)
plot(h, main = NULL, xlab = "Energie von Ballaststoffe",xlim = c(0,0.6), ylim = c(0,0.6), col = "lightgreen", freq = F)


#hist(energyFatTot,main = NULL, xlab = "Energie von Fetten", ylab = "Haeufigkeit",breaks=seq(0,0.6,length=100), col = "purple", ylim = c(0,300), plot = F)
#hist(energySatTot, main = NULL, xlab = "Energie von Saturated", ylab = "Haeufigkeit",breaks=seq(0,0.6,length=100), col = "pink", ylim = c(0,300))
#hist(energySugarTot, main = NULL, xlab = "Energie von Zucker", ylab = "Haeufigkeit",breaks=seq(0,0.6,length=100), col = "orange", ylim = c(0,300))
#hist(energyProteinTot, main = NULL, xlab = "Energie von Eiweiß", ylab = "Haeufigkeit",breaks=seq(0,0.6,length=100), col = "green", ylim = c(0,300))
#hist(energyFibreTot,main = NULL, xlab = "Energie von Ballaststoffe", ylab = "Haeufigkeit",breaks=seq(0,0.6,length=100), col = "lightgreen", ylim = c(0,300))



#5

diabetes <- read.table("diabetes_estimates_osward_2016.csv", header = TRUE, sep = ",")
head(diabetes)

# Einlesen der Shapefiles
londonWardX <- st_read("./statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Ward_CityMerged.shp")
londonWardY <- st_read("./London-wards-2014/London-wards-2014 (1)/London-wards-2014_ESRI/London_Ward_CityMerged.shp")

compare_data <- function(x, y, id.x, id.y = id.x, var.x = character(0), var.y = var.x, FUN = sum, ...) {
  # Zwei Listen müssen übergeben werden, ansonsten nicht genügend Daten
  if(!is.list(x) | !is.list(y))
    stop("Bitte überprüfen Sie Ihre Eingabe für x und y!")
  # ID muss als String übergeben werden (zumindest x, y nicht unbedingt. Wenn y übergeben wird, muss es auch ein String sein)
  if(!is.character(id.x) | ((id.x != id.y) & !is.character(id.y)))
    stop("es muss eine korrekte ID angegeben werden!")
  # Variable muss als String übergeben werden (zumindest x, y nicht unbedingt. Wenn y übergeben wird, muss es auch ein String sein)
  if(!is.character(var.x) | ((var.x != var.y) & !is.character(var.y)))
    stop("es muss eine korrekte Variable angegeben werden!")
  
  
  #spezielle Behandlung bei merge von 2 sf Objekten
  if(class(x)[1] == "sf" & class(y)[1] == "sf"){
    # verknüpft werden die Objekte, indem sie zuerst in einen Dataframe umgewandelt werden und mit der jeweiligen ID verbunden werden. Dabei wird
    # sozusagen die Geometry-Variable deaktiviert
    mergedXandY <- merge(x %>% as.data.frame(), y %>% as.data.frame(), by.x = id.x, by.y = id.y)
    # Danach wird die Geometry-Variable wieder "aktiviert" und wieder in ein Objekt der Klasse sf umgewandelt
    mergedXandY %>% st_sf(sf_column_name = 'geometry.x')
    mergedX <- merge(x %>% as.data.frame(), y %>% as.data.frame(), by.x = id.x, by.y = id.y, all.x = TRUE)
    mergedX %>% st_sf(sf_column_name = 'geometry.x')
    mergedY <- merge(x %>% as.data.frame(), y %>% as.data.frame(), by.x = id.x, by.y = id.y, all.y = TRUE)
    mergedY %>% st_sf(sf_column_name = 'geometry.y')
    
    # Funktion wird auf die einzelnen Gruppierungen angewendet
    sumXandY <- sumXandY1 <- (FUN(get(paste(var.x, ".x",sep = ""), mergedXandY), ...))
    sumXandY2 <- (FUN(get(paste(var.y, ".x",sep = ""), mergedXandY), ...))
    sumX <- (FUN(get(paste(var.x,".x",sep = ""), mergedX), ...))
    sumY <- (FUN(get(paste(var.y,".y",sep = ""), mergedY), ...))
  }
  else{
    # Verknüpfung der Objekte nach den jeweiligen IDs
    mergedXandY <- merge(x, y, by.x = id.x, by.y = id.y)
    mergedX <- merge(x, y, by.x = id.x, by.y = id.y, all.x = TRUE)
    mergedY <- merge(x, y, by.x = id.x, by.y = id.y, all.y = TRUE)
    
    sumXandY <- sumXandY1 <- (FUN(get(var.x, mergedXandY), ...))
    sumXandY2 <- (FUN(get(var.y, mergedXandY), ...))
    sumX <- (FUN(get(var.x, mergedX), ...))
    sumY <- (FUN(get(var.y, mergedY),...))
  }
  
  # wenn die Ergebnisse der angewendeten Funktion aus dem ersten für die erste und zweite Variable nicht übereinstimmen, wird für das Ergebnis der gemeinsamen Daten
  # von X und Y NA als Ergebnis verwendet
  if(sumXandY1 != sumXandY2)
    sumXandY <- NA
  
  df <- data.frame(mergeType = c("X und Y", "nur X", "nur Y"), 
                   anzahl = c(nrow(mergedXandY),nrow(mergedX)-nrow(mergedXandY), nrow(mergedY)-nrow(mergedXandY)),
                   ergebnis = c(round(sumXandY,0), if(var.x %in% colnames(x)) round(sumX-sumXandY1,0) else NA,
                                if(var.y %in% colnames(y)) round(sumY-sumXandY2,0) else NA))
 
  return(df)
}

compare_data(londonWardX, londonWardY, "GSS_CODE", "GSS_CODE", "HECTARES", na.rm = TRUE)
compare_data(diabetes, londonWardY, "area_id", "GSS_CODE", "HECTARES", na.rm= TRUE)
compare_data(diabetes, yearOsward, "area_id", "area_id", "area_sq_km", na.rm = TRUE)
compare_data(diabetes, yearOsward, "area_id", "area_id", "gp_patients", "population", na.rm = TRUE)
sum(diabetes$gp_patients)
sum(yearOsward$population)


#6
merged <- merge(yearOsward, diabetes, by= "area_id")

# zusammenhang spearman
cor.test(merged$estimated_diabetes_prevalence, merged$energy_tot, method = "spearman")

par(mfrow=c(1,1))
plot(merged$estimated_diabetes_prevalence, merged$energy_tot, main = "Streudiagramm", xlab = "geschätzte Diabetes-Prävalenz", ylab = "Energie der Nährstoffe")

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
summary(boundariesLondon)
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
plot(mergeboundaries["num_transactions"], logz = TRUE, pal = terrain_hcl) #palette muss ma si nu anschauen

mergeboundaries$percivil <- mergeboundaries$num_transactions/mergeboundaries$population
plot(mergeboundaries["percivil"], logz = TRUE,  pal = terrain_hcl) #palette muss ma si nu anschauen

# fast kein Unterscheid zwischen den beiden




#3





#4
energyFat <- yearMsoa$energy_fat
energyFibre <- yearMsoa$energy_fibre
energyAlc <- yearMsoa$energy_alcohol
energyCarb <- yearMsoa$energy_carb
energySugar <- yearMsoa$energy_sugar
energyProtein <- yearMsoa$energy_protein
energyTot <- yearMsoa$energy_tot
energyFatTot <-c(1:length(energyFat))
energyFibreTot <-c(1:length(energyFat))
energyAlcTot <-c(1:length(energyFat))
energyCarbTot <-c(1:length(energyFat))
energySugarTot <-c(1:length(energyFat))
energyProteinTot <-c(1:length(energyFat))



for(i in seq_along(length(energyTot))) {
  energyFatTot <- (energyTot - energyFat) / energyTot
}

for(i in seq_along(length(energyTot))) {
  energyFibreTot <- (energyTot - energyFibre) / energyTot
}

for(i in seq_along(length(energyTot))) {
  energyAlcTot <- (energyTot - energyAlc) / energyTot
}

for(i in seq_along(length(energyTot))) {
  energyCarbTot <- (energyTot - energyCarb) / energyTot
}

for(i in seq_along(length(energyTot))) {
  energySugarTot <- (energyTot - energySugar) / energyTot
}

for(i in seq_along(length(energyTot))) {
  energyProteinTot <- (energyTot - energyProtein) / energyTot
}

hist(energySugarTot, main = "test",xlab = "relativer Anteil an Zucker")
hist(energySugarTot,main = "relativer Anteil an Zucker", xlab = "Zucker")
hist(energySugarTot,main = "relativer Anteil an Zucker", xlab = "Zucker", ylab = "Haeufigkeit")
hist(energySugarTot,main = "relativer Anteil an Zucker", xlab = "Zucker", ylab = "Haeufigkeit")
hist(energyFatTot,main = "relativer Anteil an Fett", xlab = "Fett", ylab = "Haeufigkeit")
hist(energyFibreTot,main = "relativer Anteil an Balaststoffe", xlab = "Balaststoffe", ylab = "Haeufigkeit")
hist(energyAlcTot,main = "relativer Anteil an Alkohol", xlab = "Alkohol", ylab = "Haeufigkeit")
hist(energyCarbTot,main = "relativer Anteil an Kohlenhydraten", xlab = "Kohlenhydraten", ylab = "Haeufigkeit")
hist(energyProteinTot,main = "relativer Anteil an Eiweiss", xlab = "Eiweiss", ylab = "Haeufigkeit")



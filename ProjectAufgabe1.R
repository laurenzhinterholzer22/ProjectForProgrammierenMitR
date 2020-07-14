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
  MedianPopulation = c(medianPopLsoa,medianPopMsoa,medianPopOsward,meadianPopBorough),
  MedianArea = c(medianAreaLsoa,medianAreaMsoa,medianAreaOsward,medianAreaBorough),
  stringsAsFactors = FALSE
)
tab1Dataframe

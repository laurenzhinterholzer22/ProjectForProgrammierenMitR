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
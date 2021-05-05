setwd("E:\\SP20\\Urban GIS\\Lab 8\\Bellingham\\Housing")
bell_rent <- read.csv('E:\\SP20\\Urban GIS\\Lab 8\\Bellingham\\Housing\\2018.csv')
med_rent <- bell_rent$MEDRENT.
hist(med_rent)
summary(med_rent)
med_ho <- bell_rent$MEDOWN.
hist(med_ho)
summary(med_ho)

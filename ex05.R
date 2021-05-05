setwd("E:\\FA20\\Stats\\Exercise 5\\G5050_Ex05")

# load data
COweather <- read.csv('CO_Climate02.csv')
head(COweather)

# libraries
library(maptools) # loads sp() too
library(rgdal)

# get the county outline shapefile, read the data
CO.shp <- readOGR(dsn = getwd(), layer = "CO_County03")
plot(CO.shp)
# plot the pre-projected station locations
plot(COweather$CO_X, COweather$CO_Y)

# set up a map of points to help you visualize things in R
COweather2 <- SpatialPoints(cbind(COweather$CO_X, COweather$CO_Y))
COweather2@proj4string <- CO.shp@proj4string
COweather2@bbox <- CO.shp@bbox
COweather2 <- SpatialPointsDataFrame(COweather2, data.frame(T_avg = COweather$ANN.TAVG.NORMAL))
# plot station locations
plot(COweather2, pch = 16)
# add county boundaries for reference
plot(CO.shp, add = TRUE)
# load the colors
library(RColorBrewer)
# select the palette
pal <- brewer.pal(5, "PuBu")
# plot the map
spplot(COweather2, "T_avg", col.regions = pal, cuts = 7)

# 1) set up libraries - gstat runs semivariograms
library(gstat)
library(lattice)

# 2) open a new Lattice graphics window
# not to be used in RStudio:
# trellis.device(color = TRUE, theme = 'col.whitebg')

# 3) set the data, set up graphics parameters; once this
# is in your script, you can change these variables and easily build new plots
attach(COweather)
# change this to whatever you want to analyze:
plotvar <- ANN.TAVG.NORMAL
plottitle <- "Annual Average Temperature"

# 4) first build a cloud plot - this doesn't bin the values
# you should see the utility of a binned variogram!
vgm.cloud <- variogram(plotvar ~1, loc = ~CO_X + CO_Y, data = COweather, cloud = T)
plot(vgm.cloud, main = paste("Variogram: ", plottitle), identify = FALSE)

# 5) build a basic variogram; remember you can always change the
# graphical parameters to customize the output
vgm <- variogram(plotvar ~1, loc = ~CO_X + CO_Y, data = COweather)
plot(vgm, pch = 16, type = "b", col = "red", main=paste("Variogram: ", plottitle))

# "build a variogram for average precipitation in mm"
plotvar <- ANN.PRCP.NORMAL
plottitle <- "Annual Average Precipitation"
prcp_vgm <- variogram(plotvar ~1, loc = ~CO_X + CO_Y, data = COweather)
plot(prcp_vgm, pch = 16, type = "b", col = "blue", main=paste("Variogram: ", plottitle))

# linear regression of the avg temp and elevation
model1 <- lm(ANN.TAVG.NORMAL~ELEVATION)
summary(model1)
plot(ANN.TAVG.NORMAL~ELEVATION, cex=.7, pch=19, xlab='Elevation (m)', ylab='Average Annual Temp', main = "Average Temperature in Relation to Elevation")

TempMod <- lm(ANN.TAVG.NORMAL~ELEVATION)    
CO_resid <- resid(TempMod)
par(mfrow = c(1,2))
plot(CO_X,CO_resid)
abline(lm(CO_resid~CO_X), col="red")
plot(CO_Y, CO_resid)
abline(lm(CO_resid~CO_Y), col="red")

cloud(CO_resid ~ CO_X*CO_Y, pch = 19, cex = .8, col='red')

plotvar <- CO_resid
plottitle <- "Residuals of Temperature in Colorado"
resid_vgm <- variogram(plotvar ~1, loc = ~CO_X + CO_Y, data = COweather)
plot(resid_vgm, pch = 16, type = "b", col = "blue", main=paste("Variogram: ", plottitle))

# directional variogram
plotvar <- CO_resid
plottitle <- "Temperatire Residuals in Colorado"
dir.vgm <- variogram(plotvar ~1, loc=~CO_X+CO_Y, data=COweather,
                     alpha=c(0,45,90,135))
plot(dir.vgm, main=paste("Directional variogram: ",plottitle)) 


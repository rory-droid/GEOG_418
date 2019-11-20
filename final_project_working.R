#Libraries

install.packages("sf")
library(sf)
install.packages("plyr")
library(plyr)
install.packages("dplyr")
library(dplyr)
install.packages("spdep")
library(spdep)
install.packages("GISTools")
library(GISTools)
install.packages("raster")
library(raster)
install.packages("maptools")
library(maptools)
install.packages("rgdal")
library(rgdal)
install.packages("spatsat")
library(spatstat)
install.packages("sp")
library(sp)
install.packages("tmap")
library(tmap)
install.packages("gstat")
library(gstat)


#Set working directory
setwd("Z:/Geog_418/Ze_Final_Project!")

#Reading in particulate matter dataset
pm25 <- read.csv("PM25.csv") #Read in PM2.5 data
#Select only columns 1 and 2
pm25 <- pm25[,1:2]
#Change the column names 
colnames(pm25) <- c("POSTALCODE", "PM25")
pm25 <- na.omit(pm25)

#Reading in postal code shapefile
postalcodes <- readOGR(dsn = ".", "BC_Postal_Codes") #Read in related postal code data

#Reading in dissemination tract and income data
income <- read.csv("Income.csv") #Read in census income data  
colnames(income) <- c("DAUID", "Income") #Select only ID and Income columns
census.tracts <- readOGR(dsn = ".","BC_DA") #Read in dissemination tract shapefile
income.tracts <- merge(census.tracts,income, by = "DAUID") #Merge income and dissemination data
nrow(income.tracts) #Determine the number of columns in the dataframe
income.tracts <- income.tracts[!is.na(income.tracts$Income),]

?choro.legend
#Create choropleth map of income
med.income <- income.tracts$Income
shades <- auto.shading(med.income, n=6, cols = brewer.pal(6, 'Reds'))
choropleth(income.tracts, med.income, shades) #map the data with associated colours
choro.legend(, , shades, cex = 10) #add a legend (you might need to change the location)

#Select postal codes that fall within dissemination tracts)
postalcodes <- intersect(postalcodes,income.tracts)
plot(postalcodes) #See what the data looks like spatially
head(postalcodes) #See what the data looks like in tabular form

#Join PM2.5 data with postal code data
pm25.spatial <- merge(postalcodes, pm25, by = "POSTALCODE")

#Aggregate the PM2.5 values in each DA in order to have a single value per DA. Here we aggregate based on the maximum.
pm25.aggregate <- aggregate((as.numeric(pm25.spatial$PM25)/10)~pm25.spatial$DAUID,FUN=max)
plot(pm25.aggregate)

#Re-join aggregated data to the income.tracts layer.
colnames(pm25.aggregate) <- c("DAUID", "PM25AGG") #Select only ID and Income columns
income.pm25 <- merge(income.tracts,pm25.aggregate, by = "DAUID") #Merge income and dissemination data

#Re-join aggregated data to the pm25.spatial points layer.
pm25.points.aggregate <- merge(pm25.spatial, pm25.aggregate, by = "DAUID")
View(pm25.points.aggregate@data)

#Create a subsample of the datapoints provided in the PM2.5 dataset using the sample n provided on CourseSpaces
sampleSize=250
spSample <- pm25.points.aggregate[sample(1:length(pm25.points.aggregate),sampleSize),]

#Create a grid called grd to use in your interpolation
# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(spSample, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
proj4string(grd) <- proj4string(spSample)
head(spSample)


proj4string(grd) <- proj4string(spSample)


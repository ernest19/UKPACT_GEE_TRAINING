###########################################################################
# 1.0 Load required libraries 
###########################################################################

library(sp)
library(raster)
library(rgdal)
library(randomForest)
library(ggplot2)
library(lattice)
library(caret)
library(sf)


###########################################################################
# 1.0 Set your working directory
###########################################################################

#the working directory is a folder where R reads and saves files

#From RStudio, use the menu to set/change your working directory under "Session > Set Working Directory > Choose Directory"
#Choose the directory you want to work in
# You can also use the command "setwd()" to set/change your working directory
# You can also use the command "getwd()" to get information on your working directory


setwd("~/Abuja Training Data/New folder")


###########################################################################
# 2.0 Loading and Exoloring Raster data in R  
###########################################################################

# In this section of the excercise we will learn how to import a satellite image into R
# Images can be loaded into R as Single bands or as a multi-band dataset
# There are three common packages (stars, raster and terra packages) available in R for working with raster datasets
# In this training we will be using the "raster package" 
# CLICK ON LINK TO LEARN MORE ABOUT THE PACKAGE https://rdrr.io/cran/raster/man/raster-package.html

# The 'raster' function in the raster package will be used to load the single bands of Landsat satellite dataset into R

Blue <- raster("T30NYM_20210126T102311_B02.jp2")
Green <- raster("T30NYM_20210126T102311_B03.jp2")
Red <- raster("T30NYM_20210126T102311_B04.jp2")
RedEdge1 <- raster("T30NYM_20210126T102311_B05.jp2")
RedEdge2 <- raster("T30NYM_20210126T102311_B06.jp2")
RedEdge3 <- raster("T30NYM_20210126T102311_B07.jp2")
NIR <- raster("T30NYM_20210126T102311_B08.jp2")
RedEdge4 <- raster("T30NYM_20210126T102311_B8A.jp2")
SWIR1 <- raster("T30NYM_20210126T102311_B11.jp2")
SWIR2 <- raster("T30NYM_20210126T102311_B12.jp2")


# Explore the properties of the images loaded 
# Take note of the follwing image properties

# class      
# dimensions(nrow, ncol, ncell)
# resolution (x, y)
# extent (xmin, xmax, ymin, ymax)
# coordinates reference system (crs)
# name
# values (min, max)

Blue 
Green 
Red 
RedEdge1 
RedEdge2 
RedEdge3 
NIR 
RedEdge4
SWIR1 
SWIR2 

# You will realise that the spectral Bands 2,3,4 and 8 have resolution = 10m
# and the spectral Bands 5,6,7,8A,11 and 12 has resolution of 20m
# In other to be able to work with all bands, we have to resamble all bands to same resolution
# In this training we will resample all bands to 10m
# This means spectral bands 5,6,7,8A,11 and 12 with resolution of 20m are going to be resampled to 10m
# We are going to do this using the 'disaggregate' function in the 'raster package'
# This will take some few minutes depending on the image size and memmory of your computer
# NB: The 'aggregate' function performs the vice


RedEdge1 <- disaggregate(RedEdge1, fact = 2) # 2 is the scale factor that converts 20 to 10
RedEdge2 <- disaggregate(RedEdge2, fact = 2)
RedEdge3 <- disaggregate(RedEdge3, fact = 2)
RedEdge4 <- disaggregate(RedEdge4, fact = 2)
SWIR1 <- disaggregate(SWIR1, fact = 2)
SWIR2 <- disaggregate(SWIR2, fact = 2)

# Now explore the properties of these bands again and compare the initial

RedEdge1 
RedEdge2 
RedEdge3 
RedEdge4
SWIR1 
SWIR2


# Right now, each raster is separate a object and they do not ‘speak’ to each other. 
# To perform any analysis, we need to combine the individual bands into one multi-band raster. 
# The stack function in the raster package will create one raster from the designated layers, 
# similar to the composite bands function in ArcGIS



Sentinel2 <- stack(Blue,
               Green,
               Red,
               RedEdge1,
               RedEdge2,
               RedEdge3,
               NIR,
               RedEdge4,
               SWIR1,
               SWIR2)

# Now explore the properties of the stacked

Sentinel2

# You will notice that the name property of the bands in the stacked images still retained the original file name
# We will want to change the the file names to the band names
# We will do this by creating a list with the names and assigning them to the bands in the stack image

names(Sentinel2)<-c("Blue",
                    "Green",
                    "Red",
                    "RedEdge1",
                    "RedEdge2",
                    "RedEdge3",
                    "NIR",
                    "RedEdge4",
                    "SWIR1",
                    "SWIR2")

# Now explore the properties of the stacked image again

Sentinel2

# Now that we know a little more about the imagery we are using, let’s plot it. 
# Since image is a multi-band raster, we use the plotRGB function from the raster package, 
# which allows us to specify what bands should be visualized.

# There are two main composites that are normally used in remote sensing: 
# the true color composite and the false color composite. 
# As the name suggests, the true color composite plots the imagery as it would appear to the human eye. 
# It uses the red band (3) for red, the green band (2) for green, and the blue band (1) for blue.


plotRGB(Sentinel2, r = 3, g = 2, b = 1, axes = TRUE, 
        stretch = "lin", main = "True Color Composite")



# Now lets try false colour composite
# It uses the NIR band (7) for red, the Red band (3) for green, and the Green band (2) for blue.


plotRGB(Sentinel2, r = 7, g = 3, b = 2, axes = TRUE, 
        stretch = "lin", main = "True Color Composite")


###########################################################################
# 3.0 Exoloring Raster data in R
###########################################################################




##########################################################################
# 2.0 how to load training and validation shapefile
##########################################################################

# To import shapefiles we use the sf function st_read() 
# st_read() requires the file path to the shapefile

FieldGPS <- st_read("TrainingData.shp")

FieldGPS


StudyArea <- st_read("StudyArea.shp")

Blue
StudyArea

lidar_chm_crop <- crop(Blue, StudyArea)

lidar_chm_crop

########################################################
#4.0 creating data frame with labeled training points containing all reflectance values
########################################################

LULC <- as.factor(FieldGPS$Class)
LULC
centres<- st_coordinates(FieldGPS)
centres
centres<- as.data.frame(st_coordinates(FieldGPS))
centres
lonlat<- st_geometry(FieldGPS)#coordinates(FieldGPS)
lonlat

trainingvalues<-extract(Sentinel2,centres)
trainingvalues<-as.data.frame(trainingvalues)
trainingvalues

trainingvalues<-cbind(LULC,trainingvalues)
trainingvalues

#head(trainingvalues)
#str(trainingvalues)
#summary(trainingvalues)


#########################################################
#5.0 splitting points into training and validation data
#########################################################

split <- sample(2,nrow(trainingvalues),replace = TRUE,prob = c(0.7,0.3))
training <- trainingvalues[split==1,]
validation <- trainingvalues[split==2,]
nrow(training)
nrow(validation)
training

#summary(training)
#head(training)

#########################################################################
#6.0 random forest model training
#########################################################################

set.seed(123)
fitControl <- trainControl(method = "repeatedcv",number = 5,repeats = 5)
#fitControl


RF_model <- train(LULC~.,data = training,method="rf",trControl=fitControl,prox=TRUE,
                  fitBest = FALSE,
                  returnData = TRUE)

print(RF_model)
plot(RF_model)
  
####checking parameters of the model

RF_model$finalModel


#############################################################################
#7.0 displaying variable importance
##########################################################################
RF_varImp <- varImp(RF_model, compete = FALSE)
plot(RF_varImp)

##########################################################################
#8.0 accuracy assessment
##########################################################################

pred_RF <- predict(RF_model$finalModel,
                   newdata = validation)
confusionMatrix(data = pred_RF, validation$LULC)
em_table <- table(data = pred_RF, validation$LULC)

#overall accuracy
overallaccuracy <-(sum(diag(em_table))/sum(em_table))*100
overallaccuracy

#user accUracy
UA <- (diag(em_table)/rowSums(em_table))*100
UA

#producer accuracy
PA<- (diag(em_table)/colSums(em_table))*100
PA


###########################################################################
#9.0 Performing classification using the RF classifier
##########################################################################
finalimage<-predict(myimage,RF_model)
#finalimage

##############################################################################
#10.0 write to wd the final LULC map
################################################################
#finalimage_2000 <- gplots(finalimage) + geom_raster(aes(fill = factor(value, labels=c("Built", "
#Closed forest", "Cropland", "Open forest", "Tree crops")))) + scale_fill_manual(values = c("brown", "fir green", "pink", "medium apple", "yellow"), name= "Land Cover") + ggtitle("Random Forest Classification") +theme(plot.title = element_text(lineheight=.4, face="bold")) + coord_equal()



writeRaster(finalimage,filename = ("C:/Users/Sinka Kadijah/Desktop/BIA/2000/2000_LULC"),format="GTiff",overwrite=TRUE)



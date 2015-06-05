## Random Forest on whole 2012 and 2014 rapideye data

## by Bart Huntley 05/06/2015


rm(list=ls())

is_installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
load_or_install<-function(package_names)  
{  
        for(package_name in package_names)  
        {  
                if(!is_installed(package_name))  
                {  
                        install.packages(package_name,repos="http://cran.csiro.au/")  
                }  
                library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)  
        }  
}  
load_or_install(c("sp","rgdal", "raster","randomForest"))

dir2012 <- "Z:\\DATA\\SatelliteMosaics\\Data\\RapidEye\\2012"
dir2014 <- "Z:\\IMAGERY\\RapidEye\\uncatelogued\\MitchellPlateau\\RapidEye_Ortho_processed"
dirW <- "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\raster-classification"
setwd(dir2012)
data.RE2012 <- "RapidEye-Kimberley-2012-GDA94-MGA51-Ortho.ers"

b1 <- raster(data.RE2012, band = 1)
b1@data@names <- "b1"

b2 <- raster(data.RE2012, band = 2)
b2@data@names <- "b2"

b3 <- raster(data.RE2012, band = 3)
b3@data@names <- "b3"

b4 <- raster(data.RE2012, band = 4)
b4@data@names <- "b4"

b5 <- raster(data.RE2012, band = 5)
b5@data@names <- "b5"

xvars <- stack(b1, b2, b3, b4, b5)

setwd(dirW)

sdata2 <- readOGR(dsn=getwd(), layer="training_buff25")

v2 <- as.data.frame(extract(xvars, sdata2))
sdata2@data = data.frame(sdata2@data, v2[match(rownames(sdata2@data), rownames(v2)),])

rf.mdl2 <- randomForest(x=sdata2@data[,5:ncol(sdata2@data)], y=as.factor(sdata2@data[,"type"]),
                        ntree=1000, importance=TRUE)
plot(rf.mdl2)
varImpPlot(rf.mdl2, type=1)
out2 <- predict(xvars, rf.mdl2, filename="rf_25buff_2012_all.img", type="response", 
                index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

##2014
setwd(dir2014)
data.RE2014 <- "o2014_Mitchell_Plateau_GDA94_MGA51_crosscal_data.ers"

b1.14 <- raster(data.RE2014, band = 1)
b1.14@data@names <- "b1"

b2.14 <- raster(data.RE2014, band = 2)
b1.14@data@names <- "b2"

b3.14 <- raster(data.RE2014, band = 3)
b1.14@data@names <- "b3"

b4.14 <- raster(data.RE2014, band = 4)
b1.14@data@names <- "b4"

b5.14 <- raster(data.RE2014, band = 5)
b1.14@data@names <- "b5"

xvars2 <- stack(b1.14, b2.14, b3.14, b4.14, b5.14)

setwd(dirW)

sdata3 <- readOGR(dsn=getwd(), layer="training_buff25")

v3 <- as.data.frame(extract(xvars2, sdata3))


sdata3@data = data.frame(sdata3@data, v3[match(rownames(sdata3@data), rownames(v3)),])

rf.mdl3 <- randomForest(x=sdata3@data[,5:ncol(sdata3@data)], y=as.factor(sdata3@data[,"type"]),
                        ntree=1000, importance=TRUE)
plot(rf.mdl3)
varImpPlot(rf.mdl3, type=1)
out3 <- predict(xvars2, rf.mdl3, filename="rf_25buff_2014_all.img", type="response", 
                index=1, na.rm=TRUE, progress="window", overwrite=TRUE)


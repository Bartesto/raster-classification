## Random Forest on whole 2014 Rapideye and Landsat data

## by Bart Huntley 15/06/2015


rm(list=ls())

## Functions to load and install necessary packages
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

## Set up 2 directories for image locations and working directory
dir2014 <- "Z:\\IMAGERY\\RapidEye\\uncatelogued\\MitchellPlateau\\RapidEye_Ortho_processed"
dirW <- "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\raster-classification"

#2012
## Random forest classification on 2012 Rapideye data
setwd(dir2014) # dir to image location
data.RE2014 <- "o2014_Mitchell_Plateau_GDA94_MGA51_crosscal_data.ers"


# For Rapideye
b1 <- raster(data.RE2014, band = 1)
b1@data@names <- "b1" # rename ind bands to something sensible

b2 <- raster(data.RE2014, band = 2)
b2@data@names <- "b2"

b3 <- raster(data.RE2014, band = 3)
b3@data@names <- "b3"

b4 <- raster(data.RE2014, band = 4)
b4@data@names <- "b4"

b5 <- raster(data.RE2014, band = 5)
b5@data@names <- "b5"

# For Landsat
setwd(dirW)
data.L2014 <- "L8_Kimberley_2014_mos_resamp5_chk.ers"

lb1 <- raster(data.L2014, band = 1)
lb1@crs <- b1@crs
lb1@data@names <- "lb1" # rename ind bands to something sensible

lb2 <- raster(data.L2014, band = 2)
lb2@crs <- b2@crs
lb2@data@names <- "lb2"

lb3 <- raster(data.L2014, band = 3)
lb3@crs <- b3@crs
lb3@data@names <- "lb3"

lb4 <- raster(data.L2014, band = 4)
lb4@crs <- b4@crs
lb4@data@names <- "lb4"

lb5 <- raster(data.L2014, band = 5)
lb5@crs <- b5@crs
lb5@data@names <- "lb5"

lb6 <- raster(data.L2014, band = 6)
lb6@crs <- b1@crs #no band 6 in RE
lb6@data@names <- "lb6"



xvars <- stack(b1, b2, b3, b4, b5, lb1, lb2, lb3, lb4, lb5, lb6)

setwd(dirW) # dir to working location

sdata2 <- readOGR(dsn=getwd(), layer="training_buff25") # read in training point shape file

v2 <- as.data.frame(extract(xvars, sdata2))
sdata2@data = data.frame(sdata2@data, v2[match(rownames(sdata2@data), rownames(v2)),])

rf.mdl2 <- randomForest(x=sdata2@data[,5:ncol(sdata2@data)], y=as.factor(sdata2@data[,"type"]),
                        ntree=501, importance=TRUE)
plot(rf.mdl2)
varImpPlot(rf.mdl2, type=1)
out2 <- predict(xvars, rf.mdl2, filename="rf_25buff_ALL_2012_RE_L.img", type="response", 
                datatype = 'INT1U', index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

importance(rf.mdl2)


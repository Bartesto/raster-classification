## Random Forest on whole 2012 and Landsat

## by Bart Huntley 05/06/2015


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

## Set up 3 directories for image locations and working directory
# dir2012 <- "Z:\\DATA\\SatelliteMosaics\\Data\\RapidEye\\2012"
# dirL2012 <- paste0("Z:\\DEC\\Kimberley_Science_and_Sustainability_Strategy\\",
#                    "Working\\Mitchell_Plateau\\Rapideye")

dirW <- "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\raster-classification"

#2012
## Random forest classification on 2012 Rapideye data
setwd(dirW) # dir to image location
data.RE2012 <- "RE-Warrender-2012-GDA94-MGA51_for_test.ers"


# For Rapideye
b1 <- raster(data.RE2012, band = 1)
b1@data@names <- "b1" # rename ind bands to something sensible

b2 <- raster(data.RE2012, band = 2)
b2@data@names <- "b2"

b3 <- raster(data.RE2012, band = 3)
b3@data@names <- "b3"

b4 <- raster(data.RE2012, band = 4)
b4@data@names <- "b4"

b5 <- raster(data.RE2012, band = 5)
b5@data@names <- "b5"

# For Landsat
# setwd(dirL2012)
data.L2012 <- "L7_2012_Warrender_test_5m_cr.ers"

lb1 <- raster(data.L2012, band = 1)
lb1@data@names <- "lb1" # rename ind bands to something sensible

lb2 <- raster(data.L2012, band = 2)
lb2@data@names <- "lb2"

lb3 <- raster(data.L2012, band = 3)
lb3@data@names <- "lb3"

lb4 <- raster(data.L2012, band = 4)
lb4@data@names <- "lb4"

lb5 <- raster(data.L2012, band = 5)
lb5@data@names <- "lb5"

lb6 <- raster(data.L2012, band = 6)
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
out2 <- predict(xvars, rf.mdl2, filename="rf_25buff_test_2012_RE_L.img", type="response", 
                datatype = 'INT1U', index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

importance(rf.mdl2)

#2014
## Random forest classification on 2012 Rapideye data
setwd(dirW) # dir to image location
data.RE2014 <- "RE-Warrender-2014-GDA94-MGA51_cal_for_test_cr2.bil"


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
# setwd(dirL2012)
data.L2014 <- "L8_2014_Warrender_test_5m_cr2.ers"

lb1 <- raster(data.L2014, band = 1)
lb1@data@names <- "lb1" # rename ind bands to something sensible

lb2 <- raster(data.L2014, band = 2)
lb2@data@names <- "lb2"

lb3 <- raster(data.L2014, band = 3)
lb3@data@names <- "lb3"

lb4 <- raster(data.L2014, band = 4)
lb4@data@names <- "lb4"

lb5 <- raster(data.L2014, band = 5)
lb5@data@names <- "lb5"

lb6 <- raster(data.L2014, band = 6)
lb6@data@names <- "lb6"

##Couldn't get extents to line up so chnged manually
lb1@extent@xmin <- 793880
lb1@extent@xmax <- 806230
lb1@extent@ymin <- 8382080
lb1@extent@ymax <- 8395900

lb2@extent@xmin <- 793880
lb2@extent@xmax <- 806230
lb2@extent@ymin <- 8382080
lb2@extent@ymax <- 8395900

lb3@extent@xmin <- 793880
lb3@extent@xmax <- 806230
lb3@extent@ymin <- 8382080
lb3@extent@ymax <- 8395900

lb4@extent@xmin <- 793880
lb4@extent@xmax <- 806230
lb4@extent@ymin <- 8382080
lb4@extent@ymax <- 8395900

lb5@extent@xmin <- 793880
lb5@extent@xmax <- 806230
lb5@extent@ymin <- 8382080
lb5@extent@ymax <- 8395900

lb6@extent@xmin <- 793880
lb6@extent@xmax <- 806230
lb6@extent@ymin <- 8382080
lb6@extent@ymax <- 8395900

xvars <- stack(b1, b2, b3, b4, b5, lb1, lb2, lb3, lb4, lb5, lb6)

setwd(dirW) # dir to working location

sdata2 <- readOGR(dsn=getwd(), layer="training_buff25") # read in training point shape file

v2 <- as.data.frame(extract(xvars, sdata2))
sdata2@data = data.frame(sdata2@data, v2[match(rownames(sdata2@data), rownames(v2)),])

rf.mdl2 <- randomForest(x=sdata2@data[,5:ncol(sdata2@data)], y=as.factor(sdata2@data[,"type"]),
                        ntree=501, importance=TRUE)
plot(rf.mdl2)
varImpPlot(rf.mdl2, type=1)
out2 <- predict(xvars, rf.mdl2, filename="rf_25buff_test_2014_RE_L.img", type="response", 
                datatype = 'INT1U', index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

## Random Forest on whole 2012 Rapideye and Landsat data

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

## Set up 3 directories for image locations and working directory
dir2012 <- "Z:\\DATA\\SatelliteMosaics\\Data\\RapidEye\\2012"
# dirL2012 <- paste0("Z:\\DEC\\Kimberley_Science_and_Sustainability_Strategy\\",
#                    "Working\\Mitchell_Plateau\\Rapideye")

dirW <- "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\raster-classification"

#2012
## Random forest classification on 2012 Rapideye data
setwd(dir2012) # dir to image location
data.RE2012 <- "RapidEye-Kimberley-2012-GDA94-MGA51-Ortho_20150717.ers"


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
setwd(dirW)
data.L2012 <- "L7_Kimberley_2012_mos_resamp5_chk.ers"

lb1 <- raster(data.L2012, band = 1)
lb1@crs <- b1@crs
lb1@data@names <- "lb1" # rename ind bands to something sensible

lb2 <- raster(data.L2012, band = 2)
lb2@crs <- b2@crs
lb2@data@names <- "lb2"

lb3 <- raster(data.L2012, band = 3)
lb3@crs <- b3@crs
lb3@data@names <- "lb3"

lb4 <- raster(data.L2012, band = 4)
lb4@crs <- b4@crs
lb4@data@names <- "lb4"

lb5 <- raster(data.L2012, band = 5)
lb5@crs <- b5@crs
lb5@data@names <- "lb5"

lb6 <- raster(data.L2012, band = 6)
lb6@crs <- b1@crs #no band 6 in RE
lb6@data@names <- "lb6"

lb1@extent@xmin <- b1@extent@xmin
lb1@extent@xmax <- b1@extent@xmax
lb1@extent@ymin <- b1@extent@ymin
lb1@extent@ymax <- b1@extent@ymax
lb1@nrows <- b1@nrows

lb2@extent@xmin <- b2@extent@xmin
lb2@extent@xmax <- b2@extent@xmax 
lb2@extent@ymin <- b2@extent@ymin
lb2@extent@ymax <- b2@extent@ymax
lb2@nrows <- b2@nrows

lb3@extent@xmin <- b3@extent@xmin
lb3@extent@xmax <- b3@extent@xmax
lb3@extent@ymin <- b3@extent@ymin
lb3@extent@ymax <- b3@extent@ymax
lb3@nrows <- b3@nrows

lb4@extent@xmin <- b4@extent@xmin
lb4@extent@xmax <- b4@extent@xmax
lb4@extent@ymin <- b4@extent@ymin
lb4@extent@ymax <- b4@extent@ymax
lb4@nrows <- b4@nrows

lb5@extent@xmin <- b5@extent@xmin
lb5@extent@xmax <- b5@extent@xmax
lb5@extent@ymin <- b5@extent@ymin
lb5@extent@ymax <- b5@extent@ymax
lb5@nrows <- b5@nrows
# 
lb6@extent@xmin <- b5@extent@xmin
lb6@extent@xmax <- b5@extent@xmax
lb6@extent@ymin <- b5@extent@ymin
lb6@extent@ymax <- b5@extent@ymax
lb6@nrows <- b5@nrows

xvars <- stack(b1, b2, b3, b4, b5, lb1, lb2, lb3, lb4, lb5, lb6)

setwd(dirW) # dir to working location

sdata2 <- readOGR(dsn=getwd(), layer="training_buff25") # read in training point shape file

v2 <- as.data.frame(extract(xvars, sdata2))
sdata2@data = data.frame(sdata2@data, v2[match(rownames(sdata2@data), rownames(v2)),])

rf.mdl2 <- randomForest(x=sdata2@data[,5:ncol(sdata2@data)], y=as.factor(sdata2@data[,"type"]),
                        ntree=501, importance=TRUE)
plot(rf.mdl2)
varImpPlot(rf.mdl2, type=1)
out2 <- predict(xvars, rf.mdl2, filename="rf_25buff_ALL_2012_RE_L_20150717.img", type="response", 
                datatype = 'INT1U', index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

importance(rf.mdl2)


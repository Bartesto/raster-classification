## Test method for supervised classification - random Forest

## by Bart Huntley 04/06/2015


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

dir <- "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\raster-classification"

setwd(dir)

##2012 rapideye
data.RE <- "RE-Warrender-ALT2014-GDA94-MGA51_cal_for_test.bil"

b1 <- raster(data.RE, band = 1)
b1@data@names <- "b1"

b2 <- raster(data.RE, band = 2)
b2@data@names <- "b2"

b3 <- raster(data.RE, band = 3)
b3@data@names <- "b3"

b4 <- raster(data.RE, band = 4)
b4@data@names <- "b4"

b5 <- raster(data.RE, band = 5)
b5@data@names <- "b5"

xvars <- stack(b1, b2, b3, b4, b5)

##based on points in 2012 mask
sdata <- readOGR(dsn=getwd(), layer="training_buff25_ALT2014")


v <- as.data.frame(extract(xvars, sdata))
sdata@data = data.frame(sdata@data, v[match(rownames(sdata@data), rownames(v)),])

rf.mdl <- randomForest(x=sdata@data[,5:ncol(sdata@data)], y=as.factor(sdata@data[,"type"]),
                       ntree=501, importance=TRUE)
plot(rf.mdl)
varImpPlot(rf.mdl, type=1)
out <- predict(xvars, rf.mdl, filename="rf_ALT2014_data_test.img", type="response", 
        index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

##based on points buffered in 25m in 2012 mask
sdata2 <- readOGR(dsn=getwd(), layer="training_buff25")


v2 <- as.data.frame(extract(xvars, sdata2))
sdata2@data = data.frame(sdata2@data, v2[match(rownames(sdata2@data), rownames(v2)),])

rf.mdl2 <- randomForest(x=sdata2@data[,5:ncol(sdata2@data)], y=as.factor(sdata2@data[,"type"]),
                       ntree=501, importance=TRUE)
plot(rf.mdl2)
varImpPlot(rf.mdl2, type=1)
out2 <- predict(xvars, rf.mdl2, filename="rf_25buff_test.img", type="response", 
               index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

importance(rf.mdl2)

##2014 rapideye
data.RE2014 <- "RE-Warrender-2014-GDA94-MGA51_cal_for_test.bil"

b1_4 <- raster(data.RE2014, band = 1)
b1_4@data@names <- "b1"

b2_4 <- raster(data.RE2014, band = 2)
b2_4@data@names <- "b2"

b3_4 <- raster(data.RE2014, band = 3)
b3_4@data@names <- "b3"

b4_4 <- raster(data.RE2014, band = 4)
b4_4@data@names <- "b4"

b5_4 <- raster(data.RE2014, band = 5)
b5_4@data@names <- "b5"

xvars4 <- stack(b1_4, b2_4, b3_4, b4_4, b5_4)


##2014 25 buffer
sdata3 <- readOGR(dsn=getwd(), layer="training_buff25")


v3 <- as.data.frame(extract(xvars, sdata3))
sdata3@data = data.frame(sdata3@data, v3[match(rownames(sdata3@data), rownames(v3)),])

rf.mdl3 <- randomForest(x=sdata3@data[,5:ncol(sdata3@data)], y=as.factor(sdata3@data[,"type"]),
                        ntree=501, importance=TRUE)
plot(rf.mdl3)
varImpPlot(rf.mdl3, type=1)
out3 <- predict(xvars4, rf.mdl3, filename="rf_25buff_2014_test.img", type="response", 
                index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

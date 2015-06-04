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
data.RE <- "RE-Warrender-2012-GDA94-MGA51_for_test.ers"

b1 <- raster(data.RE, band = 1)
b2 <- raster(data.RE, band = 2)
b3 <- raster(data.RE, band = 3)
b4 <- raster(data.RE, band = 4)
b5 <- raster(data.RE, band = 5)

xvars <- stack(b1, b2, b3, b4, b5)

##based on points in 2012 mask
sdata <- readOGR(dsn=getwd(), layer="training")


v <- as.data.frame(extract(xvars, sdata))
sdata@data = data.frame(sdata@data, v[match(rownames(sdata@data), rownames(v)),])

rf.mdl <- randomForest(x=sdata@data[,5:ncol(sdata@data)], y=as.factor(sdata@data[,"type"]),
                       ntree=501, importance=TRUE)
plot(rf.mdl)
varImpPlot(rf.mdl, type=1)
out <- predict(xvars, rf.mdl, filename="RfClassPred.img", type="response", 
        index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

##based on points buffered in 25m in 2012 mask
sdata2 <- readOGR(dsn=getwd(), layer="training")


v2 <- as.data.frame(extract(xvars, sdata2))
sdata2@data = data.frame(sdata2@data, v2[match(rownames(sdata2@data), rownames(v2)),])

rf.mdl2 <- randomForest(x=sdata2@data[,5:ncol(sdata2@data)], y=as.factor(sdata2@data[,"type"]),
                       ntree=501, importance=TRUE)
plot(rf.mdl2)
varImpPlot(rf.mdl2, type=1)
out2 <- predict(xvars, rf.mdl2, filename="RfClassPred2.img", type="response", 
               index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

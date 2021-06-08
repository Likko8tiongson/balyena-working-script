#install.packages("tidyverse","sf")
library(tidyverse)
#install.packages("sf")
library(sf)
balyena <- read_csv("All points for locoh.csv")

#install.packages("rgdal")
library(rgdal)
head(balyena)
summary(balyena)
names(balyena)
cleanbalyenadataset <- balyena[, c("Closest Lat", "Closest Long")]
head(cleanbalyenadataset)
cleanbalyenadataset


library(sf)
library(tidyverse)
summary(cleanbalyenadataset)
##### overall spinner k-LoCoH muna...trying to figure out how to take long and lat
#install.packages("dplyr")
cleanbalyenadataset
cleanbalyenadataset <- sapply( cleanbalyenadataset, as.numeric )
cleanbalyenadataset
#select(cleanbalyenadataset, Closest Long, Closest Lat) -> balyenaMC
head(cleanbalyenadataset)
cleanbalyenadataset[,c("Closest Long", "Closest Lat")]
library(dplyr)

na.omit(cleanbalyenadataset) -> balyena_no_NA
head(balyena_no_NA)
balyena_no_NA
#coordinates(Spinnerthesis_no_NA) = ~LATITUDE+LONGITUDE

balyena.latlong <- SpatialPoints (balyena_no_NA[ , c("Closest Long","Closest Lat")], proj4string=CRS("+proj=longlat +ellps=WGS84"))
plot(balyena.latlong)
balyena.sp.utm <- spTransform(balyena.latlong, CRS("+proj=utm +north +zone=51 +ellps=WGS84"))
plot(balyena.sp.utm)
head(balyena.sp.utm)
balyena.sp.utm
##removing outlier data point (this was the pilot whale, and also one point on land in Moalboal) DONE!!!!!!!
balyena.mat.utm <- coordinates(balyena.sp.utm)
head(balyena.mat.utm)
colnames(balyena.mat.utm) <- c("x", "y")
head(balyena.mat.utm)
##Now generate k-LoCoH time!!

#installing tlocoh


install.packages("tlocoh", dependencies=T, repos=c("http://R-Forge.R-project.org" , "http://cran.cnr.berkeley.edu"))
install.packages("tlocoh")
install.packages("tlocoh", dependencies=T, repos=c("http://R-Forge.R-project.org"))
install.packages("tlocoh", dependencies=TRUE, repos=c("http://cran.cnr.berkeley.edu"))
require(tlocoh)
library(tlocoh)
library(rgdal)
balyena.lxy <- xyt.lxy(xy=balyena.mat.utm, id="humpback", proj4string=CRS("+proj=utm +north +zone=51 +ellps=WGS84"))
plot(balyena.lxy)

balyena.lxy <- lxy.nn.add(balyena.lxy, s=0, k=70)
#Summary(balyena.lxy)

#k=7
balyena.lhs <- lxy.lhs(balyena.lxy, k=7*1:2, s=0)
plot(balyena.lhs, hulls=TRUE, figs.per.page=2)
balyenak70.lhs <- lhs.iso.add(balyena.lhs)
plot(balyenak70.lhs, iso=TRUE, figs.per.page=4)


## save shapefile firstplot(Spinnerthesis.lhs.k32, iso=TRUE)
lhs.plot.isoarea(balyenak70.lhs)
lhs.plot.isoear(balyenak70.lhs)

#k=12
#most appropriate K
balyenak12.lhs <- lxy.lhs(balyena.lxy, k=12*1:2, s=0)
plot(balyenak12.lhs, hulls=TRUE, figs.per.page=2)
balyenak12.lhs <- lhs.iso.add(balyenak12.lhs)
plot(balyenak12.lhs, iso=TRUE, figs.per.page=4)

balyena.lhs.k12 <- lhs.select(balyenak12.lhs, k=12)

plot(balyena.lhs.k12, iso=TRUE)

#k=32
balyenak32.lhs <- lxy.lhs(balyena.lxy, k=32*1:2, s=0)
plot(balyenak32.lhs, hulls=TRUE, figs.per.page=2)
balyenak32.lhs <- lhs.iso.add(balyenak32.lhs)
plot(balyenak32.lhs, iso=TRUE, figs.per.page=4)

balyena.lhs.k32 <- lhs.select(balyenak32.lhs, k=32)

plot(balyena.lhs.k32, iso=TRUE)

#k=16
balyenak16.lhs <- lxy.lhs(balyena.lxy, k=16*1:2, s=0)
plot(balyenak16.lhs, hulls=TRUE, figs.per.page=2)
balyenak16.lhs <- lhs.iso.add(balyenak16.lhs)
plot(balyenak16.lhs, iso=TRUE, figs.per.page=4)

balyena.lhs.k16 <- lhs.select(balyenak16.lhs, k=16)

plot(balyena.lhs.k16, iso=TRUE)


#Spinnerthesis.lhs.k32 <- lhs.select(Spinnerthesis.lhs, k=32)
#plot(Spinnerthesis.lhs.k28, iso=TRUE)
#plot(Spinnerthesis.lhs.k32, iso=TRUE)

plot(balyena.lhs.k16, iso=TRUE)
lhs.exp.shp(balyena.lhs.k12,iso=T)
lhs.exp.shp(balyena.lhs.k32,iso=T)
lhs.exp.shp(balyena.lhs.k16, iso=T)
#lhs.exp.shp(Spinnerthesis.lhs.k32, iso=T)
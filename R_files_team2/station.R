library(ggmap)
library(tidyverse)
library(dplyr)
library(NbClust)
library(factoextra)
library(ggplot2)

station_details <- read.csv(file = "F:/UTA/Courses/Assignments/Data Mining/Project2/stations.csv",
                            header = TRUE, sep = ",",stringsAsFactors = FALSE,
                            na.strings = c("NA","N/A","Unknown","unknown",".P"))
head(station_details)
station<-data.frame(station_details["StationNumber"],station_details["Lat"],station_details["Lon"])
qplot(Lat, Lon, data= station)


c1<- read.csv(file = "F:/UTA/Courses/Assignments/Data Mining/Project2/station_cluster.csv",
              header = TRUE, sep = ",",stringsAsFactors = FALSE,
              na.strings = c("NA","N/A","Unknown","unknown",".P"))
c1[1] <- list(NULL)
commonstation<-intersect(station$StationNumber,c1$StationNumber)

c2<- data.frame(commonstation)
colnames(c2)=c("StationNumber")

s<- merge(station, c2, by="StationNumber")
s<- merge(station,c1, by="StationNumber")
s1 <- sqldf("SELECT DISTINCT s.* from s")
s1<-s1[!duplicated(s1[1]),]

ggplot()  +
  geom_point(data=s1, aes(x = Lon, y = Lat, color = cluster), 
             size = 2, alpha=1)

ggplot(data=s1, mapping=aes(x = Lon, y = Lat,color = cluster))  +
  geom_point(size = 2, alpha=1)+ggtitle('Visualization of clusters on Texas Map for year 2007-2008')


qplot(Lon,Lat, data = s1, color=cluster)

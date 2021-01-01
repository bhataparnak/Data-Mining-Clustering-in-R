library(dplyr)
library(lubridate)
library(ggplot2)
library(sqldf)
library(amap)
library(reprex)
library(cluster)
library(fpc)
library(NbClust)
library(factoextra)
library(clv)
library(clusteval)
library(FactoMineR)
library(tidyverse)


feb_2007  <- read.csv(file = "F:/UTA/Courses/Assignments/Data Mining/Project2/feb2007.csv",
                      header =  TRUE, sep = ",",stringsAsFactors = FALSE,
                      na.strings = c("NA","N/A","Unknown","unknown",".P"))
feb_2007[1] <- list(NULL)
feb_2007[3] <- list(NULL)
feb_2007[16] <- list(NULL)

df_feb_2007 <- as.data.frame(feb_2007)
df1<-data.frame(df_feb_2007["Count_STP"],df_feb_2007["Count_WDSP"],df_feb_2007["Count_DewP"])

#distance matrix computation and visualization
dist_2007 <- get_dist(df1)
fviz_dist(dist_2007, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"), lab_size = 8)


set.seed(5)

wss <- function(k) {
  kmeans(df1, k, nstart = 4 )$tot.withinss
}

# Compute and plot wss for k = 2 to k = 8
k.values <- 2:8

wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(5)
fviz_nbclust(df1, kmeans, method = "wss")+
  geom_vline(xintercept = 3,linetype=2)+
  labs(subtitle = "withinSS")

set.seed(5)
final <- kmeans(df1, 3, nstart = 4)
print(final)

fviz_cluster(final, data = df1)

df_feb_2007 %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

c1<- cbind(final$cluster)
clusplot(df1, final$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

set.seed(5)
clusters_euclidean_2007<- Kmeans(df1, centers=3,iter.max = 400, nstart = 4,
                                 method = "euclidean")
#[1] withinss - 195.570312  41.743471   4.798454

clusters_correlation_2007 <- Kmeans(df1, centers=3,iter.max = 400, nstart = 4,
                                    method = "correlation")

#[1] withinss - 0.000000e+00 4.930381e-32 0.000000e+00

plotcluster(df1, clusters_euclidean_2007$cluster)
clusplot(df1, clusters_euclidean_2007$cluster,  color=T, shade=F,labels=0,lines=0, main='k-Means Cluster Analysis')
plot(df1,col=clusters_euclidean_2007$cluster)
plot(df1,col=clusters_correlation_2007$cluster)

jaccard_2007 = cluster_similarity(clusters_euclidean_2007$cluster
                                  , clusters_correlation_2007$cluster, similarity="jaccard", method="independence")
# 0.44458052
table(df_feb_2007$Count_STP,clusters_correlation_2007$cluster)

df2 <- data.frame(df_feb_2007["DewP"],df_feb_2007["WDSP"],df_feb_2007["STP"],df_feb_2007["Temp"])


set.seed(5)

wss <- function(k) {
  kmeans(df2, k, nstart = 4 )$tot.withinss
}

# Compute and plot wss for k = 2 to k = 8
k.values <- 2:8

wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(5)
fviz_nbclust(df2, kmeans, method = "wss")+
  geom_vline(xintercept = 3,linetype=2)+
  labs(subtitle = "withinSS")

set.seed(5)
final <- kmeans(df2, 3, nstart = 4)
print(final)

fviz_cluster(final, data = df2)

df_feb_2007 %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

c1<- cbind(final$cluster)
clusplot(df2, final$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)



set.seed(5)
clusters_euclidean_2007 <- Kmeans(df2, centers = 3,iter.max = 400, nstart = 4,
                                  method = "euclidean")
#[1] 34.7159    827.7353 436432.9143
clusters_correlation_2007 <- Kmeans(df2, centers = 3,iter.max = 400, nstart = 4,
                                    method = "correlation")
#[1] 3.164241e-16 1.168329e-08 1.550169e-14
points(clusters_euclidean_2007$center,col=1:2,pch=8,cex=1)

#nstart is the random number generator

plotcluster(df2, clusters_euclidean_2007$cluster)

clusplot(df2, clusters_euclidean_2007$cluster,  color=T, shade=F,labels=0,lines=0, main='k-Means Cluster Analysis euclidean')
clusplot(df2, clusters_correlation_2007$cluster,  color=T, shade=F,labels=0,lines=0, main='k-Means Cluster Analysis correlation')
plot(df2,col=clusters_euclidean_2007$cluster)

plot(df2,col=clusters_correlation_2007$cluster)

plot(df2)
jaccard_val = cluster_similarity(clusters_euclidean_2007$cluster
                                 , clusters_correlation_2007$cluster, similarity="jaccard", method="independence")

#0.75822686


set.seed(30)

wss <- function(k) {
  kmeans(df1, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 2 to k = 8
k.values <- 2:8

wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(30)
fviz_nbclust(df1, kmeans, method = "wss")+
  geom_vline(xintercept = 2,linetype=2)+
  labs(subtitle = "withinSS")

set.seed(30)
final <- kmeans(df1, 2, nstart = 10)
print(final)

fviz_cluster(final, data = df1)

df_feb_2007 %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

c1<- cbind(final$cluster)
clusplot(df1, final$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

set.seed(30)
clusters_euclidean_2007<- Kmeans(df1, centers=2,iter.max = 400, nstart = 10,
                                 method = "euclidean")
#[1] withinss - 9.49320573 0.00382011

clusters_correlation_2007 <- Kmeans(df1, centers=2,iter.max = 400, nstart = 10,
                                    method = "correlation")

#[1] withinss - 0.000000e+00 4.930381e-32

plotcluster(df1, clusters_euclidean_2007$cluster)
clusplot(df1, clusters_euclidean_2007$cluster,  color=T, shade=F,labels=0,lines=0, main='k-Means Cluster Analysis')
plot(df1,col=clusters_euclidean_2007$cluster)
plot(df1,col=clusters_correlation_2007$cluster)


jaccard_2007 = cluster_similarity(clusters_euclidean_2007$cluster
                                  , clusters_correlation_2007$cluster, similarity="jaccard", method="independence")
# 0.801169
table(df_feb_2007$Count_STP,clusters_correlation_2007$cluster)

df2 <- data.frame(df_feb_2007["DewP"],df_feb_2007["WDSP"],df_feb_2007["STP"],df_feb_2007["Temp"])


set.seed(30)

wss <- function(k) {
  kmeans(df2, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 2 to k = 8
k.values <- 2:8

wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(30)
fviz_nbclust(df2, kmeans, method = "wss")+
  geom_vline(xintercept = 2,linetype=2)+
  labs(subtitle = "withinSS")

set.seed(30)
final <- kmeans(df2, 2, nstart = 10)
print(final)

fviz_cluster(final, data = df2)

df_feb_2007 %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

c1<- cbind(final$cluster)
clusplot(df2, final$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)



set.seed(30)
clusters_euclidean_2007 <- Kmeans(df2, centers = 2,iter.max = 400, nstart = 10,
                                  method = "euclidean")
#[1] 4.796944e+07 8.277353e+02
clusters_correlation_2007 <- Kmeans(df2, centers = 2,iter.max = 400, nstart = 10,
                                    method = "correlation")
#[1]  1.506864e-06 1.168329e-08
points(clusters_euclidean_2007$center,col=1:2,pch=8,cex=1)

#nstart is the random number generator

plotcluster(df2, clusters_euclidean_2007$cluster)

clusplot(df2, clusters_euclidean_2007$cluster,  color=T, shade=F,labels=0,lines=0, main='k-Means Cluster Analysis')

plot(df2,col=clusters_euclidean_2007$cluster)

plot(df2,col=clusters_correlation_2007$cluster)

plot(df2)
jaccard_val = cluster_similarity(clusters_euclidean_2007$cluster
                                 , clusters_correlation_2007$cluster, similarity="jaccard", method="independence")

#0.746955



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


feb_2009  <- read.csv(file = "F:/UTA/Courses/Assignments/Data Mining/Project2/feb2009.csv",
                      header =  TRUE, sep = ",",stringsAsFactors = FALSE,
                      na.strings = c("NA","N/A","Unknown","unknown",".P"))
feb_2009[1] <- list(NULL)
feb_2009[3] <- list(NULL)
feb_2009[16] <- list(NULL)
#due to NaN value for SNDP
feb_2009[16] <- list(NULL)

df_feb_2009 <- as.data.frame(feb_2009)

df1<- data.frame(df_feb_2009["Count_STP"],df_feb_2009["Count_WDSP"],df_feb_2009["Count_DewP"])



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

df_feb_2009 %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")


c1<- cbind(final$cluster)
clusplot(df1, final$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

set.seed(5)
clusters_euclidean_2009<- Kmeans(df1, centers=3,iter.max = 400, nstart = 4,
                                 method = "euclidean")

clusters_correlation_2009 <- Kmeans(df1, centers=3,iter.max = 400, nstart = 4,
                                    method = "correlation")

plotcluster(df1, clusters_euclidean_2009$cluster)
clusplot(df1, clusters_euclidean_2009$cluster,  color=T, shade=F,labels=0,lines=0, main='k-Means Cluster Analysis')
plot(df1,col=clusters_euclidean_2009$cluster)
plot(df1,col=clusters_correlation_2009$cluster)


jaccard_2009 = cluster_similarity(clusters_euclidean_2009$cluster
                                  , clusters_correlation_2009$cluster, similarity="jaccard", method="independence")

# 0.719963

df2 <- data.frame(df_feb_2009["DewP"],df_feb_2009["WDSP"],df_feb_2009["STP"],df_feb_2009["Temp"])

dist_2009 <- get_dist(df2)
fviz_dist(dist_2009, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
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
  geom_vline(xintercept = 6,linetype=2)+
  labs(subtitle = "withinSS")

set.seed(5)
final <- kmeans(df2, 6, nstart = 4)
print(final)

fviz_cluster(final, data = df2)

df_feb_2009 %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

set.seed(5)
clusters_euclidean_2009 <- Kmeans(df2, centers = 6,iter.max = 400, nstart = 4,
                                  method = "euclidean")
#  2269.5424    261.4165    245.5641 596391.2711      0.0000   1822.0882
clusters_correlation_2009 <- Kmeans(df2, centers = 6,iter.max = 400, nstart = 4,
                                    method = "correlation")

# 8.939285e-09 3.254868e-06 1.858646e-09 1.830305e-11 9.535004e-09 7.083919e-09

#nstart is the random number generator

plotcluster(df2, clusters_euclidean_2009$cluster)

clusplot(df2, clusters_euclidean_2009$cluster,  color=T, shade=F,labels=0,lines=0, main='k-Means Cluster Analysis euclidean')
clusplot(df2, clusters_correlation_2009$cluster,  color=T, shade=F,labels=0,lines=0, main='k-Means Cluster Analysis correlation')
plot(df2,col=clusters_euclidean_2009$cluster)

plot(df2,col=clusters_correlation_2009$cluster)

plot(df2)
jaccard_val = cluster_similarity(clusters_euclidean_2009$cluster
                                 , clusters_correlation_2009$cluster, similarity="jaccard", method="independence")
#0.384089

df1<- data.frame(df_feb_2009["Count_STP"],df_feb_2009["Count_WDSP"],df_feb_2009["Count_DewP"])



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
  geom_vline(xintercept = 3,linetype=2)+
  labs(subtitle = "withinSS")

set.seed(30)
final <- kmeans(df1, 3, nstart = 10)
print(final)


fviz_cluster(final, data = df1)

df_feb_2009 %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")


c1<- cbind(final$cluster)
clusplot(df1, final$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

set.seed(30)
clusters_euclidean_2009<- Kmeans(df1, centers=3,iter.max = 400, nstart = 10,
                                 method = "euclidean")

clusters_correlation_2009 <- Kmeans(df1, centers=3,iter.max = 400, nstart = 10,
                                    method = "correlation")

plotcluster(df1, clusters_euclidean_2009$cluster)
clusplot(df1, clusters_euclidean_2009$cluster,  color=T, shade=F,labels=0,lines=0, main='k-Means Cluster Analysis')
plot(df1,col=clusters_euclidean_2009$cluster)
plot(df1,col=clusters_correlation_2009$cluster)


jaccard_2009 = cluster_similarity(clusters_euclidean_2009$cluster
                                  , clusters_correlation_2009$cluster, similarity="jaccard", method="independence")

# 0.669368

df2 <- data.frame(df_feb_2009["DewP"],df_feb_2009["WDSP"],df_feb_2009["STP"],df_feb_2009["Temp"])

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
  geom_vline(xintercept = 6,linetype=2)+
  labs(subtitle = "withinSS")

set.seed(30)
final <- kmeans(df2, 6, nstart = 10)
print(final)

fviz_cluster(final, data = df2)

df_feb_2009 %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

set.seed(30)
clusters_euclidean_2009 <- Kmeans(df2, centers = 6,iter.max = 400, nstart = 10,
                                  method = "euclidean")
#    180.9329   1405.0142      0.0000    272.3448   2269.5424 596391.2711
clusters_correlation_2009 <- Kmeans(df2, centers = 6,iter.max = 400, nstart = 10,
                                    method = "correlation")

# 2.475625e-11 9.535004e-09 8.144502e-09 1.858646e-09 7.083919e-09 3.254868e-06

#nstart is the random number generator

plotcluster(df2, clusters_euclidean_2009$cluster)

clusplot(df2, clusters_euclidean_2009$cluster,  color=T, shade=F,labels=0,lines=0, main='k-Means Cluster Analysis')

plot(df2,col=clusters_euclidean_2009$cluster)

plot(df2,col=clusters_correlation_2009$cluster)

plot(df2)
jaccard_val = cluster_similarity(clusters_euclidean_2009$cluster
                                 , clusters_correlation_2009$cluster, similarity="jaccard", method="independence")
#0.351395


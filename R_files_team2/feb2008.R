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

feb_2008  <- read.csv(file = "F:/UTA/Courses/Assignments/Data Mining/Project2/feb2008.csv",
                      header =  TRUE, sep = ",",stringsAsFactors = FALSE,
                      na.strings = c("NA","N/A","Unknown","unknown",".P"))
feb_2008[1] <- list(NULL)
feb_2008[3] <- list(NULL)
feb_2008[16] <- list(NULL)

df_feb_2008 <- as.data.frame(feb_2008)

df1<- data.frame(df_feb_2008["Count_STP"],df_feb_2008["Count_WDSP"],df_feb_2008["Count_DewP"])


dist_2008 <- get_dist(df1)
fviz_dist(dist_2008, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


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
  geom_vline(xintercept = 2,linetype=2)+
  labs(subtitle = "withinSS")

set.seed(5)
final <- kmeans(df1, 2, nstart = 4)
print(final)

fviz_cluster(final, data = df1)

df_feb_2008 %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")


c1<- cbind(final$cluster)
clusplot(df1, final$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

set.seed(5)
clusters_euclidean_2008<- Kmeans(df1, centers=2,iter.max = 400, nstart = 4,
                                 method = "euclidean")
# 0.1732012 553.3209877
clusters_correlation_2008 <- Kmeans(df1, centers=2,iter.max = 400, nstart = 4,
                                    method = "correlation")
# 0 0
plotcluster(df1, clusters_euclidean_2008$cluster)
clusplot(df1, clusters_euclidean_2008$cluster,  color=T, shade=F,labels=0,lines=0, main='k-Means Cluster Analysis')
plot(df1,col=clusters_euclidean_2008$cluster)
plot(df1,col=clusters_correlation_2008$cluster)


jaccard_2008 = cluster_similarity(clusters_euclidean_2008$cluster
                                  , clusters_correlation_2008$cluster, similarity="jaccard", method="independence")
#0.710262


df2 <- data.frame(df_feb_2008["DewP"],df_feb_2008["WDSP"],df_feb_2008["STP"],df_feb_2008["Temp"])
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
  geom_vline(xintercept = 4,linetype=2)+
  labs(subtitle = "withinSS")


set.seed(5)
final <- kmeans(df2, 4, nstart = 4)
print(final)

fviz_cluster(final, data = df2)

df_feb_2008 %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

set.seed(5)
clusters_euclidean_2008 <- Kmeans(df2, centers = 4,iter.max = 400, nstart = 4,
                                  method = "euclidean")
#[1]   3512.7013    101.8226 938791.5167   2824.4259

clusters_correlation_2008 <- Kmeans(df2, centers = 4,iter.max = 400, nstart = 4,
                                    method = "correlation")
#[1] 2.554972e-09 3.867764e-07 1.347809e-08 2.068301e-10

#nstart is the random number generator

plotcluster(df2, clusters_euclidean_2008$cluster)

clusplot(df2, clusters_euclidean_2008$cluster,  color=T, shade=F,labels=0,lines=0, main='k-Means Cluster Analysis euclidean')
clusplot(df2, clusters_correlation_2008$cluster,  color=T, shade=F,labels=0,lines=0, main='k-Means Cluster Analysis correlation')

plot(df2,col=clusters_euclidean_2008$cluster)


plot(df2,col=clusters_correlation_2008$cluster)

plot(df2)
jaccard_val = cluster_similarity(clusters_euclidean_2008$cluster
                                 , clusters_correlation_2008$cluster, similarity="jaccard", method="independence")
#0.670425


#seed value = 30 and nstart = 10

df1<- data.frame(df_feb_2008["Count_STP"],df_feb_2008["Count_WDSP"],df_feb_2008["Count_DewP"])


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

df_feb_2008 %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")


c1<- cbind(final$cluster)
clusplot(df1, final$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

set.seed(30)
clusters_euclidean_2008<- Kmeans(df1, centers=2,iter.max = 400, nstart = 10,
                                 method = "euclidean")
# 0.1732012 553.3209877
clusters_correlation_2008 <- Kmeans(df1, centers=2,iter.max = 400, nstart = 10,
                                    method = "correlation")
# 0 0
plotcluster(df1, clusters_euclidean_2008$cluster)
clusplot(df1, clusters_euclidean_2008$cluster,  color=T, shade=F,labels=0,lines=0, main='k-Means Cluster Analysis')
plot(df1,col=clusters_euclidean_2008$cluster)
plot(df1,col=clusters_correlation_2008$cluster)


jaccard_2008 = cluster_similarity(clusters_euclidean_2008$cluster
                                  , clusters_correlation_2008$cluster, similarity="jaccard", method="independence")
#0.719099


df2 <- data.frame(df_feb_2008["DewP"],df_feb_2008["WDSP"],df_feb_2008["STP"],df_feb_2008["Temp"])
dist_2008 <- get_dist(df2)
fviz_dist(dist_2008, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

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
  geom_vline(xintercept = 3,linetype=2)+
  labs(subtitle = "withinSS")


set.seed(30)
final <- kmeans(df2, 3, nstart = 10)
print(final)

fviz_cluster(final, data = df2)

df_feb_2008 %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

set.seed(30)
clusters_euclidean_2008 <- Kmeans(df2, centers = 3,iter.max = 400, nstart = 10,
                                  method = "euclidean")
#[1]    101.8226   2463.6257 938791.5167

clusters_correlation_2008 <- Kmeans(df2, centers = 3,iter.max = 400, nstart = 10,
                                    method = "correlation")
#[1] 1.746641e-09 3.867764e-07 2.554972e-09

#nstart is the random number generator

plotcluster(df2, clusters_euclidean_2008$cluster)

clusplot(df2, clusters_euclidean_2008$cluster,  color=T, shade=F,labels=0,lines=0, main='k-Means Cluster Analysis')

plot(df2,col=clusters_euclidean_2008$cluster)


plot(df2,col=clusters_correlation_2008$cluster)

plot(df2)
jaccard_val = cluster_similarity(clusters_euclidean_2008$cluster
                                 , clusters_correlation_2008$cluster, similarity="jaccard", method="independence")
#0.741483


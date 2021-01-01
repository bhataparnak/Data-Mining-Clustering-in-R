df_feb_2007_2009 <-  read.csv(file = "F:/UTA/Courses/Assignments/Data Mining/Project2/feb2007_feb2009.csv",
                              header =  TRUE, sep = ",",stringsAsFactors = FALSE,
                              na.strings = c("NA","N/A","Unknown","unknown",".P"))
df_feb_2007_2009[1] <- list(NULL)
df_feb_2007_2009[3] <- list(NULL)
df_feb_2007_2009[16] <- list(NULL)

df1_feb_2007_2009 <- data.frame(df_feb_2007_2009["DewP"],df_feb_2007_2009["WDSP"],df_feb_2007_2009["STP"],df_feb_2007_2009["Temp"])
set.seed(5)

wss <- function(k) {
  kmeans(df1_feb_2007_2009, k, nstart = 4 )$tot.withinss
}

# Compute and plot wss for k = 2 to k = 8
k.values <- 2:8

wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(5)
fviz_nbclust(df1_feb_2007_2009, kmeans, method = "wss")+
  geom_vline(xintercept = 6,linetype=2)+
  labs(subtitle = "withinSS")


set.seed(5)
final <- kmeans(df1_feb_2007_2009, 6, nstart = 4)
print(final)

fviz_cluster(final, data = df1_feb_2007_2009)



set.seed(5)
clusters_euclidean_2007_2009 <- Kmeans(df1_feb_2007_2009, centers = 6,iter.max = 400, nstart = 4,
                                       method = "euclidean")

clusters_correlation_2007_2009 <- Kmeans(df1_feb_2007_2009, centers = 6,iter.max = 400, nstart = 4,
                                         method = "correlation")

df_feb_2009_2007 <-  read.csv(file = "F:/UTA/Courses/Assignments/Data Mining/Project2/feb2009_feb2007.csv",
                              header =  TRUE, sep = ",",stringsAsFactors = FALSE,
                              na.strings = c("NA","N/A","Unknown","unknown",".P"))
df_feb_2009_2007[1] <- list(NULL)
df_feb_2009_2007[3] <- list(NULL)
df_feb_2009_2007[16] <- list(NULL)

df1_feb_2009_2007 <- data.frame(df_feb_2009_2007["DewP"],df_feb_2009_2007["WDSP"],df_feb_2009_2007["STP"],df_feb_2009_2007["Temp"])
set.seed(5)
clusters_euclidean_2009_2007 <- Kmeans(df1_feb_2009_2007, centers = 6,iter.max = 400, nstart = 1,
                                       method = "euclidean")

clusters_correlation_2009_2007 <- Kmeans(df1_feb_2009_2007, centers = 6,iter.max = 400, nstart = 1,
                                         method = "correlation")

jaccard_val_euclidean = cluster_similarity(clusters_euclidean_2007_2009$cluster
                                           , clusters_euclidean_2009_2007$cluster, similarity="jaccard", method="independence")
# 0.227494

jaccard_val_correlation = cluster_similarity(clusters_correlation_2007_2009$cluster
                                             , clusters_correlation_2009_2007$cluster, similarity="jaccard", method="independence")
# 0.1716489

DewP <- as.vector(df1_feb_2007_2009["DewP"][,1])
plot(x= clusters_euclidean_2007_2009$cluster,y= DewP, ylim = c(1,100))

DewP <- as.vector(df1_feb_2007_2009["DewP"][,1])
plot(x= clusters_correlation_2007_2009$cluster,y= DewP, ylim = range(1,100))

DewP <- as.vector(df1_feb_2009_2007["DewP"][,1])
plot(x= clusters_euclidean_2009_2007$cluster,y= DewP, ylim = c(1,100))

DewP <- as.vector(df1_feb_2009_2007["DewP"][,1])
plot(x= clusters_correlation_2009_2007$cluster,y= DewP, ylim = range(1,100))

WDSP <- as.vector(df1_feb_2007_2009["WDSP"][,1])
plot(x= clusters_euclidean_2007_2009$cluster,y= WDSP, ylim = c(1,50))

WDSP <- as.vector(df1_feb_2007_2009["WDSP"][,1])
plot(x= clusters_correlation_2007_2009$cluster,y= WDSP, ylim = c(1,50))

WDSP <- as.vector(df1_feb_2009_2007["WDSP"][,1])
plot(x= clusters_euclidean_2009_2007$cluster,y= WDSP, ylim = c(1,50))

WDSP <- as.vector(df1_feb_2009_2007["WDSP"][,1])
plot(x= clusters_correlation_2009_2007$cluster,y= WDSP, ylim = c(1,50))

Temp <- as.vector(df1_feb_2007_2009["Temp"][,1])
plot(x= clusters_euclidean_2007_2009$cluster,y= Temp, ylim = c(1,100))

Temp <- as.vector(df1_feb_2007_2009["Temp"][,1])
plot(x= clusters_correlation_2007_2009$cluster,y= Temp, ylim = c(1,100))

Temp <- as.vector(df1_feb_2009_2007["Temp"][,1])
plot(x= clusters_euclidean_2009_2007$cluster,y= Temp, ylim = c(1,100))

Temp <- as.vector(df1_feb_2009_2007["Temp"][,1])
plot(x= clusters_correlation_2009_2007$cluster,y= Temp, ylim = c(1,100))

STP <- as.vector(df1_feb_2007_2009["STP"][,1])
plot(x= clusters_euclidean_2007_2009$cluster,y= STP, ylim = c(1,1500))

STP <- as.vector(df1_feb_2007_2009["STP"][,1])
plot(x= clusters_correlation_2007_2009$cluster,y= STP, ylim = c(1,1500))

STP <- as.vector(df1_feb_2009_2007["STP"][,1])
plot(x= clusters_euclidean_2009_2007$cluster,y= STP, ylim = c(1,1500))

STP <- as.vector(df1_feb_2009_2007["STP"][,1])
plot(x= clusters_correlation_2009_2007$cluster,y= STP, ylim = c(1,1500))


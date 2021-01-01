df_feb_2007_2008_2009 <-  read.csv(file = "F:/UTA/Courses/Assignments/Data Mining/Project2/feb2007_feb2008_feb2009.csv",
                              header =  TRUE, sep = ",",stringsAsFactors = FALSE,
                              na.strings = c("NA","N/A","Unknown","unknown",".P"))
df_feb_2007_2008_2009[1] <- list(NULL)
df_feb_2007_2008_2009[3] <- list(NULL)
df_feb_2007_2008_2009[16] <- list(NULL)

df1_feb_2007_2008_2009 <- data.frame(df_feb_2007_2008_2009["DewP"],df_feb_2007_2008_2009["WDSP"],df_feb_2007_2008_2009["STP"],df_feb_2007_2008_2009["Temp"])
set.seed(5)

wss <- function(k) {
  kmeans(df1_feb_2007_2008_2009, k, nstart = 4 )$tot.withinss
}

# Compute and plot wss for k = 2 to k = 8
k.values <- 2:8

wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(5)
fviz_nbclust(df1_feb_2007_2008_2009, kmeans, method = "wss")+
  geom_vline(xintercept = 5,linetype=2)+
  labs(subtitle = "withinSS")


set.seed(5)
final <- kmeans(df1_feb_2007_2008_2009, 5, nstart = 4)
print(final)
fviz_cluster(final, data = df1_feb_2007_2008_2009)



set.seed(5)
clusters_euclidean_2007_2008_2009 <- Kmeans(df1_feb_2007_2008_2009, centers = 5,iter.max = 400, nstart = 4,
                                       method = "euclidean")

clusters_correlation_2007_2008_2009 <- Kmeans(df1_feb_2007_2008_2009, centers = 5,iter.max = 400, nstart = 4,
                                         method = "correlation")


jaccard_val_correlation = cluster_similarity(clusters_euclidean_2007_2008_2009$cluster
                                             , clusters_correlation_2007_2008_2009$cluster, similarity="jaccard", method="independence")
# 0.4437509
clusplot(df1_feb_2007_2008_2009, clusters_euclidean_2007_2008_2009$cluster,  color=T, shade=F,labels=0,lines=0, main='k-Means Cluster Analysis euclidean')
clusplot(df1_feb_2007_2008_2009, clusters_correlation_2007_2008_2009$cluster,  color=T, shade=F,labels=0,lines=0, main='k-Means Cluster Analysis correlation')
plot(df1_feb_2007_2008_2009,col=clusters_euclidean_2007_2008_2009$cluster)

plot(df1_feb_2007_2008_2009,col=clusters_correlation_2007_2008_2009$cluster)

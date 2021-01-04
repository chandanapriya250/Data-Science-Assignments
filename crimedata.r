#-----------------------K-means clustering--------------------------
library(plyr)

crime <- read.csv(file.choose())
str(crime)

normalized_data<-scale(crime[,2:5])

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))      # Determine number of clusters by scree-plot 
for (i in 2:5) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:5, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

fit <- kmeans(normalized_data, 4) # 4 cluster solution
final2<- data.frame(crime, fit$cluster) # append cluster membership
final2

aggregate(crime[,2:5], by=list(fit$cluster), FUN=mean)
table(fit$cluster)


library(animation)
 nm <- (normalized_data[,1:1])
 km <- kmeans(normalized_data,4) #kmeans clustering
str(km)
km$cluster
km1 <- kmeans.ani(normalized_data, 4)
str(km1)
 km1$centers
 
 #Insights
 #The Cluster with maximum # of murders seems to be a major threat to Live.
 #The 2nd most cluster with a maximum on Assault, Rape and other factors takes the 2nd rank
 #on the most dangerous state to live.
# The Next two clusters would take those rankings based on the Crime rate categories
# on rape, murder, assault and urbanpop metrics.
 
 #-------------------------------Hierarchial clustering---------------------
 
 library(rmarkdown)
 library(readxl)
 library(ggplot2)
 library(cluster)
 library(fpc)
 library(dendextend)
 
 View(crime)
 
 # Normalizing continuous columns to bring them under same scale
 normalized_data<-scale(crime[,2:5]) #excluding the ID from spreadsheet
 d <- dist(normalized_data, method = "euclidean") # distance matrix
 fit <- hclust(d, method="complete")
 
 fit <- as.dendrogram(fit)
 cd = color_branches(fit,k=4)
 plot(cd) # display dendrogram
 plot(fit, hang=-1)
 
 # rect.hclust(fit, k=2, border="red")
 groups <- cutree(fit, k=4) # cut tree into 4 clusters
 
 table(groups)
 
 Crime_Rate_Categories<-as.matrix(groups) # groups or cluster numbers
 final <- data.frame(crime, Crime_Rate_Categories)
 
 final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
 
 View(final1)
 
 aggregate(crime[,-1],by=list(final$Crime_Rate_Categories),mean)
 
 #Insights
 #Cluster 1 has the maximum number of Murder which to me looks like it is more vulnerable
 #with other Assault on second top in the list.
# Cluster 2 has the maximum number of Rape,Assault and murder Pop and stands second in Murder.
 #Cluster 3 is a little vulnerable in terms of the Murder(Less). However it still stands 2nd in
 #UrbanPop Crime
 #Cluster 4 has got the less vulnerable to all the Crime categories.
 
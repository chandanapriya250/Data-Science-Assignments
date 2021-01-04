
#-----------------------------K-means clustering----------------------------------
library(plyr)

mydata <- read.csv(file.choose())
str(mydata)
normalized_data<-scale(mydata[,2:12])

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))      # Determine number of clusters by scree-plot 
for (i in 2:12) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

fit <- kmeans(normalized_data, 3) # 3 cluster solution
final2<- data.frame(mydata, fit$cluster) # append cluster membership
# final2
aggregate(mydata[,2:12], by=list(fit$cluster), FUN=mean)

table(fit$cluster)

#Insights:
#The Cluster with 4.8 % data seems to have more preminum customers in terms of their balance # miles, qualifying for topflight status ,miles earned using frequent flier credit card , Bonus # miles earned, Bonus transactions and also the Award.
#The Cluster with 32.1 % Customers seems to be the next set of Premium customers.
#The CLuster with more than 60 % Customer comes under average category.

#----------------------Hierarchial clustering ----------------------

library(rmarkdown)
library(readxl)
#library(rJava)
library(ggplot2)
library(cluster)
library(fpc)
library(dendextend)

View(mydata)

# Normalizing continuous columns to bring them under same scale
normalized_data<-scale(mydata[,2:12]) #excluding the ID from spreadsheet

d <- dist(normalized_data, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2")

fit <- as.dendrogram(fit)
cd = color_branches(fit,k=3)

plot(cd) # display dendrogram

plot(fit, hang=-1)

# rect.hclust(fit, k=2, border="red")

groups <- cutree(fit, k=3) # cut tree into 5 clusters

table(groups)

membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(mydata, membership)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]

final1

Fin <- aggregate(mydata[,-1],by=list(final$membership),mean)

Fin

#Insights:
#On analyzing the data set with 3 clusters, I conclude that Cluster 3 has the premium membership
#having more balance miles, Maximum number of Non-flight Trans in the last , Maximum number of
#flight miles in the last 12 months, Maximum number of flight transactions in the last 12 months and also
#awarded.
#Cluster 2 - This customer forms the second best priority customer with 2nd highest balance hours, First top
#qualifying miles,20000+ miles earned using frequent flier program and received Awards.
#The Least priority of customers were from Cluster 1.
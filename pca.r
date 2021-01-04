library(cluster)
library(factoextra)
pca<-read.csv(file.choose()) ## use read.csv for csv files
View(pca)

#help(princomp) ## to understand the api for princomp
## the first column in pca has Types of Wine
View(pca[-1]) 
# pca[-1] -> Considering only numerical values for applying PCA
data <- pca[,-1]
attach(data)
cor(data)
# cor = TRUE use correlation matrix for getting PCA scores
pcaObj<-princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)

str(pcaObj)
## princomp(pca, cor = TRUE) not_same_as prcomp(pca, scale=TRUE); similar, but different
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)
plot(pcaObj) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

biplot(pcaObj)
# Showing the increase of variance with considering principal components
# Which helps in choosing number of principal components
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")
#pcaObj$loadings

pcaObj$scores[,1:3] # Top 3 PCA Scores which represents the whole data

# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata
pca<-cbind(pca,pcaObj$scores[,1:3])
View(pca)

############################## Hierarchial Clustering ##############################
# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-pca[,8:10]

# Normalizing the data 
norm_clus<-scale(clus_data) # Scale function is used to normalize data
dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
# here I am considering Euclidean distance

# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete") # method here is complete linkage

plot(fit1) # Displaying Dendrogram

rect.hclust(fit1, k=7, border="red")

groups<-cutree(fit1,7) # Cutting the dendrogram for 7 clusters

membership_1<-as.matrix(groups) # cluster numbering 

View(membership_1)

final1<-cbind(membership_1,pca) # binding column wise with orginal data
View(final1)
View(aggregate(final1[,-c(2,16:18)],by=list(membership_1),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final1,file="wine_cluster.csv",row.names = F,col.names = F)

getwd()
########################### K-Means Clustering ################################
library(plyr)
pca <- read.csv(file.choose())
str(pca)

View(pca)

normalized_data<-scale(pca[,8:10])

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))     # Determine number of clusters by scree-plot 
for (i in 1:7) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

fit <- eclust(normalized_data, "kmeans", k = 7, nstart = 25, graph = FALSE) # 7 cluster solution
fviz_cluster(fit, geom = "point", frame.type = "norm")

final2<- data.frame(fit$cluster,pca) # append cluster membership
View(final2)
aggregate(pca[,8:10], by=list(fit$cluster), FUN=mean)
table(fit$cluster)

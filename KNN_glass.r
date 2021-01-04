
library(caret)
library(pROC)
library(mlbench)
library(lattice)
library(gmodels)
library(class)

glass <- read.csv(file.choose()) #Loading data set
View(glass)

 #table on different types of glasses 
table(glass$Type)
glass$type = as.factor(glass$Type)
str(glass)
# table or proportation of enteries in the datasets. What % of glass of Type 1 and what % of glass of Type 2
round(prop.table(table(glass$Type))*100,1)
summary(glass[1:4])
#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))
#Apply the normalization function to glass dataset
glass_n<- as.data.frame(lapply(glass[1:9], norm))
View(glass_n)
summary(glass_n[1:4])

# glass_n <- cbind(glass$Type,glass_n[1:9])
View(glass_n)

#create training and test datasets
set.seed(123)
ind <- sample(2, nrow(glass_n), replace = TRUE, prob = c(0.7,0.3))
glass_train <- glass_n[ind==1,]
glass_test <-  glass_n[ind==2,]


#Get labels for training and test datasets
set.seed(123)
ind1 <- sample(2, nrow(glass), replace = TRUE, prob = c(0.7,0.3))
glass_train_labels <- glass[ind1==1,10]
glass_test_labels <-  glass[ind1==2,10]


# Build a KNN model on taining dataset

# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset

glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=3)
table(glass_test_pred,glass_test_labels)

mean(glass_test_pred==glass_test_labels) #68.42
# Evaluating Model Performance ----

# load the gmodel library

CrossTable(x=glass_test_labels,y=glass_test_pred,prop.chisq = FALSE) 

# Accuracy is 65.07 %. 

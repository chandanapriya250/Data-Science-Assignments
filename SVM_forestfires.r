library(kernlab)
library(caret)
library(plyr)
library(ggplot2)
library(psych)
library(e1071)
# Read the data
FF <- read.csv(file.choose())
View(FF)
class(FF)
str(FF)

# The area value has lots of zeros

hist(FF$area)
rug(FF$area)

# Transform the Area value to Y 

FF1 <- mutate(FF, y = log(area + 1))  # default is to the base e, y is lower case
hist(FF1$y)

summary(FF) # Confirms on the different scale and demands normalizing the data.

# Prediction of Forest fires requires only prediction from 
# temperature, rain, relative humidity and wind speed

# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
FF$temp = normalize(FF$temp)
FF$RH   = normalize(FF$RH)
FF$wind = normalize(FF$wind)
FF$rain = normalize(FF$rain)
# We need to tweak this as a classification problem.lets base out the Size using this criteria :

attach(FF)
# Data Partition 
set.seed(123)
ind <- sample(2, nrow(FF), replace = TRUE, prob = c(0.7,0.3))
FF_train <- FF[ind==1,]
View(FF_train)
str(FF_train)

FF_test  <- FF[ind==2,]
View(FF_test)
str(FF_test)


# to train model
# e1071 package from LIBSVM library
# SVMlight algorithm klar package 

# kvsm() function uses gaussian RBF kernel 

# Building model 
FF_train$size_category <- as.factor(FF_train$size_category)
FF_test$size_category <- as.factor(FF_test$size_category)



model1<-svm(size_category~temp+rain+wind+RH, 
            data= FF_train)

model1

Area_pred <- predict(model1, FF_test)

table(Area_pred,FF_test$size_category)

agreement <- Area_pred == FF_test$size_category
table(agreement)
prop.table(table(agreement))

# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(size_category~temp+rain+wind+RH, 
                  data= FF_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=FF_test)
mean(pred_rfdot==FF_test$size_category) # 68.41

# kernel = vanilladot
model_vanilla<-ksvm(size_category~temp+rain+wind+RH, 
                    data= FF_train,kernel = "vanilladot")

pred_vanilla<-predict(model_vanilla,newdata=FF_test)
mean(pred_vanilla==FF_test$size_category) # 67.80

# kernal = besseldot
model_besseldot<-ksvm(size_category~temp+rain+wind+RH, 
                      data= FF_train,kernel = "besseldot")

pred_bessel<-predict(model_besseldot,newdata=FF_test)
mean(pred_bessel==FF_test$size_category) # 67.80

# kernel = polydot

model_poly<-ksvm(size_category~temp+rain+wind+RH, 
                 data= FF_train,kernel = "polydot")

pred_poly<-predict(model_poly,newdata = FF_test)
mean(pred_poly==FF_test$size_category) # 67.80


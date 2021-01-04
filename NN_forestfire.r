library(neuralnet)# regression
library(nnet) # classification 
library(NeuralNetTools)
library(plyr)
# Read the data
forestfire <- read.csv(file.choose())
View(forestfire)
class(forestfire)


forestfire$size_category <- as.numeric(revalue(forestfire$size_category,
                                                    c("small"="0", "large"="1")))
str(forestfire)

forestfire <- as.data.frame(forestfire)
hist(forestfire$area)
rug(forestfire$area)

# Transform the Area value to Y 

forestfire1 <- mutate(forestfire, y = log(area + 1))  # default is to the base e, y is lower case
hist(forestfire1$y)
summary(forestfire)

# Prediction of Forest fires requires only prediction from 
# temperature, rain, relative humidity and wind speed

# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forestfire$temp = normalize(forestfire$temp)
forestfire$RH   = normalize(forestfire$RH)
forestfire$wind = normalize(forestfire$wind)
forestfire$rain = normalize(forestfire$rain)
# We need to tweak this as a classification problem.lets base out the Size using this criteria :

attach(forestfire)
# Data Partition 
set.seed(123)
ind <- sample(2, nrow(forestfire), replace = TRUE, prob = c(0.7,0.3))
forestfire_train <- forestfire[ind==1,]
forestfire_test  <- forestfire[ind==2,]
View(forestfire_test)

forestfire_model <- neuralnet(size_category~temp+rain+wind+RH,data = forestfire_train)
str(forestfire_model)

plot(forestfire_model, rep = "best")

summary(forestfire_model)

 par(mar = numeric(4), family = 'serif')
plotnet(forestfire_model, alpha = 0.6)


# Evaluating model performance

set.seed(12323)
model_results <- compute(forestfire_model,forestfire_test[7:11])
predicted_sizecategory <- model_results$net.result

# Predicted profit Vs Actual profit of test data.
cor(predicted_sizecategory,forestfire_test$size_category)

# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on profit
str_max <- max(forestfire$size_category)
str_min <- min(forestfire$size_category)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

Actualsizecategory_pred <- unnormalize(predicted_sizecategory,str_min,str_max)
head(Actualsizecategory_pred)

# Improve the model performance :
set.seed(12345)
forestfire_model2 <- neuralnet(size_category~temp+rain+wind+RH,data = forestfire_train,
                             hidden = 2)
plot(forestfire_model2 ,rep = "best")

summary(forestfire_model2)
model_results2<-compute(forestfire_model2,forestfire_test[7:11])
predicted_forestfire2<-model_results2$net.result
cor(predicted_forestfire2,forestfire_test$size_category)
plot(predicted_forestfire2,forestfire_test$size_category)

par(mar = numeric(4), family = 'serif')
plotnet(forestfire_model2, alpha = 0.6)

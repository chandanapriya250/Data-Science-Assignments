library(randomForest)
library(MASS)
library(caret)
# USe the set.seed function so that we get same results each time 
set.seed(123)


CompanyData <- read.csv(file.choose())
View(CompanyData)
hist(CompanyData$Sales, main = "Sales of Companydata",xlim = c(0,20),
     breaks=c(seq(10,20,30)), col = c("blue","red", "green","violet"))

highsales = ifelse(CompanyData$Sales<9, "No", "Yes")  # if greater than 8 then high sales else Low
CD = data.frame(CompanyData[2:11], highsales)
str(CD)
View(CD)
CD$ShelveLoc <- as.factor(CD$ShelveLoc)
CD$Urban <- as.factor(CD$Urban)
CD$US <- as.factor(CD$US)
CD$highsales <- as.factor(CD$highsales)
table(CD$highsales)

# Data Partition
set.seed(123)
ind <- sample(2, nrow(CD), replace = TRUE, prob = c(0.7,0.3))
train <- CD[ind==1,]
View(train)
test  <- CD[ind==2,]
View(test)
set.seed(213)
train$ShelveLoc <- as.factor(train$ShelveLoc)
train$Urban <- as.factor(train$Urban)
train$US <- as.factor(train$US)
train$highsales <- as.factor(train$highsales)
rf <- randomForest(highsales~., data=train)
rf  # Description of the random forest with no of trees, mtry = no of variables for splitting

# each tree node.
# Out of bag estimate of error rate is 16.84 % in Random Forest Model.
attributes(rf)
# Prediction and Confusion Matrix - Training data 
pred1 <- predict(rf, train)
head(pred1)

head(train$highsales)
# looks like the first six predicted value and original value matches.

confusionMatrix(pred1, train$highsales)   # 100 % accuracy on training data 

# more than 95% Confidence Interval. 
# Sensitivity for Yes and No is 100 % 

# Prediction with test data - Test Data 
test$ShelveLoc <- as.factor(test$ShelveLoc)
test$Urban <- as.factor(test$Urban)
test$US <- as.factor(test$US)
test$highsales <- as.factor(test$highsales)
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$highsales) # 82.61 % accuracy on test data 
# Error Rate in Random Forest Model :
plot(rf)
# Tune Random Forest Model mtry 
tune <- tuneRF(train[,-11], train[,11], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)

rf1 <- randomForest(highsales~., data=train, ntree = 300, mtry = 3, importance = TRUE,
                    proximity = TRUE)
rf1

pred1 <- predict(rf1, train)
confusionMatrix(pred1, train$highsales)  # 100 % accuracy on training data
# Around 98% Confidence Interval. 
# Sensitivity for Yes and No is 100 % 

# test data prediction using the Tuned RF1 model
pred2 <- predict(rf1, test)
confusionMatrix(pred2, test$highsales) # 84.35 % accuracy on test data 

# Confidence Interval is around 90 % 


# no of nodes of trees

hist(treesize(rf1), main = "No of Nodes for the trees", col = "green")
# Majority of the trees has an average number of 45 to 50 nodes. 

# Variable Importance :

varImpPlot(rf1)

# Mean Decrease Accuracy graph shows that how worst the model performs without each variable.
# say ShelveLoc is the most important variable for prediction.on looking at population,it has no value.

# MeanDecrease gini graph shows how much by average the gini decreases if one of those nodes were 
# removed. Price is very important and Urban is not that important.

varImpPlot(rf1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")

# Quantitative values 
importance(rf1)
varUsed(rf)   # which predictor variables are actually used in the random forest.

# Partial Dependence Plot 
partialPlot(rf1, train, Price, "Yes")

# On that graph, i see that if the price is 100 or greater, than they are not buying those computers.

# Extract single tree from the forest :

getTree(rf, 1, labelVar = TRUE)

# Multi Dimension scaling plot of proximity Matrix
MDSplot(rf1, CD$highsales)


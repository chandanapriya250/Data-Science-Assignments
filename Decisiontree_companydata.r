library(rmarkdown)
library(C50)
library(tree)
library(gmodels)
library(caret)
library(party)

CompanyData <- read.csv(file.choose())
# Splitting data into training and testing.
# splitting the data based on Sales
hist(CompanyData$Sales)
View(CompanyData)
High = ifelse(CompanyData$Sales<10, "No", "Yes")
CD = data.frame(CompanyData, High)
View(CD)
CD_train <- CD[1:200,]

View(CD_train)
CD_test <- CD[201:400,]
 View(CD_test)
CD_train$High <- as.factor(CD_train$High)
CD_train$ShelveLoc <- as.factor(CD_train$ShelveLoc)
CD_train$Urban<- as.factor(CD_train$Urban)
CD_train$US <- as.factor(CD_train$US)
#Using Party Function 
 op_tree = ctree(High ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc
                 + Age + Education + Urban + US, data = CD_train)
 
summary(op_tree)

plot(op_tree)


# On looking into the Above tree, i see that if the Location of the Shelv is good,
# then there is a probability of 60% chance that the customer will buy.
# With ShelveLoc having a Bad or Medium and Price <= 87, the probability of High sales 
# could be 60%.
# If ShelveLoc is Bad or Medium, With Price >= 87 and Advertising less then <= 7 then there
# is a zero percent chance of high sales.
# If ShelveLoc is Bad or Medium, With Price >= 87 and Advertising less then > 7 then there
# is a 20 % percent chance of high sales.
CD_test$High <- as.factor(CD_test$High)
CD_test$ShelveLoc <- as.factor(CD_test$ShelveLoc)
CD_test$Urban<- as.factor(CD_test$Urban)
CD_test$US <- as.factor(CD_test$US)
pred_tree <- as.data.frame(predict(op_tree,newdata=CD_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=CD_test)

CrossTable(CD_test$High,pred_test_df)
confusionMatrix(CD_test$High,pred_test_df)
##### Using tree function 
CD$High <- as.factor(CD$High)
CD$ShelveLoc <- as.factor(CD$ShelveLoc)
CD$Urban <- as.factor(CD$Urban)
CD$US <- as.factor(CD$US)
cd_tree_org <- tree(High~.-Sales,data=CD)
summary(cd_tree_org)
plot(cd_tree_org)
text(cd_tree_org,pretty = 0)

# Using the training data

##### Using tree function 
cd_tree <- tree(High~.-Sales,data=CD_train)
summary(cd_tree)
plot(cd_tree)
text(cd_tree,pretty = 0)
### Evaluate the Model

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(cd_tree,newdata=CD_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(cd_tree,newdata=CD_test)


pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]

pred_tree$final <- as.factor(pred_tree$final)
summary(pred_tree$final)

summary(CD_test$High)
mean(pred_tree$final==CD$High) # Accuracy = 77.25

CrossTable(CD_test$High,pred_tree$final)
confusionMatrix(CD_test$High,pred_tree$final)



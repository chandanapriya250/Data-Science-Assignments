library(C50)
library(tree)
library(gmodels)
library(knitr)
library(png)
library(caret)
library(party)

FraudCheck <- read.csv(file.choose())
View(FraudCheck)
# Splitting data into training and testing.
# splitting the data based on Sales
hist(FraudCheck$Taxable.Income)
Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
FC = data.frame(FraudCheck,Risky_Good)
View(FC)

FC_train <- FC[1:300,]

View(FC_train)
FC_test <- FC[301:600,]

 View(FC_test)

###Using Party Function 

png(file = "decision_tree.png")
FC$Undergrad <- as.factor(FC$Undergrad)
FC$Marital.Status <- as.factor(FC$Marital.Status)
FC$Urban <- as.factor(FC$Urban)
FC$Risky_Good <- as.factor(FC$Risky_Good)
opall_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban, data = FC)
summary(opall_tree)
plot(opall_tree)
# From the above tree, It looks like the data has 20 % of Risky patients and 80 % good patients


# using the training Data 

png(file = "decision_tree.png")
FC_train$Undergrad <- as.factor(FC_train$Undergrad)
FC_train$Marital.Status <- as.factor(FC_train$Marital.Status)
FC_train$Urban <- as.factor(FC_train$Urban)
FC_train$Risky_Good <- as.factor(FC_train$Risky_Good)
op_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)
summary(op_tree)

plot(op_tree)
FC_test$Undergrad <- as.factor(FC_test$Undergrad)
FC_test$Marital.Status <- as.factor(FC_test$Marital.Status)
FC_test$Urban <- as.factor(FC_test$Urban)
FC_test$Risky_Good <- as.factor(FC_test$Risky_Good)
pred_tree <- as.data.frame(predict(op_tree,newdata=FC_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=FC_test)


mean(pred_test_df==FC_test$Risky_Good) # Accuracy = 82 %
CrossTable(FC_test$Risky_Good,pred_test_df)
confusionMatrix(FC_test$Risky_Good,pred_test_df)


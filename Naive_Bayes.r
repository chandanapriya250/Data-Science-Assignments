
library(ggplot2)
library(caret)
library(psych)
library(e1071)
library(tidyverse)
train_sal <- read.csv(file.choose())
str(train_sal)
View(train_sal)
train_sal$educationno <- as.factor(train_sal$educationno)

class(train_sal)# Data(Test)
test_sal <- read.csv(file.choose())
str(test_sal)
View(test_sal)
test_sal$educationno <- as.factor(test_sal$educationno)
class(test_sal)

#Visualization 
# ggplot 
ggplot(data=train_sal,aes(x=Salary, y = age, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")


ggplot(data=train_sal,aes(x=Salary, y = capitalgain, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=train_sal,aes(x=Salary, y = capitalloss, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=train_sal,aes(x=Salary, y = hoursperweek, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")



#Density Plot 

ggplot(data=train_sal,aes(x = age, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("Age - Density Plot")

ggplot(data=train_sal,aes(x = workclass, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("Workclass Density Plot")

ggplot(data=train_sal,aes(x = education, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("education Density Plot")

ggplot(data=train_sal,aes(x = educationno, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("educationno Density Plot")
ggplot(data=train_sal,aes(x = maritalstatus, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("maritalstatus Density Plot")

ggplot(data=train_sal,aes(x = occupation, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("occupation Density Plot")

ggplot(data=train_sal,aes(x = sex, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("sex Density Plot")
ggplot(data=train_sal,aes(x = relationship, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("Relationship Density Plot")

ggplot(data=train_sal,aes(x = race, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("Race Density Plot")

ggplot(data=train_sal,aes(x = capitalgain, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("Capitalgain Density Plot")

ggplot(data=train_sal,aes(x = capitalloss, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("Capitalloss Density Plot")

ggplot(data=train_sal,aes(x = hoursperweek, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("Hoursperweek Density Plot")

ggplot(data=train_sal,aes(x = native, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("native Density Plot")

# Naive Bayes Model 
library(naivebayes)
library(psych)
train_sal$Salary <- as.factor(train_sal$Salary)
Model <- naiveBayes(Salary ~ ., data = train_sal)
Model
predict(Model,train_sal[1:15,],type = "raw")
test_sal$Salary <- as.factor(test_sal$Salary)
Model_pred <- predict(Model,test_sal)

mean(Model_pred==test_sal$Salary)

confusionMatrix(Model_pred,test_sal$Salary)


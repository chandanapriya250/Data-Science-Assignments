library(kernlab)
library(caret)
library(plyr)
library(ggplot2)
library(psych)
library(e1071)
# Data(Train)
train_sal <- read.csv(file.choose())
str(train_sal)
View(train_sal)
train_sal$educationno <- as.factor(train_sal$educationno)
class(train_sal)
# Data(Test)
test_sal <- read.csv(file.choose())
str(test_sal)

View(test_sal)
test_sal$educationno <- as.factor(test_sal$educationno)
class(test_sal)
test_sal$workclass <- as.factor(test_sal$workclass)
test_sal$Salary <- as.factor(test_sal$Salary)
test_sal$education <- as.factor(test_sal$education)
test_sal$maritalstatus <- as.factor(test_sal$maritalstatus)
test_sal$occupation <- as.factor(test_sal$occupation)
test_sal$relationship <- as.factor(test_sal$relationship)
test_sal$race <- as.factor(test_sal$race)
test_sal$sex <- as.factor(test_sal$sex)
test_sal$native <- as.factor(test_sal$native)


#Visualization 
# Plot and ggplot 
ggplot(data=train_sal,aes(x=Salary, y = age, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

train_sal$workclass <- as.factor(train_sal$workclass)
train_sal$Salary <- as.factor(train_sal$Salary)
train_sal$education <- as.factor(train_sal$education)
train_sal$maritalstatus <- as.factor(train_sal$maritalstatus)
train_sal$occupation <- as.factor(train_sal$occupation)
train_sal$relationship <- as.factor(train_sal$relationship)
train_sal$race <- as.factor(train_sal$race)
train_sal$sex <- as.factor(train_sal$sex)
train_sal$native <- as.factor(train_sal$native)



plot(train_sal$workclass,train_sal$Salary)
plot(train_sal$education,train_sal$Salary)
plot(train_sal$educationno,train_sal$Salary)
plot(train_sal$maritalstatus,train_sal$Salary)
plot(train_sal$occupation,train_sal$Salary)
plot(train_sal$relationship,train_sal$Salary)
plot(train_sal$race,train_sal$Salary)
plot(train_sal$sex,train_sal$Salary)
ggplot(data=train_sal,aes(x=Salary, y = capitalgain, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=train_sal,aes(x=Salary, y = capitalloss, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=train_sal,aes(x=Salary, y = hoursperweek, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

plot(train_sal$native,train_sal$Salary)

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

# Building model 


model1 <- svm(Salary~., data= train_sal)

model1


pred <- predict(model1, test_sal)

table(pred,test_sal$Salary)

agreement <- pred == test_sal$Salary
table(agreement)

prop.table(table(agreement))
# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(train_sal$Salary~., 
                  data= train_sal,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=test_sal)
mean(pred_rfdot==test_sal$Salary) # 85.19

# kernel = vanilladot
model_vanilla<-ksvm(train_sal$Salary~., 
                    data= train_sal,kernel = "vanilladot")

pred_vanilla<-predict(model_vanilla,newdata=test_sal)
mean(pred_vanilla==test_sal$Salary) # 84.64


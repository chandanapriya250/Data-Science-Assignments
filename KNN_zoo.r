library(caret)
library(pROC)
library(mlbench)
library(lattice)
zoo <- read.csv(file.choose())
str(zoo)
View(zoo)
zoo1 <- zoo[,2:18]
str(View(zoo1))
zoo1$hair <- as.factor(zoo1$hair)
zoo1$feathers <- as.factor(zoo1$feathers)
zoo1$eggs <- as.factor(zoo1$eggs)
zoo1$milk <- as.factor(zoo1$milk)
zoo1$airborne <- as.factor(zoo1$airborne)
zoo1$aquatic <- as.factor(zoo1$aquatic)
zoo1$predator <- as.factor(zoo1$predator)
zoo1$toothed <- as.factor(zoo1$toothed)
zoo1$backbone <- as.factor(zoo1$backbone)
zoo1$breathes <- as.factor(zoo1$breathes)
zoo1$venomous <- as.factor(zoo1$venomous)
zoo1$fins <- as.factor(zoo1$fins)
zoo1$legs <- as.factor(zoo1$legs)
zoo1$tail <- as.factor(zoo1$tail)
zoo1$domestic <- as.factor(zoo1$domestic)
zoo1$catsize <- as.factor(zoo1$catsize)
zoo1$type <- as.factor(zoo1$type)



# Data partition
set.seed(123)
ind <- sample(2,nrow(zoo1), replace = T, prob = c(0.7,0.3))
train <- zoo1[ind==1,]
test <- zoo1[ind==2,]

# KNN Model 

trcontrol <- trainControl(method = "repeatedcv", number = 10,repeats = 3
                          # classprobs are needed when u want to select ROC for optimal K Value
)
set.seed(222)
fit <- train(type ~., data = train, method = 'knn', tuneLength = 20,
             trControl = trcontrol, preProc = c("center","scale"))

# default metric is accuracy but if u want to use ROC, then mention the same
# Model Performance :
fit # the optimum value for k should be 7

plot(fit)
varImp(fit)
# 89.66 % is accuracy
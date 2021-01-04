library(data.table)
library(readr)
library(psych)
startup_50 <- fread("C://Users//Dell//Desktop//ExcelR//Assignments//Multi_linear_regression//50_Startups.csv")
summary(startup_50)

# Variance
var(startup_50$`R&D Spend`)
var(startup_50$Administration)
var(startup_50$`Marketing Spend`)
var(startup_50$Profit)

# Standard Deviation
sd(startup_50$`R&D Spend`)
sd(startup_50$Administration)
sd(startup_50$`Marketing Spend`)
sd(startup_50$Profit)

#standardization
startup_50$st_RDSpend <- scale(startup_50$`R&D Spend`) 
sd(startup_50$st_RDSpend,na.rm = TRUE)
var(startup_50$st_RDSpend,na.rm = TRUE)

startup_50$st_Administration <- scale(startup_50$Administration)
sd(startup_50$st_Administration,na.rm = TRUE)
var(startup_50$st_Administration,na.rm = TRUE)

startup_50$st_MarketingSpend <- scale(startup_50$`Marketing Spend`)
sd(startup_50$st_MarketingSpend,na.rm = TRUE)
var(startup_50$st_MarketingSpend,na.rm = TRUE)

startup_50$st_Profit <- scale(startup_50$Profit)
sd(startup_50$st_Profit,na.rm = TRUE)
var(startup_50$st_Profit,na.rm = TRUE)

#  RMSE value calculation
sqrt(sum(startup_50$`R&D Spend`^2)/nrow(startup_50))  
sqrt(sum(startup_50$Administration^2)/nrow(startup_50))
sqrt(sum(startup_50$`Marketing Spend`^2)/nrow(startup_50))
sqrt(sum(startup_50$`Profit`^2)/nrow(startup_50))

#checking how many cities are in state
unique(startup_50$State)

startup_50 <- cbind(startup_50,ifelse(startup_50$State=="New York",1,0), ifelse(startup_50$State=="California",1,0),  ifelse(startup_50$State=="Florida",1,0))


# Renaming the column
setnames(startup_50, 'V2','New York')
setnames(startup_50, 'V3','California')
setnames(startup_50, 'V4','Florida')

# Ploting the data on scatter plot
plot(startup_50[,-c('State')])#In this plot we are plotting dummy which seems no relative


plot(startup_50[,-c('State','New York','California','Florida')]) # After removing state and dummy columns

#After seeing scatter finding correlation
library(corpcor)
cor2pcor(cor(startup_50[,-c('State','New York','California','Florida')]))

#creating model
colnames(startup_50)

Profit_Model <- lm(Profit~`R&D Spend`+Administration+`Marketing Spend`, data = startup_50)

summary(Profit_Model)
#P value is greater than 0.05 so now checking the influence records

library(car)
influenceIndexPlot(Profit_Model)
influencePlot(Profit_Model,id.n=3)

Profit_Model_Inf <- lm(Profit~`R&D Spend`+Administration+`Marketing Spend`, data = startup_50[-c(50,49),])

summary(Profit_Model_Inf)

#Variance Inflation factor to check collinearity b/n variables
Profit_Model <- lm(Profit~`R&D Spend`+Administration+`Marketing Spend`, data = startup_50)
class(startup_50$`Marketing Spend`)

vif(Profit_Model)
summary(Profit_Model)

## vif>10 then there exists collinearity among all the variables 
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(Profit_Model)

#Creating final model after removing Administration data.
Profit_Model_Revised <- lm(Profit~`R&D Spend`+Administration+`Marketing Spend`+`New York`+California+Florida, data = startup_50)

library(MASS)

stepAIC(Profit_Model_Revised)
Profit_Model_Final <- lm(Profit~`R&D Spend`+`Marketing Spend`, data = startup_50)

summary(Profit_Model_Final)
plot(Profit_Model_Final)
qqPlot(Profit_Model_Final, id.n=5)
#R square value is 0.9483 and all p value is also significant.

#


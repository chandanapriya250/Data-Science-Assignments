# Load salary_data.csv dataset
library(readr)
library(psych)
salary_data <- read_csv("C://Users//Dell//Desktop//ExcelR//Assignments//COMPLETED_ASSIGNMENTS//Simple_linear_regression//Salary_Data.csv")
View(salary_data)

# Exploratory data analysis
summary(salary_data)

#Business moments
#Measures of central tendency, measures of dispersion, skewness, kurtosis

mean(salary_data$YearsExperience)

median(salary_data$YearsExperience)

mode(salary_data$YearsExperience)

hist(salary_data$YearsExperience, col = "royalblue")

boxplot(salary_data$YearsExperience,col = "pink")
skew(salary_data$YearsExperience)
kurtosi(salary_data$YearsExperience)
var(salary_data$YearsExperience, na.rm = TRUE)
sd(salary_data$YearsExperience, na.rm = TRUE)


#Scatter plot
plot(salary_data$YearsExperience, salary_data$Salary)  # plot(X,Y)

attach(salary_data)


#Correlation Coefficient (r)
cor(YearsExperience, Salary)             # cor(X,Y)

# Simple Linear Regression model
reg <- lm(Salary ~ YearsExperience) # lm(Y ~ X)

summary(reg)

pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(salary_data))  # one way to calculate RMSE value

sqrt(mean(reg$residuals^2)) #second way to calculate RMSE value

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)


ggplot(data = salary_data, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_data, aes(x=YearsExperience, y=Salary))



# Logrithamic Model

# x = log(YearsExperience); y = Salary

plot(log(YearsExperience), Salary)
cor(log(YearsExperience), Salary)

reg_log <- lm(Salary ~ log(YearsExperience))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(salary_data))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

# x = YearsExperience and y = log(Salary)

plot(YearsExperience, log(Salary))

cor(YearsExperience, log(Salary))

reg_exp <- lm(log(Salary) ~ YearsExperience)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logat <- predict(reg_exp)
at <- exp(logat)

error = salary_data$Salary - at
error

sqrt(sum(error^2)/nrow(salary_data))  #RMSE

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

##############################
# Polynomial model with 2 degree (quadratic model)

plot(YearsExperience, Salary)
plot(YearsExperience*YearsExperience, Salary)

cor(YearsExperience*YearsExperience, Salary)

plot(YearsExperience*YearsExperience, log(Salary))

cor(YearsExperience, log(Salary))
cor(YearsExperience*YearsExperience, log(Salary))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Salary) ~ YearsExperience + I(YearsExperience*YearsExperience))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = salary_data$Salary - expy

sqrt(sum(err^2)/nrow(salary_data))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = salary_data, aes(x = YearsExperience + I(YearsExperience^2), y = log(Salary))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_data, aes(x=YearsExperience+I(YearsExperience^2), y=logpol))


##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(Salary)~Waist + I(YearsExperience*YearsExperience) + I(YearsExperience*YearsExperience*YearsExperience))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


# visualization
ggplot(data = salary_data, aes(x = YearsExperience + I(YearsExperience^2) + I(YearsExperience^3), y = Salary)) +
  geom_point(color='blue') +
  geom_line(color='red',data = salary_data, aes(x=YearsExperience+I(YearsExperience^2)+I(YearsExperience^3), y=expy3))

################################


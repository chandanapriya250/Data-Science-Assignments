
# Load delivery_time.csv dataset
library(readr)
library(psych)
d_t <- read_csv("C://Users//Dell//Desktop//ExcelR//Assignments//COMPLETED_ASSIGNMENTS//Simple_linear_regression//delivery_time.csv")
View(d_t)

# Exploratory data analysis
summary(d_t)

#Business moments
#Measures of central tendency, measures of dispersion, skewness, kurtosis

mean(d_t$`Delivery Time`)

median(d_t$`Delivery Time`)

mode(d_t$`Delivery Time`)

hist(d_t$`Delivery Time`, col = "royalblue")

boxplot(d_t$`Delivery Time`,col = "pink")
skew(d_t$`Delivery Time`)
kurtosi(d_t$`Delivery Time`)
var(d_t$`Delivery Time`, na.rm = TRUE)
sd(d_t$`Delivery Time`, na.rm = TRUE)


#Scatter plot
plot(d_t$`Delivery Time`, d_t$`Sorting Time`)  # plot(X,Y)


attach(d_t)


#Correlation Coefficient (r)
cor(`Delivery Time`,`Sorting Time`)           # cor(X,Y)

# Simple Linear Regression model
reg <- lm(`Sorting Time` ~ `Delivery Time`) # lm(Y ~ X)

summary(reg)

pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(d_t))  #RMSE 


confint(reg,level=0.95)
predict(reg,interval="predict")


# ggplot for adding regresion line for data
library(ggplot2)


ggplot(data = d_t, aes(x = `Delivery Time`, y = `Sorting Time`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = d_t, aes(x=`Delivery Time`, y=`Sorting Time`))



# Logrithamic Model

# x = log(`Delivery Time`); y = `Sorting Time`

plot(log(`Delivery Time`), `Sorting Time`)
cor(log(`Delivery Time`), `Sorting Time`)

reg_log <- lm(`Sorting Time` ~ log(`Delivery Time`))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(d_t))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

# x = Delivery Time and y = log(Sorting Time)

plot(`Delivery Time`, log(`Sorting Time`))

cor(`Delivery Time`, log(`Sorting Time`))

reg_exp <- lm(log(`Sorting Time`) ~ `Delivery Time`)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logat <- predict(reg_exp)
at <- exp(logat)

error = d_t$`Sorting Time` - at
error

sqrt(sum(error^2)/nrow(d_t))  #RMSE

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

##############################
# Polynomial model with 2 degree (quadratic model)

plot(`Delivery Time`, `Sorting Time`)
plot(`Delivery Time`*`Delivery Time`, `Sorting Time`)

cor(`Delivery Time`*`Delivery Time`, `Sorting Time`)

plot(`Delivery Time`*`Delivery Time`, log(`Sorting Time`))

cor(`Delivery Time`, log(`Sorting Time`))
cor(`Delivery Time`*`Delivery Time`, log(`Sorting Time`))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(`Sorting Time`) ~ `Delivery Time` + I(`Delivery Time`*`Delivery Time`))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = d_t$`Sorting Time` - expy

sqrt(sum(err^2)/nrow(d_t))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = d_t, aes(x = `Delivery Time` + I(`Delivery Time`^2), y = log(`Sorting Time`))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = d_t, aes(x=`Delivery Time`+I(`Delivery Time`^2), y=logpol))


##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(`Sorting Time`)~`Delivery Time` + I(`Delivery Time`*`Delivery Time`) + I(`Delivery Time`*`Delivery Time`*`Delivery Time`))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


# visualization
ggplot(data = d_t, aes(x = `Delivery Time` + I(`Delivery Time`^2) + I(`Delivery Time`^3), y = `Sorting Time`)) +
  geom_point(color='blue') +
  geom_line(color='red',data = d_t, aes(x=`Delivery Time`+I(`Delivery Time`^2)+I(`Delivery Time`^3), y=expy3))

################################

#Insights:-

#Least RMSE value is the best model
# Logarithmic model has the least RMSE value when compared to other models 




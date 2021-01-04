library(haven)
library(dplyr)
library(ggpubr)
cutlets <- read.csv(file.choose())
View(cutlets)
attach(cutlets)
#normality test for Unit.A
ggdensity(cutlets$Unit.A, main="Normality test",xlab = "Unit A")
ggqqplot(Unit.A)
shapiro.test(Unit.A) # p-value is 0.32
#probability tet for Unit.A
qqnorm(Unit.A)
qqline(Unit.A)
# p-value is greater than 0.05, so accept null hypothesis.
#Data are normal.

#normality tet for Unit.B
ggdensity(cutlets$Unit.B, main="Normality test",xlab = "Unit B")
ggqqplot(Unit.B)
shapiro.test(Unit.B) # p-value is 0.52
#probability tet for Unit.B
qqnorm(Unit.B)
qqline(Unit.B)
# p-value is greater than 0.05, so accept null hypothesis.
#Data are normal.

#We can go for further test called variance test

# Variance test
#H0: Variance of UnitA = Variance of UnitB
#Ha: Variance of UnitA NOT = Variance of UnitB

var(Unit.A)
var(Unit.B)
var(Unit.A)/var(Unit.B)
var.test(Unit.A,Unit.B)
# p-value is > 0.05, so accept H0
#hence we prove variance of UnitA = variance of Unit B

# 2 sample t test to compare mean
#H0 : average of UnitA = average of UnitB
#Ha : average of Unit A NOT= average of Unit B

attach(cutlets)
t.test(Unit.A,Unit.B)
boxplot(Unit.A,Unit.B)

#p-value is 0.47 >0.05, so accept H0, hence average of Unit A = average of Unit B
# As per above results we can say that there is similarity between Unit A and Unit B i.e.., Unit A = Unit B
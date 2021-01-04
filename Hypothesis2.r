library(haven)
library(dplyr)
library(ggpubr)
library(car)
LabTAT <- read.csv(file.choose())
View(LabTAT)
attach(LabTAT)

#normality test for Laboratory1
ggdensity(LabTAT$Laboratory.1, main="Normality test",xlab = "Laboratory1")
ggqqplot(Laboratory.1)
shapiro.test(Laboratory.1) # p-value is 0.55
#probability test for Laboratory1
qqnorm(Laboratory.1)
qqline(Laboratory.1)
# p-value is greater than 0.05, so accept null hypothesis.
#Data are normal.

#normality test for Laboratory2
ggdensity(LabTAT$Laboratory.2, main="Normality test",xlab = "Laboratory.2")
ggqqplot(Laboratory.2)
shapiro.test(Laboratory.2) # p-value is 0.86
#probability test for Laboratory2
qqnorm(Laboratory.2)
qqline(Laboratory.2)
# p-value is greater than 0.05, so accept null hypothesis.
#Data are normal.

#normality test for Laboratory3
ggdensity(LabTAT$Laboratory.3, main="Normality test",xlab = "Laboratory.3")
ggqqplot(Laboratory.3)
shapiro.test(Laboratory.3) # p-value is 0.42
#probability test for Laboratory3
qqnorm(Laboratory.3)
qqline(Laboratory.3)
# p-value is greater than 0.05, so accept null hypothesis.
#Data are normal.

#normality test for Laboratory4
ggdensity(LabTAT$Laboratory.4, main="Normality test",xlab = "Laboratory.4")
ggqqplot(Laboratory.4)
shapiro.test(Laboratory.4) # p-value is 0.66
#probability test for Laboratory4
qqnorm(Laboratory.4)
qqline(Laboratory.4)
# p-value is greater than 0.05, so accept null hypothesis.
#Data are normal.

#To check equality of variances we go for Levene test

y <- c(Laboratory.1,Laboratory.2,Laboratory.3,Laboratory.4)

group <- as.factor(c(rep(1, length(Laboratory.1)), rep(2, length(Laboratory.2)),rep(3, length(Laboratory.3)),rep(4, length(Laboratory.4))))

leveneTest(y,group)
#plot(group,y)
# p-value = 0.051 >0.05. Accept H0, hence we prove all variances of all laboratory are same.

#Anova Test-one way
#H0 : average of all laboratory are same
#Ha : average of atleast 1 laboratory are different.

combined_labs <- data.frame(cbind(Laboratory.1,Laboratory.2,Laboratory.3,Laboratory.4))
summary(combined_labs)

stacked_labs <- stack(combined_labs)
summary(stacked_labs)

Anova_results <- aov(values~ind,data = stacked_labs)
summary(Anova_results)
# p-value is < 0.05, hence average of atleast 1 laboratory are different.
# As per results we can say that these are not equal,which means average of atleast 1 laboratory are different.
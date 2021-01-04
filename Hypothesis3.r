library(haven)
library(dplyr)
library(ggpubr)
library(car)
buyerRatio <- read.csv(file.choose())
View(buyerRatio)
attach(buyerRatio)

# we cannot perform normality test as sample size is less than 3

#chi square test
#H0: All averages are same
#Ha: Atleast one average are different

chisq.test(buyerRatio[,2:5])
chisq.test(t(buyerRatio[,2:5]))

# p-value=0.66>0.05. Accept H0,hence averages are same
#As per results we can say that there is proportion of male and female buying is similar.










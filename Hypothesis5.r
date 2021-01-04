library(haven)
library(dplyr)
library(ggpubr)
library(car)
faltoons <- read.csv(file.choose())
View(faltoons)
attach(faltoons)

#2-proportion Test
#H0 : Proportion of male vs female in weekdays = proportion of males vs females in weekends
#Ha : H0 : Proportion of male vs female in weekdays NOT = proportion of males vs females in weekends

names(faltoons)
table1 <- table(faltoons$Weekdays,faltoons$Weekend)
table1

prop.test(table1,correct = FALSE)

table2 <- table(faltoons$Weekend,faltoons$Weekdays)
table2

prop.test(table2,correct = FALSE)

#p-value = 0.968>0.05.So accept H0
# Hence Proportion of male vs female in weekdays = proportion of males vs females in weekends
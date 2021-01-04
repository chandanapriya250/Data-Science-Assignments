library(haven)
library(dplyr)
library(ggpubr)
library(car)
customerorderform <- read.csv(file.choose())
View(customerorderform)
attach(customerorderform)

#chi-square test
#H0 : All are same
#Ha : Atleast 1 are different


table(customerorderform$Phillippines,customerorderform$Indonesia,customerorderform$Malta,customerorderform$India)

chisq.test(customerorderform$Phillippines,customerorderform$Indonesia,customerorderform$Malta,customerorderform$India,simulate.p.value=TRUE) 

# p-value is 0.22>0.05. So accept H0,hence averages are same
#As per results we can say that all the centers are equal.
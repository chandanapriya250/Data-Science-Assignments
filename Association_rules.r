#To capture the different set of rule values for mymovies Dataset using apriori algorithm.
#Also Observe the change in number of rules for different support,confidence values

library(arules)
library(arulesViz)
library(rmarkdown)
mymovies <- read.csv(file.choose())

View(mymovies)

rules <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.2, confidence = 0.5,minlen=5)))

# Provided the rules with 2% Support, 50 % Confidence and watched a minimum of 2 movies
rules
inspect(head(sort(rules, by = "lift")))  
head(quality(rules))
plot(rules,method = "scatterplot")
plot(rules, method = "grouped")
plot(rules,method = "graph")


# To capture the different set of rule values for Books Dataset using apriori algorithm.
#Also Observe the change in number of rules for different support,confidence values
book <- read.csv(file.choose())

View(book)

rules <- apriori(as.matrix(book),parameter=list(support=0.02, confidence = 0.5,minlen=5))
rules
inspect(head(sort(rules, by = "lift"))) 
head(quality(rules))
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")


# To capture the different set of rule values for groceries Dataset using apriori algorithm.
#Also Observe the change in number of rules for different support,confidence values

data()
data("Groceries")
summary(Groceries)
rules <- apriori(Groceries,parameter=list(support=0.002, confidence = 0.5))
rules
inspect(head(sort(rules, by = "lift")))

plot(rules)
head(quality(rules))
plot(rules, method = "grouped")
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")


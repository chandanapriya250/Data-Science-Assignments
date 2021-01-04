library(readr)
library(knitr)
library(mlr)
library(ggplot2)
library(dendextend)
library(cowplot)

library(gridExtra)
library(dplyr)

library(GGally)

bank <- read.csv("C://Users//Dell//Desktop//ExcelR//Assignments//Logistic_regression//bank-full.csv", sep = ";")
#bank <- bank %>% select(-c(9:11)) #Dropping unnecessary variables (i.e.: contact, day, month)

#Data Cleaning and Transformation
str(bank)
summarizeColumns(bank) %>% knitr::kable( caption = 'Feature Summary before Data Preprocessing')

#Scanning for NAs
colSums(is.na(bank))

#Scanning for white space
bank[, sapply( bank, is.character )] <- sapply( bank[, sapply( bank, is.character )], trimws)

#Scanning for case errors
unique(bank$job)

unique(bank$marital)
unique(bank$education)
unique(bank$default)
unique(bank$housing)
unique(bank$loan)
unique(bank$poutcome)

#Renaming some variable's values
bank$default <-ifelse(bank$default =="yes", "defaulter","no defaulter")
bank$housing <-ifelse(bank$housing =="yes", "housing loan","no housing loan")
bank$loan <-ifelse(bank$loan =="yes", "personal loan","no personal loan")

#Exploratory data analysis

job_sum <- bank%>% group_by(job) %>% summarise(count = n())
job_sum$job <- job_sum$job %>% factor(levels = job_sum$job[order(-job_sum$count)]) 
ggplot(job_sum,aes(x = job, y = count)) + geom_bar(stat="identity") +theme(axis.text.x=element_text(angle=45,hjust=1)) + labs(title = "Figure 1")

marital_sum <- bank%>% group_by(marital) %>% summarise(count = n())
marital_sum$marital <- marital_sum$marital %>% factor(levels = marital_sum$marital[order(-marital_sum$count)]) 
ggplot(marital_sum,aes(x = marital, y = count)) + geom_bar(stat="identity") + labs(title = "Figure 2")

education_sum <- bank%>% group_by(education) %>% summarise(count = n())
education_sum$education <- education_sum$education %>% factor(levels = education_sum$education[order(-education_sum$count)]) 
ggplot(education_sum,aes(x = education, y = count)) + geom_bar(stat="identity") + labs(title = "Figure 3")

default_sum <- bank%>% group_by(default) %>% summarise(count = n())
default_sum$default <- default_sum$default %>% factor(levels = default_sum$default[order(-default_sum$count)]) 
ggplot(default_sum,aes(x = default, y = count)) + geom_bar(stat="identity") + labs(title = "Figure 4")

housing_sum <- bank%>% group_by(housing) %>% summarise(count = n())
housing_sum$housing <- housing_sum$housing %>% factor(levels = housing_sum$housing[order(-housing_sum$count)]) 
ggplot(housing_sum,aes(x = housing, y = count)) + geom_bar(stat="identity") + labs(title = "Figure 5")

loan_sum <- bank%>% group_by(loan) %>% summarise(count = n())
loan_sum$loan <- factor(c(loan_sum$loan), levels = c("personal loan", "no personal loan"), ordered = TRUE)
ggplot(loan_sum,aes(x = loan, y = count)) + geom_bar(stat="identity") + labs(title = "Figure 6")


poutcome_sum <- bank%>% group_by(poutcome) %>% summarise(count = n())
poutcome_sum$poutcome <- poutcome_sum$poutcome %>% factor(levels = poutcome_sum$poutcome[order(-poutcome_sum$count)]) 
ggplot(poutcome_sum,aes(x = poutcome, y = count)) + geom_bar(stat="identity") + labs(title = "Figure 7")

y_sum <- bank%>% group_by(y) %>% summarise(count = n())
y_sum$y <- y_sum$y %>% factor(levels = y_sum$y[order(-y_sum$count)]) 
ggplot(y_sum,aes(x = y, y = count)) + geom_bar(stat="identity") + labs(title = "Figure 8")


#numerical values
#Age

p <- ggplot(bank, aes(x = factor(1), y = age)) +   geom_boxplot(width = .50)
p1 <- ggplot(bank, aes(x = age)) +
  geom_density(fill = "dodgerblue", alpha = .2) +
  geom_histogram(colour="white",aes(y=..density..),alpha = 1/2) +
  geom_vline(xintercept= median(bank$age)) +
  annotate("text",label = "Median",x = 39, y = 0.045) +
  geom_vline(xintercept= mean(bank$age),linetype=2) +
  annotate("text",label = "Mean",x = 41, y = 0.05) + labs(title = "Figure 9")
plot_grid(p1, p + coord_flip() + theme(axis.title.y=element_blank(), 
                                       axis.text.y=element_blank(),
                                       axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1)) 

#Average yearly balance
p_balance <- ggplot(bank, aes(x = factor(1), y = balance)) +geom_boxplot(width = .50)

p1_balance <- ggplot(bank, aes(x = balance)) + labs(title = "Figure 10") + geom_histogram(colour="white",aes(y=..count..), bins = 10)

plot_grid(p1_balance, p_balance + coord_flip() + theme(axis.title.y=element_blank(), 
                                                       axis.text.y=element_blank(),
                                                       axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1)) 


#duration of last call

p_duration <- ggplot(bank, aes(x = factor(1), y = duration)) +   geom_boxplot(width = .50)
p1_duration <- ggplot(bank, aes(x = duration)) +
  labs(title = "Figure 11") + geom_histogram(colour ="white", aes(y=..count..),alpha = 1/2)
plot_grid(p1_duration, p_duration + coord_flip() + theme(axis.title.y=element_blank(),                                                        axis.text.y=element_blank(),
                                                         axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1)) 

#campaign
p_campaign <- ggplot(bank, aes(x = factor(1), y = campaign )) + labs(y ="Number of contact") + geom_boxplot(width = .50)
p1_campaign <- ggplot(bank, aes(x = campaign)) + labs(title = "Figure 12", x ="Number of contact") + geom_histogram(colour ="white", aes(y=..count..),alpha = 1/2)
plot_grid(p1_campaign, p_campaign + coord_flip() + theme(axis.title.y=element_blank(),                                                          axis.text.y=element_blank(),
                                                         axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1)) 

#passed days after client lost contact
p_pdays <- ggplot(bank, aes(x = factor(1), y = pdays)) +  labs(y ="Number of days passed") + geom_boxplot(width = .50)
p1_pdays <- ggplot(bank, aes(x = pdays)) +
  labs (title = "Figure 13",x ="Number of days passed") + geom_histogram(colour ="white", aes(y=..count..),alpha = 1/2)
plot_grid(p1_pdays, p_pdays + coord_flip() + theme(axis.title.y=element_blank(),                                                          axis.text.y=element_blank(),
                                                   axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1)) 


#previous

p_previous <- ggplot(bank, aes(x = factor(1), y = previous)) + labs(y = "Number of contacts") + geom_boxplot(width = .50)
p1_previous <- ggplot(bank, aes(x = previous)) +
  labs(title = "Figure 14", x= "Number of contacts") + geom_histogram(colour ="white", aes(y=..count..),alpha = 1/2)
plot_grid(p1_previous, p_previous + coord_flip() + theme(axis.title.y=element_blank(), 
                                                         axis.text.y=element_blank(),
                                                         axis.ticks.y = element_blank()), ncol=1, align="v",
          rel_heights = c(2,1)) 

##Factorise the target feature y
bank$y <- factor(c(bank$y), levels = c("yes","no"), ordered = TRUE)
#Divide the target feature into two seperate subsets with Yes and No
bank_yes <- bank %>% filter(bank$y =="yes")
bank_no <- bank %>% filter(bank$y =="no")

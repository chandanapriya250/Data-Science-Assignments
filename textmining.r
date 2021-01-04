library(twitteR)
library(ROAuth)
library(base64enc)
library(httpuv)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
cred <- OAuthFactory$new(consumerKey='BagGgBbanzbdpPNNp8Uy6TQBP', # Consumer Key (API Key)
                         consumerSecret='pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',                accessURL='https://api.twitter.com/oauth/access_token',                 authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")

#Access Token Secret

setup_twitter_oauth("BagGgBbanzbdpPNNp8Uy6TQBP", # Consumer Key (API Key)
                    "pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa", #Consumer Secret (API Secret)
                    "1076425245521731584-Ev31ZLB7Cf0idVMqDI8BxiVG2SgRnu",  # Access Token
                    "ZVUw0Z0mFrX7d6sjQxuB08l48JHhmnjmlAm86G2OPG7BS")  #Access Token Secret
#registerTwitterOAuth(cred)

Tweets <- userTimeline('facebook', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)
str(TweetsDF)
setwd("C:/Users/Dell/Desktop/ExcelR/Assignments/Text_mining")

write.csv(TweetsDF, "Tweets.csv",row.names = F)

getwd()


# Read file
facebook <- read.csv(file.choose())
str(facebook)# Build Corpus and DTM/TDM
corpus <- iconv(facebook$text, to = "UTF-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])
# Clean the text 
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])

cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset<-tm_map(cleanset,stripWhitespace)
inspect(corpus[1:5])

cleanset<-tm_map(cleanset,removeWords, c('facebook','can'))
cleanset<-tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])


#Term Document Matrix :

tdm <- TermDocumentMatrix(cleanset)
tdm
# the terms indicate that there are 2547 words and 1000 documents(# of tweets) in this TDM
# Sparsity is 100% which indicates that there are lots of zero values.
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

# Bar Plot 
w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 25) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))

# the word account as the highest frequency. This implies
# that facebook is more concerned about people's account

# Word Cloud :

w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(222)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.7)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
head(w)
wordcloud2(w,size = 0.5, shape = 'star',rotateRatio = 0.5, minSize = 1)

# lettercloud 

letterCloud(w,word = "F", size=1)
# Sentiment Analysis for tweets:

# Read File 
fbdata <- read.csv(file.choose(), header = TRUE)
tweets <- iconv(fbdata$text, to = 'UTF-8')
tweets <- as.character(fbdata$text)
class(tweets)
#obtain sentiment scores
s <-get_nrc_sentiment(tweets)
head(s)
tweets[6]


 #barplot
 barplot(colSums(s), las = 2, col = rainbow(10),
         ylab = 'Count',main= 'Sentiment scores for Facebook Tweets')

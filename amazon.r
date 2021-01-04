library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
################################ Amazon Reviews #############################
aurl <- "https://www.amazon.com/AUKEY-Bluetooth-Headphones-Microphones-Waterproof/dp/B08GCC4SHT/ref=sr_1_1_sspa?crid=HYWMTNA09N8&currency=INR&dchild=1&keywords=oneplus%2Bbullets%2Bwireless%2B2&qid=1609230263&sprefix=one%2Bplus%2Bbull%2Caps%2C505&sr=8-1-spons&smid=A2Y3YX6RFARCLC&spLa=ZW5jcnlwdGVkUXVhbGlmaWVyPUEyNUNCSkpWSDNGSlFLJmVuY3J5cHRlZElkPUEwMzk0MjA3MU1NV1I3WUVRWTBSTyZlbmNyeXB0ZWRBZElkPUEwMTE4OTc0M0tBRVdUUFg1M0RFRyZ3aWRnZXROYW1lPXNwX2F0ZiZhY3Rpb249Y2xpY2tSZWRpcmVjdCZkb05vdExvZ0NsaWNrPXRydWU&th=1"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
length(amazon_reviews)
setwd("C:/Users/Dell/Desktop/ExcelR/Assignments/Text_mining")
write.table(amazon_reviews,"AUKEYBLUETOOTH.csv",row.names = F)


AUKEY_oneplus <- read.delim('AUKEYBLUETOOTH.csv')
AUKEY_oneplus$x <- as.factor(AUKEY_oneplus$x)
str(AUKEY_oneplus)
View(AUKEY_oneplus)

# Build Corpus and DTM/TDM
library(tm)
corpus <- AUKEY_oneplus[-1,]
head(corpus)
class(corpus)
corpus <- iconv(AUKEY_oneplus$x, to = "UTF-8")
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

cleanset<-tm_map(cleanset,removeWords, c('headphones','earbuds'))

cleanset <- tm_map(cleanset, gsub,pattern = 'microphone', replacement = 'mike')

inspect(cleanset[1:5])

cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm

tdm <- as.matrix(tdm)
tdm[1:10,1:20]

# Bar Plot 

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 25) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))

# Word Cloud :
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(222)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)

library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.3, shape = 'star', rotateRatio = 0.5, minSize = 1)

# lettercloud 

letterCloud(w,word = 'Am',frequency(5), size=1)

# Read File 
Amzn_reviews <-  read.delim('AUKEYBLUETOOTH.csv')
reviews <- as.character(Amzn_reviews[-1,])
class(reviews)
s <- get_nrc_sentiment(reviews)


head(s)

reviews[4]

 # barplot 
 
 barplot(colSums(s), las = 2.5, col = rainbow(10),
         ylab = 'Count',main= 'Sentiment scores for Amazon Reviews
        -oneplus bulletsz')
 
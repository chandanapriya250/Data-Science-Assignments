library(tidyverse)
library(Matrix)
library(recommenderlab)
library(dplyr)
library(caTools)
library(kableExtra)
library(gridExtra)
library(ggplot2)
library(tidyr)
book_ratings <-read.csv("C://Users//Dell//Desktop//ExcelR//Assignments//Recommendation _systems//book.csv")
view(book_ratings)
# table dimensions
dim(book_ratings)

# first few ratings for books
head(book_ratings, 10)
object.size(book_ratings)


bmatrix <- as(book_ratings, "realRatingMatrix")
dim(bmatrix)

sim <- similarity(bmatrix[1:10, ], method = "cosine", which = "users")
image(as.matrix(sim), main = "User Similarity")

sim2 <- similarity(bmatrix[ ,1:10], method = "cosine", which = "items")
image(as.matrix(sim2), main = "Item Similarity")


book_ratings$User.ID<-as.factor(book_ratings$User.ID)

rated_count <- colCounts(bmatrix)
read_book <- data.frame(
  User.ID = names(rated_count),
  read = rated_count
)
book_ratings$Book.Rating<-as.factor(book_ratings$Book.Rating)
ratings<- colCounts(bmatrix)

book_ratings<-data.frame(User.ID = names(rated_count),
  Book.Title = names(ratings),
       read1 = ratings                  
)




top_books <- 
  inner_join(read_book, book_ratings, by = "User.ID") %>% 
  arrange(desc(read)) %>% 
  select(-User.ID) %>% 
  head(10) %>% 
  ggplot(aes(x = title, y = read)) + geom_bar(stat = "identity", fill = "lightblue") + geom_text(aes(label=read), vjust=-0.3, size=3.5) + ggtitle("Top 10 Rated Books") +  coord_flip() 


top_books

avg_book_ratings <- data.frame("avg_rating" = colMeans(bmatrix)) %>% 
  ggplot(aes(x = avg_rating)) + stat_bin(bins = 20) +
  geom_histogram(color = "black", fill = "lightgreen") + 
  ggtitle("Distribution of Average Ratings for Books") 
 avg_book_ratings

image(bmatrix[2:100, 2:100], main = "First 100 users and books")

min_readers <- quantile(rowCounts(bmatrix), 0.99)
min_books <- quantile(colCounts(bmatrix), 0.99)
a <- image(bmatrix[rowCounts(bmatrix) > min_readers, colCounts(bmatrix) > min_books], main = "Non-Normalized")
# to eliminate bias therefore average rating would be 0
book_norm <- normalize(bmatrix)
b <- image(book_norm[rowCounts(book_norm) > min_readers, colCounts(book_norm) > min_books], main = "Normalized")
grid.arrange(a, b, ncol = 2)

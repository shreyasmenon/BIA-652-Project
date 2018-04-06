#Data preprocessing

#Reading the dataset
reddit_data <- read.csv('C:/Users/Dell1/Desktop/Stevens MIS Coursework/Multivariate Reddit Project/RS_2017-09_filtered70.csv',header = TRUE)
View(reddit_data)

#Dropping columns that are unnecessary

#dropping the ID column as it is not needed for processing
reddit_data$X <- NULL 

#dropping the columns from the dataset with missing values
reddit_data <- reddit_data[,colSums(is.na(reddit_data))<nrow(reddit_data)] 

#Cleaning missing data beyond a threshold value
thresh_values <- round(nrow(reddit_data) * 0.8)
for (i in names(reddit_data)) {
  if (!is.na(sum(reddit_data[i] == "")) & sum(reddit_data[i] == "") > thresh_values) {
    reddit_data[i] <- NULL
  }
}

#Handling Missing values
#for numeric columns thumbnail_height and thumbnail_weight
reddit_data$thumbnail_height = ifelse(is.na(reddit_data$thumbnail_height),
                                      ave(reddit_data$thumbnail_height, FUN = function(x)
                                        mean(x,na.rm = TRUE)),reddit_data$thumbnail_height)

reddit_data$thumbnail_width = ifelse(is.na(reddit_data$thumbnail_width),
                                     ave(reddit_data$thumbnail_width, FUN = function(x)
                                       mean(x,na.rm = TRUE)),reddit_data$thumbnail_width)

#Convert variable names to weekday
library(lubridate)
reddit_data$created_utc <- factor(wday(x =  as.POSIXct(reddit_data$created_utc , origin="1970-01-01"), label = TRUE),ordered = FALSE)
reddit_data$retrieved_on <- factor(wday(x =  as.POSIXct(reddit_data$retrieved_on , origin="1970-01-01"), label = TRUE),ordered = FALSE)

#Splitting the dataset in 70-30 ratio (@menon:Use this...this works)
indexes <- sample(1:nrow(reddit_data), size = 0.3*nrow(reddit_data))
test <- reddit_data[indexes,]
train <- reddit_data[-indexes,]

#install.packages('caTools')
#library(caTools)
#set.seed(123)
#reddit_data$split <- sample.split(reddit_data, SplitRatio = 0.7)
#training_set <- subset(reddit_data, reddit_data$split==TRUE)
#testing_set <- subset(reddit_data, reddit_data$split==FALSE)
#View(training_set)
#View(testing_set)

#for first 20 values (@menon: this is simple splitting ...just taking the first 20 records)
first <- reddit_data[1:20,]
View(first)

#Text manipulation
#library(tm)
#library(SnowballC)
#first$title <- VCorpus(VectorSource(first$title))
#first$title <- tm_map(first$title, content_transformer(tolower))
#first$title <- tm_map(first$title, removePunctuation)
#first$title <- tm_map(first$title, removeWords, stopwords("english"))
#first$title <- tm_map(first$title, removeNumbers)
#first$title <- tm_map(first$title, stemDocument)
#first$title <- tm_map(first$title, stripWhitespace)
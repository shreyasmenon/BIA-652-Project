#Data preprocessing

#Reading the dataset
reddit_data <- read.csv('C:/Users/Dell1/Desktop/Multivariate Reddit Project/RS_2017-09_filtered70.csv',header = TRUE)
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

#for further splitting the data 
#factor(wday(x =  as.POSIXct(reddit_data$created_utc , origin="1970-01-01"), label = TRUE),ordered = FALSE, labels  = c('0','1','2','3','4','5','6') )

#Text manipulation
#Converting all letters to lowercase
reddit_data$title <- sapply(reddit_data$title, tolower)

#Removing stop words


#Removing numbers
reddit_data$title <- gsub("[[:digit:]]", "", reddit_data$title)

#Removing punctuation
reddit_data$title <- gsub('[[:punct:]]', '', reddit_data$title)
#Removing white spaces
reddit_data$title <- gsub("[ \t]{2,}", " ", reddit_data$title)
reddit_data$title <- gsub("^\\s+|\\s+$", "", reddit_data$title)
 

library(tm)
#library(SnowballC)
#reddit_title <- VCorpus(VectorSource(reddit_data$title))
#reddit_title <- tm_map(reddit_title, content_transformer(tolower))
#reddit_title <- tm_map(reddit_title, removePunctuation)
#reddit_title <- tm_map(reddit_title, removeWords, stopwords("english"))
#reddit_title <- tm_map(reddit_title, removeNumbers)
#reddit_title <- tm_map(reddit_title, stemDocument)
#reddit_title <- tm_map(reddit_title, stripWhitespace)


dim(reddit_data)
names(reddit_data)

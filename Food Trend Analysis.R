rm(list=ls())
library(tm)
library(wordcloud)
library(topicmodels)
library(stringr)
library(lubridate)
library(forecast)
library(stats)


#############   Topic Modeling   ############
# Build corpus
docs2015 <- Corpus(DirSource("/Users/jiaxingfeng/Desktop/Rochester/Spring/Social Media/HW/5/fpost-2015"))
# Construct the 
mystop <- c('US', 'today', 'http', 'https', '“', '”', 'we', 'what', 'friend', 'day','great','good',
            'will','just','new','love','make','know','time','birthday','food','free','get','can','thank','thanks','many','made','now','see','please') 
dtm <- DocumentTermMatrix(docs2015, control=list(tolower=T, removePunctuation=T, removeNumbers=T, stripWhitespace=T, stopwords=c(mystop, stopwords("english"), stopwords("spanish"))))
# remove terms with a low appearance probability (about 1%)
dtm <- removeSparseTerms(dtm,0.99)
# remove empty documents
idx <- rowSums(as.matrix(dtm))>0
newdocs <- docs2015[idx]
dtm <- dtm[idx,]
# Run lda model to do the topic modeling (10 topics)
lda.model = LDA(dtm, 10)

###########   Examine frequent words used in 2015   #############
termsPerTopic <- terms(lda.model,20)
top10terms <- termsPerTopic[1:20,]
top10terms   # returns top 10 terms used for each topic
# From the matrix we can conclude that, in 2015, facebook users mentions more about "cup cake","cheese","chicken","cream",
# "chocolate","sauce","garlic"


###########   Explore food trend   #############
# create a monthly index for a hypothetical trend
x_axis <- as.Date("2010-12-01") + months(1:60)

# Defind function to return the monthly precent of posts containing the term we want to explore in all the posts
# So 60 percentages in total
freq <- c()
term_freq<- function(term)
{
  for (i in 1:5){
    # construct path to read files
    year <- i
    folder <- paste("fpost-201",as.character(i),sep = '')
    folderyear <- paste("fpost-201",as.character(year),"-",sep='')
    for (j in 1:12) {
      month <- j
      filename <- paste(folderyear, as.character(month), ".csv", sep='')
      posts <- readLines(paste(folder,as.character('/'),filename,sep = ''))     # read the file
      posts <- tolower(posts)
      freq1 <- length(posts[grepl(as.character(term), posts,ignore.case=TRUE, perl=TRUE)])   # count term frequency in each file
      freq_all <- length(posts)
      freq <- c(freq,freq1/freq_all)  # record appearance probability of the word in time series # probability is more reasonale than absolute count
    }
  }
  freq
}

############# Check time series trend of different terms ################

############# cake ################
# Get frequency of the term across the 60 documents
freq1 <- term_freq("cake") 
df1 <- data.frame(x_axis,freq1)
# plot the time series term frequency 
plot(df1,type = "l",main="Frequency of posts mentioning Cake",xlab = 'Time',ylab = 'Apperance Probability')
# There is a increasing frequency of cake, so let's check specific types of cakes.
# LDA model in 2015 indicates high frequency of "cheese","cup", so I will plot time series frequency of cheese cake and cup cake.

# use ts() function convert numeric vector into R time series object to see the trend
timeseries <- ts(data = df1$freq,frequency=12)    
plot(timeseries,main="Time Series of Cake")
ggseasonplot(timeseries)

############# cup cake ################
freq2 <- term_freq("cup cake|cupcake") 
df2 <- data.frame(x_axis,freq2)
plot(df2,type = "l",main="Frequency of posts mentioning Cup Cake",xlab = 'Time',ylab = 'Apperance Probability')
# Mentioned more in 2013, lower frequency in 2015, but the end of 2015 has witnessed an increase.

timeseries2 <- ts(data = df2$freq,frequency=12)    
ggseasonplot(timeseries2)

############# cheese cake ################
dev.off()
freq3 <- term_freq("cheese cake") 
df3 <- data.frame(x_axis,freq3)
plot(df3,type = "l",main="Frequency of posts mentioning Cheese Cake",xlab = 'Time',ylab = 'Apperance Probability')
# Apparently an emerging trend in cheese cake.

############# pumpkin ################
freq4 <- term_freq("pumpkin pie") 
df4 <- data.frame(x_axis,freq4)
plot(df4,type = "l",main="Frequency of posts mentioning Pumpkin Pie",xlab = 'Time',ylab = 'Apperance Probability')
# More frequently mentioned in the end of each year, may be due to Thanks-Giving (validated by the seasonally consumed food)

############ Cauliflower rice ################
freq5 <- term_freq("cauliflower rice|cauliflower") 
df5 <- data.frame(x_axis,freq5)
plot(df5,type = "l",main="Frequency of posts mentioning Cauliflower rice",xlab = 'Time',ylab = 'Apperance Probability')
# Increasing popularity

############# Vegetable Noodle ################
freq6 <- term_freq("vegetable noodle|veggie noodle|zucchini noodle|zoodle|vegetable pasta|veggie pasta") 
df6 <- data.frame(x_axis,freq6)
plot(df6,type = "l",main="Frequency of posts mentioning Vegetable Noodle",xlab = 'Time',ylab = 'Apperance Probability')
# Increasing popularity

############# Spicy ################
freq7 <- term_freq("spicy") 
df7 <- data.frame(x_axis,freq7)
plot(df7,type = "l",main="Frequency of posts mentioning Spicy food",xlab = 'Time',ylab = 'Apperance Probability')
# Dramastic increasing popularity

############# Ramen ################
freq7 <- term_freq("ramen|korea noodle|korean noodle|japanese noodle|japan noodle") 
df7 <- data.frame(x_axis,freq7)
plot(df7,type = "l",main="Frequency of posts mentioning Ramen",xlab = 'Time',ylab = 'Apperance Probability')
# Increasing popularity

############# Chicken ################
freq8 <- term_freq("chicken") 
df8 <- data.frame(x_axis,freq8)
plot(df8,type = "l",main="Frequency of posts mentioning Chicken",xlab = 'Time',ylab = 'Apperance Probability')
# Increasing popularity

############# Chicken wings ################
freq9 <- term_freq("chicken wing|chicken wings") 
df9 <- data.frame(x_axis,freq9)
plot(df9,type = "l",main="Frequency of posts mentioning Chicken wings",xlab = 'Time',ylab = 'Apperance Probability')

############# Chocolate ################
freq10 <- term_freq("chocolate") 
df10 <- data.frame(x_axis,freq10)
plot(df10,type = "l",main="Frequency of posts mentioning chocolate",xlab = 'Time',ylab = 'Apperance Probability')

############# craft ################
freq11 <- term_freq("craft") 
df11 <- data.frame(x_axis,freq11)
plot(df11,type = "l",main="Frequency of posts mentioning craft",xlab = 'Time',ylab = 'Apperance Probability')

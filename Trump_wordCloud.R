#Required Packages
install.packages("tidyverse")
install.packages("tm")
install.packages("tidytext")
install.packages("topicmodels")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("wordcloud") 
install.packages("pals")
library(tidyverse)
library(tm)
library(tidytext)
library(topicmodels)
library(reshape2)
library(ggplot2)
library(wordcloud)
library(pals)

#Collect data 
trumpTweets <- read.csv("trump_tweets.csv")
head(trumpTweets, 10)

#extracting text data 
trumpTweets <- as.data.frame(trumpTweets[,11])
trumpTweets <- trumpTweets %>% 
  rename(tweets = `trumpTweets[, 11]`)

#Create Corpus for text mining 
english_stopwords <- readLines("https://slcladal.github.io/resources/stopwords_en.txt", encoding = "UTF-8")
corpusTweets <- Corpus(VectorSource(trumpTweets$tweets))

#text processing 
add_space <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))}) #functions to add spaces before special punctuation 
corpusTweets <- tm_map(corpusTweets,
                       add_space, "-")
corpusTweets <- tm_map(corpusTweets,
                       add_space, ":")
corpusTweets <- tm_map(corpusTweets,
                       add_space, "'")
corpusTweets <- tm_map(corpusTweets,
                       add_space, " -")
corpusTweets <- tm_map(corpusTweets,
                       add_space, "~")

processedTweets <- tm_map(corpusTweets,
                          removePunctuation, preserve_intra_word_dashes = TRUE) #remove punctuation
processedTweets <- tm_map(processedTweets,
                          removeWords, c(stopwords("english"), "https", "...", "the", "you", "for", "get")) #remove english stopwords
processedTweets <- tm_map(processedTweets,
                          removeNumbers) #remove numbers
processedTweets <- tm_map(processedTweets,
                          content_transformer(tolower)) #standardize data by remove capital letters
processedTweets <- tm_map(processedTweets,
                          stemDocument, language = "en") #stemming

#Model Development 
minimumFrequency <- 7 #used as baseline for dtm - using higher to have manageable/more accurate outputs
dtm <- DocumentTermMatrix(processedTweets, control = list(bounds = list(global = c(minimumFrequency, Inf))))
dim(dtm) #check dimensions
rowTotals <- apply(dtm , 1, sum) #Find the sum of words per row
dtm.new   <- dtm[rowTotals> 0, ] #remove rows w/o words

K <- 5 #number of topics to discover
set.seed(10) #maintain reproducible results
topicModel <- LDA(dtm.new, K, method="Gibbs", control=list(iter = 500, verbose = 25)) #compute LDA model

nTerms(dtm.new) #396 terms 
terms(topicModel, 20) #preview of clusters

#obtain beta (probability distribution) for each topic 
trump_topics <- tidy(topicModel, matrix = "beta")

trumpTop_terms <- trump_topics %>%
  group_by(topic) %>%
  top_n(50, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#develop word cloud for probability distribution of words within each topic 
trumpTop_terms %>%
  mutate(topic = paste("Topic", topic, sep = " : ")) %>%
  acast(term ~ topic, value.var = "beta", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4", "#00FF00", "#FFFF00", "#FFA500" ),
                   max.words = 60)


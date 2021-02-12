#Load in packages
install.packages("plotly")
install.packages("textdata")
library(textdata)
library(plotly)

#Collect data 
trumpTweets <- read.csv("trump_tweets.csv")
head(trumpTweets, 10)

#extracting text data 
trumpTweets <- as.data.frame(trumpTweets[,11])
trumpTweets <- trumpTweets %>% 
  rename(tweets = `trumpTweets[, 11]`)

#cleaning data
trumpTweets_clean <- mutate(trumpTweets, tweets = as.character(trumpTweets$tweets))
trump_tweets_clean <- trumpTweets_clean %>%
  select(tweets) %>%
  unnest_tokens(word, tweets)

head(trump_tweets_clean)

#using NRC lexicon 
text_emotion <- get_sentiments("nrc")

#match emotions to trump tweets
trumpSentiment <- trump_tweets_clean %>%
  inner_join(text_emotion)

trumpSentiment

#perform initial sentiment analysis 
trumpSentiment_count <- trumpSentiment %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)
trumpSentiment_count

trumpSentiment_count$sentiment <- factor(trumpSentiment_count$sentiment, levels = unique(trumpSentiment_count$sentiment)[order(trumpSentiment_count$n, decreasing = TRUE)])

plot_ly(data = trumpSentiment_count, 
        x = ~sentiment, 
        y = ~n, 
        color = ~sentiment,
        #marker = list(color =pal), 
        type = "bar") %>%
  layout(xaxis=list(title=""), showlegend=F,
         title="Emotion Mining for President Trumps Tweets (07/2020 - 11/2020)",
         yaxis = list(title = "Number of Tweets"))


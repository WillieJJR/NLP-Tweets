#Load in packages
install.packages("NLP")
install.packages("openNLP")
install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at")
install.packages("rJava")
install.packages("treemapify")
install.packages("gghighlight")
library(NLP)
library(openNLP)
library(rJava)
library(plotly)
library(treemapify)
library(gghighlight)

#extracting text data
trumpTweets <- read.csv("trump_tweets.csv")

trumpTweets <- trumpTweets[trumpTweets$favorites > 10000,] #used to filter data to work with annotate()

trumpTweets <- as.data.frame(trumpTweets[,11])
trumpTweets <- trumpTweets %>% 
  rename(tweets = `trumpTweets[, 11]`)

trumpTweets_vec <- paste(unlist(trumpTweets), collapse =" ") #have to change the df into a character vector to have all text in one row

trumpTweets_vec <- as.String(trumpTweets_vec) #openNLP can only work with string vectors 

#create annotation object

word_ann <- Maxent_Word_Token_Annotator() #using for word annotations
sent_ann <- Maxent_Sent_Token_Annotator() #using for sentence annotations

tweet_annotations <- NLP::annotate(trumpTweets_vec, list(sent_ann, word_ann)) #creating an annotation object

tweet_ptd <- AnnotatedPlainTextDocument(trumpTweets_vec, tweet_annotations)
tweet_ptd

#annotating people and places

person_ann <- Maxent_Entity_Annotator(kind = "person") #function to recognize people
location_ann <- Maxent_Entity_Annotator(kind = "location") #function to recognize places
organization_ann <- Maxent_Entity_Annotator(kind = "organization") #functions to recognize orgs


pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann) #create a pipeline to hold annotators
trumpTweets_annotations <- NLP::annotate(trumpTweets_vec, pipeline) #create annotation object
trumpTweets_ptd <- AnnotatedPlainTextDocument(trumpTweets_vec, trumpTweets_annotations)

#function for entity recognition output
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotation(doc)
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

#quantify the output for most popular people mentioned by frequency
person <- entities(trumpTweets_ptd, kind = 'person')

freq_person <- as.data.frame(table(person))
freq_person
freq_person %>% 
  arrange(desc(Freq))

#graph output - Top 10 people mentioned by the President 
person_Top_10 <- freq_person %>% 
  arrange(desc(freq_person$Freq)) %>%
  slice(1:10)

bar_people <- ggplot(person_Top_10, aes(x=reorder(person, -Freq), y=Freq, name = person)) + 
  geom_point(size=5, color = "red", fill=alpha("blue", 0.3), alpha=0.7, shape=21, stroke=2) + 
  geom_segment(aes(x=reorder(person, -Freq), 
                   xend=reorder(person, -Freq), 
                   y=0, 
                   yend=Freq)) + 
  labs(title="Who's on the President's Mind?", 
       subtitle="Top 5 People Mentioned in Ex-President Trumps Tweets", 
       caption="Source: Twitter",
       x = "People",
       y = "Frequency of Mentions") +
  theme_classic()

bar_people <- bar_people + theme(axis.text.x = element_text(angle = 45))
ggplotly(bar_people, tooltip = c("name","Freq"))

#graph output - Top 10 places mentioned by the President 
loc <- entities(trumpTweets_ptd, kind = 'location')
freq_loc <- as.data.frame(table(loc))
freq_loc
freq_loc %>% 
  arrange(desc(Freq))

Loc_Top_10 <- freq_loc %>% 
  arrange(desc(freq_loc$Freq)) %>%
  slice(1:10)

Loc_Top_10

Loc_Tree <-ggplot(Loc_Top_10, aes(area = Freq, label = paste(loc, Freq, sep = "\n"), fill = as.factor(Freq))) +
  geom_treemap() +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    size = 15,
  ) +
  scale_fill_brewer(palette = "Blues", name = "# of Mentions",
                    direction = 1,
  )

Loc_Tree + theme(legend.position = "none")

#graph output - Top 10 organizations mentioned by the President
org <- entities(trumpTweets_ptd, kind = 'organization')

freq_org <- as.data.frame(table(org))
freq_org
freq_org %>% 
  arrange(desc(Freq))

#quantify the output for most popular orgs mentioned by frequency
#graph output - Top 10 porgs mentioned by the President 
Org_Top_10 <- freq_org %>% 
  arrange(desc(freq_org$Freq)) %>%
  slice(1:10)

Org_Top_10

#highlight max value 
Org_Top_10 <- Org_Top_10 %>% mutate( highlightMax = ifelse( Freq == max(Freq), "yes", "no" ) )

org_plot <- ggplot( Org_Top_10, aes( x = reorder(org, Freq), y = Freq, fill = highlightMax ) ) +
  geom_bar( stat = "identity", width = 1 ) +
  scale_fill_manual( values = c( "yes"="red", "no"="#0099f9" ), guide = FALSE )+
  labs(
    title="What's on the President's Mind?", 
    subtitle="Top 5 Organizations Mentioned in Ex-President Trumps Tweets", 
    caption="Source: Twitter",
    x = "Organizations",
    y = "Frequency of Mentions"
  )+
  coord_flip()+
  theme_classic()+
  theme(legend.position = "none")


ggplotly(org_plot, tooltip = c("Organization Name","Freq"))

library(dplyr) #Data manipulation (also included in the tidyverse package)
library(tidytext) #Text mining
library(tidyr) #Spread, separate, unite, text mining (also included in the tidyverse package)
library(ggplot2)
library(wordcloud)
library(RColorBrewer) 
library(forcats)

#set the environment location
setwd("E:/UniMAP/SEM8/Data/Assignment 3")


#read .csv file
wifi_adapter <- read.csv("review.csv", header = TRUE)



#convert factor datatype to  char
wifi_adapter$review <- as.character(wifi_adapter$review)
wifi_adapter$product <- as.character(wifi_adapter$product)

#seperate sentences by word
tidy_wifi_adapter <- wifi_adapter %>%
  # Group by the titles of the plays
  group_by(product) %>%
  # Define a new column linenumber
  #to seperate data by number
  
  unnest_tokens(word, review) %>%
  ungroup()
tidy_wifi_adapter

 

# get the sentiment for each word
analysis_data <- tidy_wifi_adapter %>%
  inner_join(get_sentiments("bing"))

analysis_data

#total sentiment
#Pipe the tidy adapter data frame to the next line
total_word <- analysis_data %>%
  # Use count to find out how many times each word is used
  count(word, sort = TRUE)

total_word_rank <- total_word %>%
  filter(n > 1)
total_word_rank
#positive sentiment
#Pipe the tidy adapter data frame to the next line
total_positive_word <- analysis_data %>% 
  # Use count to find out how many times each word is used
  filter(sentiment == "positive") %>%
  count(word, sort = TRUE)
total_positive_word

#negative sentiment
#Pipe the tidy adapter data frame to the next line
total_negative_word <- analysis_data %>% 
  # Use count to find out how many times each word is used
  filter(sentiment == "negative") %>%
  count(word, sort = TRUE)
total_negative_word
tidy_wifi_adapter
# get the sentiment for each word
analysis_data <- tidy_wifi_adapter %>%
  inner_join(get_sentiments("bing")) %>%
  count(product, year, sentiment) %>%
  # Spread sentiment and n across multiple columns
  spread(sentiment, n, fill = 0) %>%
  # Use mutate to find net sentiment or score
  mutate(sentiment = positive - negative)



ggplot(analysis_data, aes(x = year, y = sentiment)) + 
  geom_point(aes(color = product))+ # add points to our plot, color-coded by wifi adapter
  geom_smooth(method = "auto") # pick a method & fit a model

#generate boxplot based on product and sentiment
ggplot(analysis_data, aes(x = product, y = sentiment, color = product)) + 
  geom_boxplot() + geom_point()

set.seed(1234)

#overall word cloud
wordcloud(words = total_word$word,freq = total_word$n, min.freq = 1, max.words = 200,
          random.order=FALSE, rot.per=0.35,     
          colors=brewer.pal(8, "Dark2"))

#positive word cloud
wordcloud(words = total_positive_word$word,freq = total_positive_word$n, min.freq = 1, max.words = 200,
          random.order=FALSE, rot.per=0.35,     
          colors=brewer.pal(8, "Dark2"))

#negative word cloud
wordcloud(words = total_negative_word$word,freq = total_negative_word$n, min.freq = 1, max.words = 200,
          random.order=FALSE, rot.per=0.35,     
          colors=brewer.pal(8, "Dark2"))

# Barplot
ggplot(total_word_rank, aes(x=word, y=n)) + 
  geom_bar(stat = "identity") +
  coord_flip()




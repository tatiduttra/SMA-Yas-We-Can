setwd("C:/Users/ffan1/OneDrive - IESEG (1)/Social Media Analytics")
BearerToken <- "AAAAAAAAAAAAAAAAAAAAACnYYAEAAAAA37CbzC2VNxfxnSusEb%2BssS1PIlI%3DjaqRrFzGj0KPIQkpFxXjBMC7aY5HQ39qnvspJWnwdnc8HpOQ7p"

gc()
rm(list=ls())

if(!require("httr")) install.packages("httr"); library("httr")
if(!require("jsonlite")) install.packages("jsonlite"); library("jsonlite")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")

Xbox <- get_timelines("Xbox",n=3500)

glimpse(Xbox) 

Xbox <- Xbox %>% filter(lang == "en" )

# how do I get this: - extract replies on Xbox tweets by users? 

#create NA marker for Xbox retweets
#O when value is missing and 1 when it exists
Xbox$reply_to_user_id<-ifelse(is.na(Xbox$reply_to_user_id), 0, 1)
XboxOwnTweets <- Xbox[Xbox$reply_to_user_id == 0,]
XboxRetweets <- Xbox[Xbox$reply_to_user_id == 1,]

# check NA values 
sapply(Xbox,function(x) sum(is.na(x))) 

#Xbox total

# convert dates datatypes 
Xbox$created_at <- as.Date(Xbox$created_at, "%Y%m%d") 


plot <- Xbox %>% 
  group_by(created_at) %>% 
  summarize(sum = sum(favorite_count)) %>% 
  ungroup()

ggplot(plot, aes(x=created_at, y=sum)) + 
  geom_bar(stat = "identity", fill = "light blue") +
  labs(title= "favorite count",
       x = "days") 

# explore what happened at the 5 favorite peaks

plotretweet <- Xbox %>% 
  group_by(created_at) %>% 
  summarize(sum = sum(retweet_count)) %>% 
  ungroup()

ggplot(plotretweet, aes(x=created_at, y=sum)) + 
  geom_bar(stat = "identity", fill = "light blue") +
  labs(title= "retweet count",
       x = "days") 

# explore what happened at the 4 retweet peaks

#Xbox own tweets

# convert dates datatypes 
XboxOwnTweets$created_at <- as.Date(XboxOwnTweets$created_at, "%Y%m%d") 


plotown <- XboxOwnTweets %>% 
  group_by(created_at) %>% 
  summarize(sum = sum(favorite_count)) %>% 
  ungroup()

ggplot(plotown, aes(x=created_at, y=sum)) + 
  geom_bar(stat = "identity", fill = "light blue") +
  labs(title= "xbox own favorite count",
       x = "days") 

# explore what happened at the 5 favorite peaks

plotownretweet <- XboxOwnTweets %>% 
  group_by(created_at) %>% 
  summarize(sum = sum(retweet_count)) %>% 
  ungroup()

ggplot(plotownretweet, aes(x=created_at, y=sum)) + 
  geom_bar(stat = "identity", fill = "light blue") +
  labs(title= "xbox own retweet count",
       x = "days") 

# explore what happened at the 4 retweet peaks

#Xbox retweets

# convert dates datatypes 
XboxRetweets$created_at <- as.Date(XboxRetweets$created_at, "%Y%m%d") 


plotxboxfav <- XboxRetweets %>% 
  group_by(created_at) %>% 
  summarize(sum = sum(favorite_count)) %>% 
  ungroup()

ggplot(plotxboxfav, aes(x=created_at, y=sum)) + 
  geom_bar(stat = "identity", fill = "light blue") +
  labs(title= "xbox retweet favorite count",
       x = "days") 

# explore what happened at the 5 favorite peaks

plotxboxret <- XboxRetweets %>% 
  group_by(created_at) %>% 
  summarize(sum = sum(retweet_count)) %>% 
  ungroup()

ggplot(plotxboxret, aes(x=created_at, y=sum)) + 
  geom_bar(stat = "identity", fill = "light blue") +
  labs(title= "xbox retweet count",
       x = "days") 

# load some packages that we will use
for (i in c('SnowballC','slam','tm','Matrix','tidytext','dplyr','hunspell','purrr')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}


# comments <- XboxRetweets$text
#  Remove punctuation and numbers with regular expressions
comments <- mutate(XboxRetweets, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

#  Tokenization (+ going to lowercase)
commentsTokenized <- comments %>% unnest_tokens(output = "word", # how should the new column be named?
                                                  input = text, # where can we find the text? 
                                                  token = "words", # which tokenization scheme should we follow?
                                                  drop=FALSE,to_lower=TRUE) # drop=FALSE specifies that we want to keep our text; to_lower puts everyting to lowercase

if (!require("textdata")) install.packages("textdata") ; library("textdata")
?get_sentiments

# They all have some specificities (e.g., AFINN ranges from -5 to 5, bing is just positive or negative, nrc contains a wider range of emotions...)
# Also, they have different words included:
# so depending on the dictionary used, the results can look somewhat different

get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment)

get_sentiments("bing") %>% 
  count(sentiment)

commentsSentiment <- inner_join(commentsTokenized,get_sentiments("bing"))

oxfamTokenized <- commentsTokenized %>% group_by(id) %>%
  mutate(prevword = lag(x=word,n=1))%>%
  ungroup()

library(stopwords)
negationwords <- get_stopwords()[c(81:98,165:167),"word"]

a <- get_stopwords()
oxfamSentiment <- commentsTokenized %>% inner_join(get_sentiments("afinn")) %>%
  mutate(correct_sentiment = ifelse(prevword %in% negationwords$word, -value, value)) %>%
  group_by(id)%>%
  summarize (Sentiment = sum(correct_sentiment))

mean(oxfamSentiment$Sentiment)





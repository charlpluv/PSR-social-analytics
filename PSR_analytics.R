# Libraries --------------------------------------------------------

library(rtweet)
library(data.table)
library(data.table)
library(quanteda)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidytext)
library(textdata)
library(data.table)

library("syuzhet")
library("RColorBrewer")
library("tm")
library("topicmodels")
library("dendextend")
library("vosonSML")
library("urltools")
library("formattable")
library("stringr")
library(fmsb)
library(readxl)
boris_tweets <- read_excel("boris_tweets.xlsx")
View(boris_tweets)

# Downloading tweets and cleaning to create dataset--------------------------------------------------------


query = "Meta OR Facebook"
number.of.tweets = 18000

tweets.df = search_tweets(
  query,
  n = number.of.tweets,
  type = "recent",
  include_rts = FALSE, #No retweets, only original tweets!
  geocode = NULL,
  max_id = tail(tweets.df$status_id,1),
  parse = TRUE,
  token = NULL,
  retryonratelimit = FALSE,
  verbose = TRUE,
  tweet_mode = "extended",
  lang= "en"# get 240 character tweets in full
)

print(head(tweets.df2$text,n=2)) #Check if the dataframes overlap
print(tail(tweets.df1$text,n=2))

Qanon = rbind(tweets.df1, tweets.df2,tweets.df3, tweets.df4, tweets.df5) #, tweets.df4, tweets.df5, tweets.df6, tweets.df7, tweets.df8, tweets.df9, tweets.df10, tweets.df11, tweets.df12, tweets.df13, tweets.df14, tweets.df15,tweets.df16,tweets.df17,tweets.df18,tweets.df19,tweets.df20) # Merge all the dataframes

Qanon_clean = Qanon[!duplicated(Qanon$text),] # remove duplicate

setDT(Qanon_clean)
#write.csv(Qanon_clean,file ="meta.csv")

#library("writexl")
#write_xlsx(Qanon_clean,"meta.xlsx")


# Exploratory Analysis --------------------------------------------------------

sum(Qanon$lang == "en") #Number of tweets in english 

i0 = Qanon_clean[,.(.N), by = .(country)] [order(-N)] #Frequencies countries
i0$N = (i0$N / 207407)
i0$N = sprintf("%.3f %%", 100*i0$N)
i0 = head(i0, !n=1)
formattable(head(i0, n=10))

i1 = Qanon_clean[,.(.N), by = .(lang)] [order(-N)] #Frequencies lang           ++
i1$N = (i1$N / 207407)
i1$N = sprintf("%.1f %%", 100*i1$N)
formattable(head(i1, n=10))

# Total Tweets and reactions by language
Qanon_clean[,.(TotalTweets = .N, 
               total_reactions=sum(retweet_count, na.rm = TRUE) + 
                 sum(favorite_count, na.rm = TRUE)+
                 sum(reply_count, na.rm = TRUE)+
                 sum(quote_count, na.rm = TRUE)), 
            by = .(lang)] [order(-total_reactions)]


#Scatterplot Potential reach of tweets over time --
ggplot(Qanon_clean, aes(x=created_at, y=(friends_count+1))) +
  geom_point() +
  scale_x_datetime(name = "Time") +
  scale_y_log10(name = "Potentia Reach", breaks = c(10,100,1000,10000) ) +
  theme_minimal()


#Histogram tweets activity over time ++
ggplot(Qanon_clean, aes(x=created_at)) +
  geom_histogram(aes(y=..count..), #make histogram
                 binwidth=60, #each bar contains number of tweets during 60 s
                 colour="blue", #colour of frame of bars
                 fill="blue", #fill colour for bars
                 alpha=0.8) + # bars are semi transparant
  ggtitle(paste0("Activity ",dim(Qanon_clean)[1]," tweets")) + #title
  scale_y_continuous(name="Number of Tweets per minute") + 
  scale_x_datetime(name = "Time") +
  theme_minimal(base_family="Times New Roman")

#Frequency of tweets activity over time 2 ++

ts_plot(Qanon_clean, "hours") +
  labs(x = NULL, y = NULL,
       title = "Frequency of tweets with a Qanon related hashtag",
       subtitle = paste0(format(min(Qanon_clean$created_at), "%d %B %Y"), " to ", format(max(Qanon_clean$created_at),"%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet") +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") + 
  theme_minimal()  



#Scatterplot Potential reach of tweets over time w/ retweets
ggplot(Qanon_clean, aes(
  x=created_at, 
  y=(friends_count+1), 
  size = favorite_count + reply_count + quote_count + retweet_count )
) +
  geom_point(aes(size = retweet_count), alpha = 0.5) +
  ggtitle(paste0("Each dot is a tweet matching '","Qanon dog whistles","'")) +
  scale_y_log10(name="Potential Reach",breaks = c(10,100,1000,10000) ) +
  scale_x_datetime(name = "Time") +
  scale_size_continuous(name="Retweets") +
  theme_minimal()

#Top tweeting location
Qanon_clean %>% 
  filter(!is.na(place_full_name)) %>% 
  count(place_full_name, sort = TRUE) %>% 
  top_n(10)

#Frequent shared link 
Qanon_clean %>% 
  filter(!is.na(urls_expanded_url)) %>% 
  count(urls_expanded_url, sort = TRUE) %>% 
  top_n(10)
dom <- function(x) strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]
sapply(list(Qanon_clean), dom)

tweets %>%
  mutate(emoji = ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE) %>%
  top_n(10)

# Top tweeters
i0 = Qanon_clean %>% 
  count(screen_name, sort = TRUE) %>%
  top_n(10) %>%
  mutate(screen_name = paste0("@", screen_name))
i0$Percentage = (i0$n / dim(Qanon_clean)[1])
i0$Percentage = sprintf("%.1f %%", 100*i0$Percentage)
formattable(i0)

#in EN twitter
i0 = Qanon_clean %>% 
  count(screen_name, sort = TRUE) %>%
  top_n(10) %>%
  mutate(screen_name = paste0("@", screen_name))
i0$Percentage = (i0$n / dim(Qanon_clean)[1])
i0$Percentage = sprintf("%.1f %%", 100*i0$Percentage)
formattable(i0)



#Top Hashtags
Qanon_clean %>% 
  unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(hashtag, "^#"),
         hashtag != "#Qanon") %>%
  count(hashtag, sort = TRUE) %>%
  top_n(30)


#Top mentions
i0 = Qanon_clean %>% 
  unnest_tokens(mentions, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(mentions, "^@")) %>%  
  count(mentions, sort = TRUE) %>%
  top_n(10)
formattable(i0)


# Tokens --------------------------------------------------------

tok_tweets <- Qanon_clean$text %>% 
  gsub("#","", . ) %>% 
  corpus %>% 
  tokens(what="word",
         remove_numbers=TRUE,
         remove_punct=TRUE,
         remove_symbols=TRUE,
         remove_separators=TRUE,
         remove_url=TRUE)
tok_tweets <- tokens_remove(tok_tweets,stopwords(language = "en"))

head(tok_tweets,n=2)

# Word Frequencies --------------------------------------------------------

#remove stop words in english and japanese + stem + punct + url
words.to.remove <- c(stopwords("english"), "just", "like", "#maga", "amp","hat")
Qanon_cleanat_corp_twitter <- Qanon_clean$text %>% corpus() %>% 
  Qanon_clean(remove = words.to.remove,
      what = "word",
      stem = TRUE, 
      remove_punct = TRUE,
      remove_url=TRUE)

Qanon_cleanat_corp_hashtag <- Qanon_clean$hashtags %>% as.character() %>% corpus() %>% 
  Qanon_clean(remove = c(words.to.remove, "u", "+", ">", "<","0627"),
      what = "word",
      remove_punct = TRUE,
      remove_url=TRUE)
#Top 25 tokens ++
dfFreq <- textstat_frequency(Qanon_cleanat_corp_twitter) %>% as.data.table
ggplot(dfFreq[1:25,], aes(x=reorder(feature, -rank), y=frequency)) + 
  geom_col() +
  coord_flip() +
  labs(x = "Stemmed word", y = "Count") +
  theme_minimal(base_family="Times New Roman")


Qanon_clean[grepl("nft",text), list(text) ]  # Example of tweets w/ this word

dfFreq_long_top20 = dfFreq[rank <= 20] %>% 
  melt(id.vars = c("feature","group","rank"),
       measure.vars = c("frequency","docfreq")
  )
ggplot(dfFreq_long_top20, aes(x=reorder(feature,-rank), y=value, fill = variable)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_x_discrete() + 
  labs(x = "", y = "Occurances", fill = "") +
  coord_flip() +
  theme_minimal()

# Wordcloud --------------------------------------------------------

textplot_wordcloud(Qanon_cleanat_corp_twitter, min_count = 6, random_order = FALSE,
                   rotation = .25,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))
# Tokens 2-grams, 3grams --------------------------------------------------------

#2-grams ++
TokensStemmed <- tokens_remove(tok_tweets, words.to.remove)

Qanon_clean2 <- Qanon_clean(tokens_ngrams(TokensStemmed,n=2))

dfFreq2 <- textstat_frequency(Qanon_clean2)


ggplot(dfFreq2[1:20,], aes(x=reorder(feature, frequency), y=frequency)) + 
  geom_col() +
  coord_flip() +
  scale_x_discrete(name = "2 gram") +
  theme(text=element_text(size=12, family="Times New Roman"))

#3-grams ++
Qanon_clean3 <- Qanon_clean(tokens_ngrams(TokensStemmed,n=3))

dfFreq3 <- textstat_frequency(Qanon_clean3)

ggplot(dfFreq3[1:40,], aes(x=reorder(feature, frequency), y=frequency)) + 
  geom_col() +
  coord_flip() +
  scale_x_discrete(name = "3 gram") +
  theme(text=element_text(size=12, family="Times New Roman"))

# Topic Modelling --------------------------------------------------------

dtm <- convert(Qanon_cleanat_corp_twitter, to = "topicmodels")
lda <- LDA(dtm, k = 6, control=list(seed=12))

terms(lda, 8) %>% utf8::utf8_print()

topicAssignment = 
  data.table(
    index = lda %>% 
      topics %>% 
      names %>% 
      gsub("text","", .) 
    %>% as.integer,
    topic = lda %>% topics
  )
topicAssignment %>% head(6)


Qanon_clean$Topic = NA # creates a new col 'topic', assign it to NA
Qanon_clean$Topic[topicAssignment$index] = topicAssignment$topic

Qanon_clean$Topic = Qanon_clean$Topic %>% as.factor




ggplot(Qanon_clean, aes(x=created_at, y=Topic, col=Topic)) +
  geom_jitter(aes(size = retweet_count)) +
  ggtitle(paste0("Each dot is a tweet matching Qanon Keywords")) +
  scale_y_discrete() +
  scale_x_datetime(name = "") + 
  scale_color_discrete(guide = FALSE) + 
  scale_size_continuous(name="Retweets")

Qanon_clean[,list(Total.Retweets = sum(retweet_count)),by=Topic] %>% 
  ggplot(aes(x = Topic, y = Total.Retweets)) + ggtitle("Retweets by Topic") +
  geom_col()

Qanon_clean[!is.na(Topic), #++
         list(
           TotalTweets = .N, 
           TotalReactions=sum(retweet_count, na.rm = TRUE) + 
             sum(favorite_count, na.rm = TRUE)+
             sum(reply_count, na.rm = TRUE)+
             sum(quote_count, na.rm = TRUE),
           Reach = sum(followers_count)/10000
         ), 
         by = Topic] %>% 
  melt(id.vars = "Topic") %>% 
  ggplot(aes(x = Topic, y = value, fill=variable)) +
  geom_bar(position="dodge", stat="identity") + ggtitle("Tweets and Reactions by Topics") + 
  scale_fill_discrete(name= "", breaks=c("TotalTweets","TotalReactions","Reach"), labels = c("Tweets","Reactions","Reach in 10,000s")) + 
  scale_y_continuous(name = "Count")

# Bi-grams

Qanon_clean2 <- convert(Qanon_clean2, to = "topicmodels")
lda2 <- LDA(Qanon_clean2, k = 6, control=list(seed=123))
terms(lda2, 8)

topicAssignment2grams = 
  data.table(
    index = lda2 %>% 
      topics %>% 
      names %>% 
      gsub("text","", .) 
    %>% as.integer,
    topic = lda2 %>% topics
  )
Qanon_clean$Topic2gram = NA # creates a new col 'topic', assign it to NA
Qanon_clean$Topic2gram[topicAssignment2grams$index] = topicAssignment2grams$topic
Qanon_clean$Topic2gram = Qanon_clean$Topic2gram %>% as.factor

ggplot(Qanon_clean, aes(x=created_at, y=Topic2gram, col=Topic2gram)) +
  geom_jitter(aes(size = retweet_count)) +
  ggtitle(paste0("Each dot is a tweet matching '",query,"'")) +
  scale_y_discrete() +
  scale_x_datetime(name = "") + 
  scale_color_discrete(guide = FALSE) + 
  scale_size_continuous(name="Retweets")


#Topics overlap 

noOfTopics1gram = Qanon_clean$Topic %>% levels %>% length
noOfTopics2gram = Qanon_clean$Topic2gram %>% levels %>% length
topics1gram = matrix(0, nrow = dim(Qanon_clean)[1], ncol = noOfTopics1gram)
colnames(topics1gram) = paste("Topic",1:noOfTopics1gram)
topics2gram = matrix(0, nrow = dim(Qanon_clean)[1], ncol = noOfTopics2gram)
colnames(topics2gram) = paste("Topic",1:noOfTopics2gram)
for (i in 1:noOfTopics1gram) {
  topics1gram[,i] = as.integer(Qanon_clean$Topic == i)
}
for (i in 1:noOfTopics2gram) {   
  topics2gram[,i] = as.integer(Qanon_clean$Topic2gram == i)
}
topics1gram[is.na(topics1gram)] = 0
topics2gram[is.na(topics2gram)] = 0

diffMatrix = matrix(NA,nrow = noOfTopics1gram, ncol = noOfTopics2gram )
for (i in 1:noOfTopics1gram) {
  for (j in 1:noOfTopics2gram) {
    diffMatrix[i,j] = 
      sum(topics1gram[,i]!=topics2gram[,j])
  }
}
rownames(diffMatrix) = paste("1gram Topic",1:noOfTopics1gram)
colnames(diffMatrix) = paste("2gram Topic",1:noOfTopics2gram)

diffMatrix

#Inspect Topic 1
Qanon_clean[Topic==6][1:10,.(text)]
ggplot(Qanon_clean[Topic==6], aes(x = followers_count)) + geom_histogram(binwidth = 30) + xlim(c(0,300))

ggplot(Qanon_clean[Topic==6], aes(x = account_created_at)) + #Histo created date
  geom_histogram()  ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

dim(Qanon_clean[Topic==6])[1] #Nbr tweets in topic 1

#What hashtag is used in the topic ? ++
Qanon_clean[Topic==6, .(hashtags)] %>% 
  unlist %>% table %>% 
  sort(decreasing = T)

tweets.excl.Topic1 = Qanon_clean[Topic !=1]#Topics not related to Topic 1
dim(tweets.excl.Topic1)

#Most common words in each topic ++
tweet_topics <- tidy(lda, matrix = "beta") %>% as.data.table

tweet_topics[order(-beta),.SD[1:3],by = topic][order(topic)]

tweet_topics[order(-beta),.SD[1:10],by = topic] %>% 
  ggplot(aes(x = reorder_within(term,beta,topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_x_reordered() + 
  coord_flip() + 
  theme_minimal()

# Sentiment Analysis --------------------------------------------------------
TextDoc <- Corpus(VectorSource(Qanon_clean$text))

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# custom stopwords as a character vector
TextDoc <- tm_map(TextDoc, removeWords, c("https","cho","tco","maga","just")) 
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)


# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 5)

# Plot the most frequent words
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")

syuzhet_vector <- get_sentiment(Qanon_clean$text, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)


# Bing lexicon :  It categorizes words in a binary fashion into positive and negative categories
bing_vector <- get_sentiment(Qanon_clean$text, method="bing")
head(bing_vector)
summary(bing_vector)

# Affin lexicon : The AFINN lexicon assigns words with a score that runs between -5 and 5
afinn_vector <- get_sentiment(Qanon_clean$text, method="afinn")
head(afinn_vector)
summary(afinn_vector)

rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)

d<-get_nrc_sentiment(Qanon_clean$text)  # The nrc lexicon categorizes words in a binary fashion (“yes”/“no”) into categories of positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)

#transpose
td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]


# Barplot of the Sentiment Analysis using the NRC lexicon
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  col = brewer.pal(8, "Spectral"),
  las = 1, 
  main = "Emotions in Ice Cream reviews", xlab="Percentage"
)

# Top 10 negative & positive words

library(janeaustenr)
tidy_books2 <- Qanon_clean %>%
  group_by(text) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

bing_word_counts2 <- tidy_books2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts2 %>%
  group_by(sentiment) %>%
  slice_max(n, n = 20) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

# Sentiment Analysis 2 --------------------------------------------------------

TextDoc <- Corpus(VectorSource(Qanon_en$text))

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# custom stopwords as a character vector
TextDoc <- tm_map(TextDoc, removeWords, c("https","cho","tco","maga","just")) 
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)


# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 20)

# Plot the most frequent words
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")

# Find associations 
findAssocs(TextDoc_dtm, terms = c("biden","trump"), corlimit = 0.25)

syuzhet_vector <- get_sentiment(Qanon_en$text, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)

# bing
bing_vector <- get_sentiment(Qanon_en$text, method="bing")
head(bing_vector)
summary(bing_vector)
#affin
afinn_vector <- get_sentiment(Qanon_en$text, method="afinn")
head(afinn_vector)
summary(afinn_vector)

rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)

d<-get_nrc_sentiment(Qanon_en$text)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)

#transpose
td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

#Plot two - count of words associated with each sentiment, expressed as a percentage
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  col = brewer.pal(8, "Dark2"),
  las = 1, 
  main = "Emotions in EN Tweets", xlab="Percentage"
)

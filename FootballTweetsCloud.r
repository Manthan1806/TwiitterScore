#install.packages(c("twitteR","ROAuth","base64enc","httpuv","tm","SnowballC","wordcloud","RColorBrewer"))

#library(twitteR)
#library(ROAuth)
#library(base64enc)
#library(httpuv)
#library(tm)
#library(SnowballC)
#library(wordcloud)
#library(RColorBrewer)

cred <- OAuthFactory$new(consumerKey='RNn3HnE717eeeDZSu5o5K4ZLm',
                         consumerSecret='fpnNhzOyqO6UBMuP60d3jybSE3NZCMihaJqZ9KzPNY9syhUaYz',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

setup_twitter_oauth("RNn3HnE717eeeDZSu5o5K4ZLm",
                    "fpnNhzOyqO6UBMuP60d3jybSE3NZCMihaJqZ9KzPNY9syhUaYz",
                    "1142077041455452161-Lb4b6v3daRTUZL5bh1Ub8eYOOnEQnY",
                    "4AoZdjj2axcsWeIUdwneUZ2FBDv4gBKjy9jVSlQVUaTBy")

match_tweets = searchTwitter('football', n=1500, lang="en", resultType = "recent")
class(match_tweets)
str(match_tweets)
match_tweets[1:10]

match_text = sapply(match_tweets, function(x) x$getText())
match_text[1]

myCorpus = Corpus(VectorSource(match_text))
myCorpus
inspect(myCorpus[1])

myCorpus = tm_map(myCorpus, content_transformer(gsub), pattern="\\W", replace=" ")

removeURL <- function(x) gsub("http[^[:space:]]*","",x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

myCorpus <- tm_map(myCorpus, content_transformer(tolower))

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

myCorpus <- tm_map(myCorpus, removePunctuation)

myCorpus <- tm_map(myCorpus, removeNumbers)

myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))

myCorpus <- tm_map(myCorpus, stripWhitespace)

dtm <- TermDocumentMatrix(myCorpus)

m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words = 100, random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))
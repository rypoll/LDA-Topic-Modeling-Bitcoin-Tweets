library(tm) #to process text
library(topicmodels)
library(dplyr)
library(tidytext)
library(tidyverse)
library(SnowballC) # for stemming
library(stringr)
library(ldatuning)
library(gutenbergr)
library(wordcloud)

data_btc <- read.csv(file = 'data/00 - Cleaned Text.csv')
# data2017 <- read.csv(file = 'data/2017_ryan_data.csv')
# data2018 <- read.csv(file = 'data/2018_ryan_data.csv')
# data2019 <- read.csv(file = 'data/2019_ryan_data.csv')
# data2020 <- read.csv(file = 'data/2020_ryan_data.csv')
# head(data2016)
# 
# data <- rbind(data2016, data2017, data2018, data2019, data2020)


###################################################take a sample of the data
#print(nrow(data))
#data <- data %>% sample_frac(.1)
#print(nrow(data))
###################################################finish sampling




#data$text1 <- tolower(gsub('[[:punct:]0-9]', ' ', data$text))
#data_btc <- dplyr::filter(data, grepl('btc|bitcoin', text1))
#data_btc <- dplyr::filter(data_btc, !grepl('Current price|current price|current rate|Current rate|price update|price action|price increase|price decrease|density|volume|last hour|latest block info|closed sell|alert', text1))

#data_btc <- data_btc %>% mutate_all(~gsub("btc|bitcoin|cryptocurrency|crypto|coin", "", .))

#install.packages("ldatuning")
library("ldatuning")


library(wordcloud)

myCorpus <- Corpus(VectorSource(data_btc$text4))
data_clean <- tm_map(myCorpus, stripWhitespace)
data_clean <- tm_map(data_clean, removeNumbers)
data_clean <- tm_map(data_clean, removePunctuation)
data_clean <- tm_map(data_clean, tolower)
stopW <- stopwords()
data_clean <- tm_map(data_clean, removeWords, stopW)
#data_clean <- tm_map(data_clean, PlainTextDocument)




## You need to use VectorSource before using Corpus






#myCorpus1 <- tm_map(myCorpus,  removeWords, stopwords("english"))
#tdm <- TermDocumentMatrix(myCorpus)
dtm <- DocumentTermMatrix(data_clean)
dtm2 <- removeSparseTerms(dtm, sparse = 0.9999999)
dtm2

dtm_freq <- sort(colSums(as.matrix(dtm2)), decreasing = T)
dataframe <- data.frame(word = names(dtm_freq), freq = dtm_freq)
head(dataframe)

wordcloud(dataframe$word, dataframe$freq, 
          random.order = FALSE, 
          random.color = TRUE, 
          max.words = 100,
          vfont=c("serif","plain")
)

head(dtm_freq)



result <- FindTopicsNumber(
  dtm2,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

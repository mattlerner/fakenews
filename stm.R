## Example code for the STM

#install.packages("stm")
#install.packages("devtools")
# if(!require(devtools)) install.packages("devtools")
# library(devtools)
# install_github("bstewart/stm",dependencies=TRUE)
# library(stringr)

library(stm)
library(tidyverse)
library(data.table)
#library(quanteda)
#library(NMF)

setwd("/users/matt/desktop/qmss/data science and public policy/fakenews")

########## Load Buzzfeed datasets ##########

base::load("Data/Cleaned_DSPP/news_articles_buzzfeed_fake.rdata")
news_articles_buzzfeed_fake$fake = 1
base::load("Data/Cleaned_DSPP/news_articles_buzzfeed_real.rdata")
news_articles_buzzfeed_real$fake  = 0

########## CLEAN UP Buzzfeed datasets ##########

news_articles_buzzfeed_fake <- news_articles_buzzfeed_fake %>% subset(!is.na(title)) %>% distinct(title, .keep_all=TRUE)
news_articles_buzzfeed_real <- news_articles_buzzfeed_real %>% subset(!is.na(title)) %>% distinct(title, .keep_all=TRUE)
  
buzzfeed = rbind(news_articles_buzzfeed_fake, news_articles_buzzfeed_real)
buzzfeed$cleandate <- base::as.POSIXct(as.numeric(buzzfeed$date), origin='1970-01-01', optional=TRUE)
buzzfeed <- buzzfeed %>% arrange(cleandate)
  
########## Fake Buzzfeed ##########

# Process texts
processed_fake <- textProcessor(documents=news_articles_buzzfeed_fake$text, metadata = news_articles_buzzfeed_fake)
out_fake <- prepDocuments(processed_fake$documents, processed_fake$vocab, processed_fake$meta) 
docs_fake <- out_fake$documents
vocab_fake <- out_fake$vocab
meta_fake <- out_fake$meta

# search between 3 and 12 topics
#buzzfeed_fake_stm_search <- searchK(out_fake$documents, out_fake$vocab, K = seq(3, 12, 1))
#plot(buzzfeed_fake_stm_search)

# predefined number of topics based on results of searchK (above)
num_topics_fake <- 4

# Estimate the Structural Topic Model
buzzfeed_stm_fake <- stm(documents = out_fake$documents, vocab = out_fake$vocab, K = num_topics_fake, init.type = "LDA")

# most identifying words for each topic
top_words_fake <- labelTopics(buzzfeed_stm_fake, n=10)
frex_words_fake <- top_words_fake$frex

# original content
sample_topic_docs_fake <- findThoughts(buzzfeed_stm_fake, texts = meta_fake$text, n=10, thresh=0.5)

topics_titles_fake <- c()
for (topic in ls(sample_topic_docs_fake$index)) {
  sample_topic_docs_titles <- c(topic,meta_fake[unlist(sample_topic_docs_fake$index[topic]),'title'])
  topics_titles_fake <- rbind(topics_titles_fake, sample_topic_docs_titles)
}
topics_titles_fake_frame <- as.data.frame(topics_titles_fake)
fakethoughts <- melt(topics_titles_fake_frame, id.vars='V1') %>% arrange(V1) %>% select(-variable)



########## Real Buzzfeed ##########

# Process texts
processed_real <- textProcessor(documents=news_articles_buzzfeed_real$text, metadata = news_articles_buzzfeed_real, customstopwords = c('trump','hillary'))
out_real <- prepDocuments(processed_real$documents, processed_real$vocab, processed_real$meta) 
docs_real <- out_real$documents
vocab_real <- out_real$vocab
meta_real <- out_real$meta

# search between 3 and 12 topics
#buzzfeed_real_stm_search <- searchK(out_real$documents, out_real$vocab, K = seq(3, 12, 1))
#plot(buzzfeed_real_stm_search)

# predefined number of topics based on results of searchK (above)
num_topics_real <- 9

# Estimate the Structural Topic Model
buzzfeed_stm_real <- stm(documents = out_real$documents, vocab = out_real$vocab, K = num_topics_real, init.type = "LDA")

# most identifying words for each topic
top_words_real <- labelTopics(buzzfeed_stm_real, n=10)
frex_words_real <- top_words_real$frex

# original content
sample_topic_docs_real <- findThoughts(buzzfeed_stm_real, texts = meta_real$text, n=5)

topics_titles_real <- c()

for (topic in ls(sample_topic_docs_real$index)) {
  sample_topic_docs_titles <- c(topic,meta_real[unlist(sample_topic_docs_real$index[topic]),'title'])
  topics_titles_real <- rbind(topics_titles_real, sample_topic_docs_titles)
}

topics_titles_real_frame <- as.data.frame(topics_titles_real)
realthoughts <- melt(topics_titles_real_frame, id.vars='V1') %>% arrange(V1) %>% select(-variable)


########## MORE ##########


# fake all caps words
meta_fake$allcaps_text <- str_count(meta_fake$text, pattern="[A-Z][A-Z]+([:punct:]| )")
meta_fake$allcaps_title <- str_count(meta_fake$title, pattern="[A-Z][A-Z]+([:punct:]| )")

# real all caps words
meta_real$allcaps_text <- str_count(meta_real$text, pattern="[A-Z][A-Z]+([:punct:]| )")
meta_real$allcaps_title <- str_count(meta_real$title, pattern="[A-Z][A-Z]+([:punct:]| )")

# fake punctuation
meta_fake$text_punctuation <- str_count(meta_fake$text, pattern="[?!]")
meta_fake$title_punctuation <- str_count(meta_fake$title, pattern="[?!]")

# freal punctuation
meta_real$text_punctuation <- str_count(meta_real$text, pattern="[?!]")
meta_real$title_punctuation <- str_count(meta_real$title, pattern="[?!]")


########## OTHER METHODS ##########

real_corpus <- corpus(meta_real$text)
real_features <- dfm(real_corpus, remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
real_features_tfidf = dfm_tfidf(real_features, scheme_tf = 'prop')
a <- nmf(as.matrix(real_features_tfidf), 5)
basis <- basis(a)
coefs <- coef(a)
words <- colnames(coefs)

for (i in 1:dim(coefs)[1]){
  row <- coefs[i,]
  top_words <- names(tail(sort(row),5))
  print(top_words)
}

# align extracted features with vocab
terms = matrix(,nrow=4,ncol=5)
for (i in 1:length(extracted)) {
  doc <- extracted[[i]]
  for (j in 1:length(doc)) {
    terms[i,j] = vocab_real[doc[j]]
  }
}


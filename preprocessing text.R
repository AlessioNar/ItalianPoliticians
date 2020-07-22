install.packages("roxygen2")
library(roxygen2)

setwd("C://Users/aless/Desktop")
clean_timelines<- clean_tweets(timelines)

ita_timelines<- clean_timelines[clean_timelines$lang == "it",]
athousandtweets <- ita_timelines[1:1000,]

ita_lemmas<- prep_ita(textdata = athousandtweets, textvar = "text", groupvar = "screen_name",
                  node_type = "groups", pos = "words", language = "italian",
                  tokenizer = "words", is_scraped = TRUE)


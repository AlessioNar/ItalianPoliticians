install.packages("tidyRSS")

library(tidyRSS)
feed <- read.csv(file = "C:/Users/aless/Documents/GitHub/ItalianPoliticians/Databases/italian_feeds.csv")

all_feeds <- data.frame()
for (i in 1:nrow(feeds)){
  if(i %in% c(7,16,22,23,27,39)) {
  }
  else{
    temp_feed <- tidyRSS::tidyfeed(feeds$rss[i])  
    temp_feed$source <- feeds$testata[i]
    temp_feed <- data.frame(temp_feed$source, temp_feed$item_title, temp_feed$item_link, temp_feed$item_description,
                     temp_feed$item_pub_date)
    names(temp_feed) <- c("source", "title","link","description","date")
    all_feeds<- rbind(all_feeds, temp_feed)
  }
  
  all_feeds <- unique(all_feeds$title)
  print(i)
  Sys.sleep(60*30)
}


#problema: feed 7 (il giornale), feed 16 (espresso),feed 22 (il tempo), feed 23 (rai news), 
#feed 27 (linkiesta), feed 39 (il Sussidiario)

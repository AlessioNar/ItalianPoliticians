clean_tweets <- function (tweets){
  # remove info regarding tweets                          
  temp_text <- gsub("&amp;|&lt;|&gt;|RT", "", tweets$text)
  # remove format
  temp_text <- gsub("\\\n|\\\r|\\\t", "", temp_text)
  # remove link
  temp_text <- gsub("https://t.co/..*", "", temp_text)
  # remove emoji
  temp_text <- gsub("\\p{So}|\\p{Cn}", "", temp_text, perl = TRUE)
  
  temp_text <- gsub("\\s+", " ", temp_text, perl = TRUE)
  # remove final spaces 
  temp_text <- gsub("^\\s|\\s$", "", temp_text, perl = TRUE)
  
  tweets$text <- temp_text
  
  return(tweets)
}



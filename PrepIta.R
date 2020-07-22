Prep_ita <- function (textdata, groupvar, textvar, node_type = c("groups", 
                                                     "words"), tokenizer = c("words", "tweets"), 
          pos = c("all", "nouns"), language = "italian", 
          remove_stop_words = FALSE, remove_numbers = NULL, 
          udmodel_lang = NULL, is_scraped = NULL) 
{
    if(!is.null(is_scraped)){
    textvar <- iconv(textdata[[textvar]], from = "UTF-8", to = "UTF-8", 
                         sub = "")
  }
  else{
textvar <- iconv(textdata[[textvar]], from = "", to = "UTF-8", 
                               sub = "")
}
  if (tokenizer == "tweets") {
    
    if (!is.null(remove_numbers) && isTRUE(remove_numbers)) {
      textdata[[textvar]] <- gsub("\\b\\d+\\b", "", 
                                  textdata[[textvar]])
    }
  }
  if (is.null(udmodel_lang)) {
    ita_model <- "C:/Users/aless/Desktop/R/Models/italian-isdt-ud-2.4-190531.udpipe"
    udmodel_lang <- udpipe_load_model(file = ita_model)
  }
    
textdata_tokens <- as_tibble(testdata) %>% 
                         select(screen_name, text) %>% 
      tidytext::unnest_tokens(output = "word", input = text, 
                              token = "words")
    #annotate data using udmodel
    textdata_pos <- as.data.frame(udpipe_annotate(udmodel_lang, 
                                                  x = textdata_tokens$word, tagger = "default", 
                                                  parser = "none", tokenizer = "vertical"))
    #take upos and lemma from annotated data
    textdata <- bind_cols(textdata_tokens, textdata_pos[,c("upos", "lemma")])


  if (remove_stop_words == TRUE) {
    textdata <- {{textdata}} %>% 
                  anti_join(stopwords::get_stopwords(language = language), by = c(lemma = "word"))
  }
  if (length(pos) > 1) {
    warning(paste0("You did not specify `pos`. Function defaults to all parts of speech."))
    pos <- "all"
  }

  if (pos == "nouns") {
    textdata <- {{textdata}} %>% 
      filter(upos %in% c("NOUN", "PROPN"))
    }
  
if (length(node_type) > 1) {
    warning(paste0("You did not specify a `node_type`. Returned nodes are ", 
                   groupvar, "."))
    node_type <- "groups"
  }
  
if (node_type == "groups") {
    textdata <- {{textdata}} %>%
      group_by_({{groupvar}}) %>% 
      count(lemma) %>% 
      rename(count = n)
  }
  
if (node_type == "words") {
    textdata <- {{textdata}} %>% 
      group_by(lemma) %>% 
      count_({{groupvar}}) %>% 
      rename(count = n)
  }
  return({{textdata}})
}
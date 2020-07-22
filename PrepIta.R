Prep_ita <- function (textdata, groupvar, textvar, node_type = c("groups", 
                                                     "words"), tokenizer = c("words", "tweets"), 
          pos = c("all", "nouns"), language = "italian", 
          remove_stop_words = FALSE, remove_numbers = NULL, compound_nouns = FALSE, 
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
  if (isFALSE(compound_nouns)) {
    textdata_tokens <- as_tibble({{textdata}}) %>% 
                         select({{groupvar}}, {{textvar}}) %>% 
      tidytext::unnest_tokens(output = "word", input = {{textvar}}, 
                              token = {{tokenizer}}, ...)
    #annotate data using udmodel
    textdata_pos <- as.data.frame(udpipe_annotate(udmodel_lang, 
                                                  x = textdata_tokens$word, tagger = "default", 
                                                  parser = "none", tokenizer = "vertical"))
    
    textdata <- bind_cols(textdata_tokens, textdata_pos[,c("upos", "lemma")])
  }

  if (isTRUE(compound_nouns)) {
    
    textdata_tokens <- as_tibble({{textdata}}) %>% 
      select({{groupvar}}, {{textvar}}) %>% 
      unnest_tokens(output = "word", input = {{textvar}}, 
                    token = {{tokenizer}}, strip_punct = FALSE, ...)
    
    textdata_tokens <- textdata_tokens %>% group_by_({{groupvar}}) %>% 
      summarise(documents = paste(word, collapse = "\n"))
    
    textdata_dep <- as.data.frame(udpipe_annotate(udmodel_lang, 
                                                  x = textdata_tokens$documents, doc_id = textdata_tokens[[groupvar]], 
                                                  tagger = "default", parser = "default"))
    
    noun_compound <- which(textdata_dep$dep_rel == "compound")
    
    compound_elements <- split(noun_compound, cumsum(c(1, diff(noun_compound) != 1)))
    
    compound_bases <- mapply(`[[`, compound_elements, 
                             lengths(compound_elements)) + 1
    
    all_compound_elements <- mapply(c, compound_elements, 
                                    compound_bases, SIMPLIFY = FALSE)
    
    compound_nouns <- sapply(all_compound_elements, function(x) paste0(textdata_dep$lemma[x], 
                                                                       collapse = " "))
    
    textdata_dep$lemma[compound_bases] <- compound_nouns
    
    textdata_dep <- textdata_dep %>% filter(dep_rel != "compound" & 
                                              upos != "PUNCT")
    textdata <- textdata_dep
    
    names(textdata)[1] <- groupvar
  }

  if (remove_stop_words == TRUE) {
    textdata <- {{textdata}} %>% 
                  anti_join(get_stopwords(language = language), by = c(lemma = "word"))
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
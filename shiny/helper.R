outcomes <- c("social_democracy", "islamism", "secularism", "anti_communism", 
              "communism", "marxism_leninism", "agrarianism", "liberalism", 
              "national_conservatism", "social_conservatism", "pro_europeanism", 
              "christian_democracy", "conservatism", "liberal_conservatism", 
              "democratic_socialism", "populism", "social_liberalism", "socialism", 
              "green_politics", "conservative_liberalism", "progressivism", 
              "republicanism", "economic_liberalism", "nationalism", "centrism", 
              "regionalism", "classical_liberalism", "euroscepticism", "right_wing_populism", 
              "left_wing_nationalism")


transform_text <- function(x, n_grams){
  text <- x %>% 
    tibble(text = .) %>% 
    mutate(id = 1:n()) %>%
    tidytext::unnest_tokens(word, text, token = "words", to_low = F) %>%
    group_by(id) %>% 
    mutate(tid = 1:n()) %>%
    ungroup 
  
  ngram <- text %>%
    group_by(id) %>%
    summarise(text_word = paste(word, collapse = " ")) %>%
    ungroup %>% 
    tidytext::unnest_tokens(ngram, text_word, token = "ngrams", n = as.integer(n_grams), to_low = F) %>%
    group_by(id) %>%
    mutate(tid = 1:n()) %>% 
    ungroup
  
  out <- text %>% 
    left_join(ngram) %>% 
    mutate(ngram = ifelse(is.na(ngram), word, ngram))
  
  return(out)
} 

transform_seq <- function(x, tokenizer, seq_len){
  new_char_seq <- tokenizer %>% 
    texts_to_sequences(texts = paste(x$word, collapse = " ")) %>% 
    pad_sequences(maxlen = seq_len, value = 0)
  return(list(data = x, seq = new_char_seq))
}

keras_predict <- function(container, model){
  
  probs <- predict(model, x = container$seq) %>% 
    as_tibble() %>% 
    purrr::set_names(outcomes)
  
  container$probs <- probs
  return(container)
}

source(here::here("R", "grade_similarity.R"))

# Randomly pick an answer, options. Answer is a word of length = nchar

setup_game = function(dict, n_char = 5){
  
  # filter dictionary for word length
  dict_nchar = dplyr::filter(dict, nchar(word) == n_char)
  # answer + options = 20 words
  
  # randomly choose answer
  answer = sample(dict_nchar$word, size = 1)
  
  # choose 19 options according to similarity
  random_order = sample(dict_nchar$word)
  dict_nchar$rank = sapply(dict_nchar$word, function(x) which(random_order == x))
  dict_nchar$sim = sapply(dict_nchar$word, function(x) grade_similarity(x, answer)) # similarity
  # dict_nchar sorted by similarity + some randomness
  dict_nchar_sorted = dict_nchar %>% 
    dplyr::arrange(dplyr::desc(sim), rank) %>%
    dplyr::filter(word != answer)
  sample_size = floor(0.05*nrow(dict_nchar_sorted))
  k = nrow(dict_nchar_sorted)
  # 6 words from bottom 5%
  bottom5 = sample(dict_nchar_sorted$word[1:sample_size], 6)
  # 6 words from top 5%
  dict_nchar_sorted = dplyr::filter(dict_nchar_sorted, !word %in% bottom5)
  k = nrow(dict_nchar_sorted)
  top5 = sample(dict_nchar_sorted$word[(k-sample_size):k], 6)
  # 7 words from middle
  dict_nchar_sorted = dplyr::filter(dict_nchar_sorted, !word %in% top5)
  k = nrow(dict_nchar_sorted)
  mid5 = sample(dict_nchar_sorted$word[(floor(k/2) - floor(sample_size/2)):(floor(k/2) + floor(sample_size/2))], 7) 
  # combine
  word_choices = c(bottom5, top5, mid5)
  
  if(any(is.na(word_choices))){
    browser()
  }
  
  game_components = list(
    "answer" = answer,
    "options" = word_choices
  )
  
  
  return(game_components)
}

# # NOT RUN 
# dict = readRDS(here::here("dict","sgb_words.rds"))
# game_components = setup_game(dict = dict)
# game_components$options

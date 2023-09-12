preprocess_guess = function(x){
  # helper function. take a guess 'x' and remove punctuation, extra whitespace,
  # remove the "> ", and return the value in UPPER CASE
  require(magrittr)
  guess = x %>%
    gsub("^>\\s+", "", .) %>% # Leading "> "
    gsub("\\s", "", .) %>%    # any spaces --> not allowed whitespace in Guess!
    gsub("[[:punct:]]", "", .) %>% # Not sure what [[:punct:]] encompasses; likely the > as well
    toupper(.)
  # Below is likely sufficient
  # toupper(gsub("[[:punct:]]|\\s", "", x))
  return(guess)
}
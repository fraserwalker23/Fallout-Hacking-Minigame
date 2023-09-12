library(testthat)

# Supply candidate 'dict' and check if a valid dictionary for our game.

dict_validate = function(dict){
  
  list_tests = list()
  
  # Class
  list_tests["dataframe"] = test_that("dict is a data.frame.",{
    expect_true(inherits(dict, "data.frame"))
  })
  
  # Column Names
  dict_colnames = colnames(dict)
  expected_cols = c("word" = "character")
  
  list_tests["colnames"] = test_that("dict has expected column names.",{
    expect_true(all(names(expected_cols) %in% dict_colnames)) 
  })
  
  # Data Types
  dict_datatypes = sapply(dict, class)
  
  list_tests["datatypes"] = test_that("expected column named have expected data types.", {
    expect_equal(dict_datatypes[names(expected_cols)], expected_cols)
  })
  
  # every record of 'word' is all lowercase!
  list_tests["lowercase"] = test_that("all words in dict are lower case.",{
    expect_equal(dict$word, tolower(dict$word))
  })
  
  # no words have punctuation!
  list_tests["no_punct"] = test_that("all words in dict do not contain punctuation!", {
    expect_false(any(grepl("[[:punct:]]", dict$word)))
  })
  
  # no duplicates!
  list_tests["unique"] = test_that("all words in dict are unique!", {
    expect_equal(
      length(dict$word),
      length(unique(dict$word))
    )
  })
  
  return(do.call("c", list_tests))
}

# # NOT RUN
# dict = readRDS(here::here("dict","sgb_words.rds"))
# dict_validate(dict)

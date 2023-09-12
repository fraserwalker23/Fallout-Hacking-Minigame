
# How a guess is scored -- how many letters+position does the guess have in common w/ answer

grade_similarity = function(guess, answer){
  # split up strings into individual characters
  guess_split = stringr::str_split_1(guess, pattern = "")
  answer_split = stringr::str_split_1(answer, pattern = "")
  # count correct char in correct position
  return(sum(guess_split == answer_split))
}


nonsense_chars = c("?","!","@","-","'",'"',"#","(",")","[","]","{","}","*","$","%","?","<",">","^","+","_","=","|","\\","/",",",".",":",";","'")

escape_html_chars = function(x){
  # For a string x, to render the string correctly in HTML we need to escape choice chars: &,<,>:
  html_chars = c("&" = "&amp", "<" = "&lt", ">" = "&gt")
  return(stringr::str_replace_all(x, pattern = c(html_chars)))
}

setup_identifiers = function(n_rows = 17, n_cols = 2){
  
  # For the '0xF54C' style ids seen at the beginning of each line.
  
  # Rules: 6 chars long
  # char 1:3 always = 0xF
  # chart 4 is always a number
  # char 5,6 can be number or letter
  # Ids presented in alphabetical order: Numbers before Letters
  total_lines = n_rows*n_cols
  char_1_to_3 = c("0xF")
  # define a continuous 3,4 number range for char 4
  # define a random distribution between this range (how many for each)
  root_4 = sample(c(0:7), size = 1)
  char_4 = sample(root_4:(root_4 + sample(if(root_4==7) c(2) else c(2,3), size = 1)) %% 10, size = total_lines, replace = TRUE)
  # randomly sample the pairs for 5,6 --> MUST BE UNIQUE!
  # n choose 2, paste(), sample without replacement n = total_lines
  char_5_6 = sample(
    # only sampling among the first 6 letters: A:F
    combn(c(as.character(0:9), base::LETTERS[1:6]), m = 2, FUN = function(x) paste0(x, collapse = "")), 
    size = total_lines, replace = FALSE)
  # Paste together, sorted
  ids = sort(paste0(char_1_to_3, char_4, char_5_6))
  return(ids)
}

# NOT RUN
#setup_identifiers()

setup_terminal = function(word_choices, filler_chars = nonsense_chars, n_chars_per_line = 12, n_rows = 17, n_cols = 2){
  
  ## Rules:
  # two word choices cannot be side by side ever.
  
  total_chars = n_chars_per_line * n_rows * n_cols                # how many characters (letters, keystrokes) are we using
  total_lines = n_rows * n_cols                                   # how many "lines" are generating
  k = sum(nchar(word_choices))                                    # how many characters are devoted to the word choices
  j = total_chars - k                                             # remainder sample size for filler
  n_words = length(word_choices)                                  # how many words are there?
  sample_filler = sample(filler_chars, size = j, replace = TRUE)  # j filler characters
  
  
  
  
  # Must check if two words are side-by-side and permute adjacent letters if so
  # author's note: I do not claim this algo to be efficient
  iterations = 0
  while(TRUE){
    terminal_string = sample(x = c(word_choices, sample_filler))     # a contiguous string to be broken down
    # find placement in character vector
    word_choice_index_in_terminal_string = sort(sapply(word_choices, function(x) which(terminal_string==x)))
    # look for any pairs --> lead(x) - x = 1
    is_paired = tidyr::replace_na(dplyr::lead(word_choice_index_in_terminal_string) - word_choice_index_in_terminal_string == 1, FALSE)
    if(any(is_paired)){
      iterations = iterations + 1
      next
    }
    else{
      # print(iterations)
      break # good to go!
    }
  }
  
  # check: is the size as expected?
  if(!sum(nchar(terminal_string)) == total_chars){
    stop("Failed to construct correct size terminal string. Exiting.")
  }
  
  # generate Identifiers
  terminal_ids = setup_identifiers(n_cols = n_cols, n_rows = n_rows)
  
  # Compose into a single string, then create n_cols*n_rows lines of length = n_chars_per_line. Return in all Upper Case, prefix w/ terminal IDs
  terminal_string_pasted = paste0(terminal_string, collapse = "")
  line_index_range = sapply(c(1:(total_lines)), function(n) c((n_chars_per_line*(n-1)+1):(n_chars_per_line*n)), simplify = FALSE)
  terminal_lines = lapply(line_index_range, function(j) toupper(substr(terminal_string_pasted, start = min(j), stop = max(j))))
  
  # concatenate terminal_ids and terminal_lines
  terminal_lines = lapply(c(1:length(terminal_ids)), function(i) paste0(terminal_ids[i], " ", terminal_lines[i]))
  
  return(terminal_lines)
}

# # NOT RUN
# dict = readRDS(here::here("dict","sgb_words.rds"))
# game_components = setup_game(dict = dict)
# game_lines = setup_terminal(word_choices = c(game_components$options, game_components$answer))

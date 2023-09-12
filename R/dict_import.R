source(here::here("R", "dict_validate.R"))

# Read a dictionary of words from a file in "./dict" 

dict_import = function(file_name){
  dict_folder = here::here("dict")
  ext = tools::file_ext(file_name)
  if(file.exists(here::here(dict_folder, file_name))){
    if(ext != "rds"){
      stop(file_name, " file extension not supported.")
    }
    dict = readRDS(here::here(dict_folder, file_name))
    dict_validate_quietly = purrr::quietly(dict_validate) # do not print "Test Passed" messages
    invisible(dict_validate_quietly(dict)) # dict_validate() will stop execution if test not passed
    return(dict)
  }
  else{
    stop(file_name, " not found in ", dict_folder, ".")
  }
}

# # NOT RUN
# dict = dict_import("sgb_words.rds") # good
# dict = dict_import("this_does_not_exit.rds") # file not found
# dict = dict_import("raw/sgb_words.txt") # wrong file extension
# dict = dict_import("example_bad.rds") # fails validation

library(magrittr)
library(dplyr)
library(shiny)
library(shinyFiles)

source(here::here("R","dict_import.R"))
source(here::here("R","setup_game.R"))
source(here::here("R","setup_terminal.R"))
source(here::here("R", "preprocess_guess.R"))

dict = dict_import("sgb_words.rds")

# javascript to listen for an Enter press.
# https://stackoverflow.com/questions/31415301/shiny-responds-to-enter
js_press_enter = '
$(document).on("keyup", function(e) {
  if(e.keyCode == 13){
    Shiny.onInputChange("keyPressed", Math.random());
  }
});
'
                    
reset_code = "X"                    

ui = fluidPage(
  # css style sheet
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "computer_terminal.css")),
  # javascript to observe Enter presses
  tags$script(js_press_enter),
  fluidRow(
    # Main Panel:
    # Title
    # Attempts
    # Word Selection
    column(
      8,
      "ROBCO INDUSTRIES (TM) TERMLINK PROTOCOL", br(),
      "ENTER PASSWORD NOW", br(), br(),
      textOutput("attempts_left_display"), # Blocks to indicate number
      hr(),
      column(
        6,
        uiOutput("terminal_col_1")
      ),
      column(
        6,
        uiOutput("terminal_col_2")
      )
    ),
    column(
      4,
      uiOutput("console_log"),
      br(),
      textInput(inputId = "guess", label = NULL, value = ">")
    )
  )
)


server = function(input, output, session){
  
  
  # Setup the game (not reactive!)
  if(!exists("game_components")){
    game_components = setup_game(dict)
    terminal_lines = setup_terminal(word_choices = c(game_components$options, game_components$answer)) %>%
      lapply(., escape_html_chars)
    starting_attempts = 4
    ANSWER = toupper(game_components$answer)
    WORD_CHOICES = toupper(game_components$options)
  }
  
  
  
  # Initialize vars
  attempts_left = reactiveVal(value = starting_attempts)
  user_guess = reactiveVal(value = "")
  console_history = reactiveVal(value = "")
  
  ## Core Game Loop, Logic ##
  # Game state updates upon pressing Enter
  observeEvent(input$keyPressed, {
    # take the value of input$guess, preprocess, slot into user_guess
    user_guess(preprocess_guess(input$guess))
    
    # assess whether the game is still active
    if(attempts_left() == 0){
      if(user_guess() == reset_code){
        session$reload()
        return()
      }
      # placeholder code to stop allowing inputs after Game ends
      return()
    }
    
    updateTextInput(session, "guess", value = ">") # reset input$guess text box
    
    if(!user_guess() %in% c(WORD_CHOICES, ANSWER)){ # Do nothing if input not an option
      return()
    }
    
    # assess if correct
    if(user_guess() == ANSWER){
      # Correct Guess. 
      # Write console feedback, enter Win State.
      console_feedback = ("Exact match! Please wait while system is accessed.")
      attempts_left(0)
    }
    else{
      # Incorrect Guess.
      # Update attempts_left, compute similarity, write console feedback.
      prev_attempts = attempts_left() 
      attempts_left(prev_attempts - 1)
      if(attempts_left() == 0){
        # Enter Lose State
        console_feedback = c("Terminal Locked. Game Over.", paste0("ANSWER: ", ANSWER)) 
      }
      else{
        # Calculate similarity to ANSWER
        guess_sim = grade_similarity(user_guess(), ANSWER)
        console_feedback = c("Entry denied.", paste0(guess_sim,"/",nchar(ANSWER), " correct."))
      }
    }
    
    updated_console = paste0(
      console_history(),                                       # existing console log
      if(console_history() == ""){""} else{"<br/>"},           # line break except for first word 
      "&gt ", user_guess(), "<br/>",                           # > new guess
      paste0("&gt ", console_feedback, collapse = "<br/>")     # Console Feedback
      )
    console_history(updated_console)
  })
  
  # observe({
  #   print(user_guess())
  # })
  # 
  # observe({
  #   print(console_history())
  # })
  
  # UI elements to display to the player
  output$attempts_left_display = renderText({
    paste0(attempts_left(), " Attempt(s) Left: ", paste0(rep("*", attempts_left()), collapse = " "))
  })
  
  output$terminal_col_1 = renderUI({
    HTML(paste0(terminal_lines[1:17], collapse = "<br/>"))
  })
  
  output$terminal_col_2 = renderUI({
    HTML(paste0(terminal_lines[18:34], collapse = "<br/>"))
  })
  
  output$console_log = renderUI({
    HTML(console_history())
  })
}


#shinyApp(ui = ui, server = server)
runGadget(ui, server, viewer = dialogViewer("Fallout 3 Hacking Minigame", width = 932, height = 803))
#runGadget(ui, server, viewer = paneViewer(minHeight = 500))

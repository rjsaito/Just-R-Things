#packages
pacman::p_load(shiny, shinyjs, DT, dplyr, V8)

#declare rps game elements  
choices <- c("rock", "paper", "scissors")
results <- c("win","lose","tie")
#according to paper, people only play paper 30% of the time - https://flowingdata.com/2010/07/30/how-to-win-rock-paper-scissors-every-time/
human_priors <- c(.35, .3, .35)

#computer should play hands based on human move priors
comp_priors <- c(human_priors[3], human_priors[1:2])

#random move generator from prior
random_move <- function() sample(choices, 1, replace = T, prob = comp_priors) 

#outcome wrapper for play vs computer moves
oc <- function(x) {
  if(length(x) > 0){
    if(x == 0) "tie" else
      if(x %in% c(1, -2)) "win" else
        if(x %in% c(-1, 2)) "lose"
  }
}

#reactive values for results
seq1 <- merge(choices, results, by = NULL); names(seq1) = c("move","outcome")  
seq2 <- merge(seq1, seq1, by = NULL, suffixes = c(".1",".2")) 
seq3 <- merge(seq2, seq1, by = NULL); names(seq3)[5:6] = c("move.3", "outcome.3")


#server
shinyServer(function(input, output, session) {

  useShinyjs()
  
  #keep this tab hidden
  hide(selector = "#tab1 li a[data-value=Finish]")
  
  #reactive value for in-game data
  values <- reactiveValues(outcome = NULL, outcomes = NULL, move = NULL, computer = NULL)
  
  #reactive values for results
  dt.cur <- reactiveValues(
    dt1 = data.frame(seq1, N = 0),
    dt2 = data.frame(seq2, N = 0),
    dt3 = data.frame(seq3, N = 0)
  )
  
  #reactive trigger for game completed
  games <- reactiveValues(completed = 0)
  
  #read rds
  dt1in = readRDS("dt1.rds")
  dt2in = readRDS("dt2.rds")
  dt3in = readRDS("dt3.rds")
  dt1a <- reactive(dt.cur$dt1 %>% mutate(N = N + dt1in$N))
  dt2a <- reactive(dt.cur$dt2 %>% mutate(N = N + dt2in$N))
  dt3a <- reactive(dt.cur$dt3 %>% mutate(N = N + dt3in$N))
  
  #action buttons
  observeEvent(input$in_rock, values$move  <- "rock", priority = 2)
  observeEvent(input$in_scissors, values$move  <- "scissors", priority = 2)
  observeEvent(input$in_paper, values$move  <- "paper", priority = 2)
  
  #reactive when any of the three choices are clicked
  clicked <- reactive(c(input$in_rock, input$in_scissors, input$in_paper))
  
  #when click triggered, create and append move/outcome
  observeEvent(clicked(), {
    values$moves <- c(values$moves, values$move)
    values$computer <- if(any(clicked() > 0 )) random_move()
    values$computers <- c(values$computers, values$computer)
    values$outcome <- oc(which(choices == values$move) - which(choices == values$computer))
    values$outcomes <- c(values$outcomes, values$outcome)
  }, priority = 1)
  
  #player history
  history <- eventReactive(clicked(), 
    data.frame(
      round = rev(seq_len(length(values$moves))),
      player = rev(values$moves), 
      computer = rev(values$computers), 
      outcome = rev(values$outcomes)
    )
  )
  
  #reactive to round length (should happen after clicked)
  cur.round <- reactive(length(values$outcomes))
  
  #reactions to outcomes
  observeEvent(cur.round(), {
    
    if(cur.round() >= 1)
      dt.cur$dt1 <- mutate(dt.cur$dt1, N = ifelse(move == values$move & outcome == values$outcome, N+1, N))

    if(cur.round() >= 2)
      dt.cur$dt2 <- mutate(dt.cur$dt2, N = ifelse(move.1 == values$moves[cur.round()-1] & outcome.1 == values$outcomes[cur.round()-1] & move.2 == values$move & outcome.2 == values$outcome, N+1, N))
    
    if(cur.round() >= 3)
      dt.cur$dt3 <- mutate(dt.cur$dt3, N = ifelse(move.1 == values$moves[cur.round()-2] & outcome.1 == values$outcomes[cur.round()-2] & move.2 == values$moves[cur.round()-1] & outcome.2 == values$outcomes[cur.round()-1] & move.3 == values$move & outcome.3 == values$outcome, N+1, N))
   
  })
    
  #data table
  performance <- reactive(
    dt.cur$dt1 %>%
      group_by(move) %>%
      summarise(
          Played = sum(N), 
          Wins = N[outcome == "win"], 
          Losses = N[outcome == "lose"], 
          Ties = N[outcome == "tie"]
      ) %>%
      mutate(`% Win` = round(Wins/(Wins+Losses), 2)) 
  )
  
  #game completion trigger
  completed <- reactive(as.numeric(sum(values$outcomes == "win") >= 5 | sum(values$outcomes == "lose") >= 5))
  
  #save results when game completed
  observe(if(completed() == 1) {
    hide(selector = "#tab1 li a[data-value=Play]")
    show(selector = "#tab1 li a[data-value=Finish]")
    updateTabsetPanel(session, "tab1", selected = "Finish")
    saveRDS(dt1a(), file = "dt1.rds")
    saveRDS(dt2a(), file = "dt2.rds")
    saveRDS(dt3a(), file = "dt3.rds")
  }, priority = -1)
  
  
  #for UI
  #image functions
  blankImg <- function(h = 1, w = 1) {list(src = "images/blank.jpg", contentType = "image/jpeg", height = h, width = w)}
  
  rockImg <- function(h = 250, w = 250) {list(src = "images/rock.jpg", contentType = "image/jpeg", height = h, width = w)}
  
  scissorsImg <- function(h = 250, w = 250) {list(src = "images/scissors.jpg", contentType = "image/jpeg", height = h, width = w)}
  
  paperImg <- function(h = 250, w = 250) {list(src = "images/paper.jpg", contentType = "image/jpeg", height = h, width = w)}
  
  
    #player image, reactively changes with outcome
  playerImgOut <- reactive(
    if(is.null(values$outcome)){
      return(list(src = "images/3d_human_normal.jpg", contentType = "image/jpeg",
        height = 230, width = 200))
    } else
    if(values$outcome == "tie"){      
      return(list(src = "images/3d_human_normal.jpg", contentType = "image/jpeg",
        height = 230, width = 200))
    } else
    if(values$outcome == "win"){
      return(list(src = "images/3d_human_win1.jpg", contentType = "image/jpeg",
        height = 230, width = 200))
    } else 
    if(values$outcome == "lose"){
      return(list(src = "images/3d_human_lose1.jpg", contentType = "image/jpeg",
        height = 230, width = 200))
    }
  )   
  
  
  resultTab <- reactive(
    dt3a() %>%
      rename(`Move (t-2)` = move.1, `Outcome (t-2)` = outcome.1,
             `Move (t-1)` = move.2, `Outcome (t-1)` = outcome.2, 
             `Move (t)` = move.3, `Outcome (t)` = outcome.3) %>%
      data.frame(check.names = F) %>%
      group_by_(.dots = lapply(input$columns, as.symbol)) %>%
      summarise(N = sum(N))
  )
  
  
  #Outputs
  #tab "Play"
  
  output$rockButton <- renderImage(rockImg(),  deleteFile = FALSE)
  output$scissorsButton <- renderImage(scissorsImg(),  deleteFile = FALSE)
  output$paperButton <- renderImage(paperImg(),  deleteFile = FALSE)
  
  output$playerImage <- renderImage(playerImgOut(),  deleteFile = FALSE)
  
  #player move, reactively changes with outcome
  output$playerMove <- renderImage({
    if(all(clicked() == 0)) blankImg(1, 1) else
    if(values$move == "rock") rockImg(80, 80) else
    if(values$move == "scissors") scissorsImg(80, 80) else
    if(values$move == "paper") paperImg(80, 80)
  },  deleteFile = FALSE)
  
  #computer move, reactively changes with computer
  output$botMove <- renderImage({
    if(all(clicked() == 0)) blankImg(1, 1) else
    if(values$computer == "rock") rockImg(80, 80) else
    if(values$computer == "scissors") scissorsImg(80, 80) else
    if(values$computer == "paper") paperImg(80, 80)
  },  deleteFile = FALSE)
  
  # static bot image
  output$botImage <- renderImage({
    return(list(src = "images/bot_normal.png", contentType = "image/png",
      height = 230, width = 200))
  }, deleteFile = F)
  
  output$computer <- renderText(values$computer)
  output$result <- renderText(if(!is.null(values$outcome)) paste("You", ifelse(values$outcome == "tie", "tied", values$outcome)))
  output$ft <- renderDataTable(performance(), options = list(paging = F, searching = FALSE, autoWidth = T, columnDefs = list(list(width = '60px', targets = "_all"))), rownames = F)
  
  
  #tab "Finish"  
  #refresh game when reach end and button clicked
  observeEvent(input$refreshButton, js$reset())
  output$playerImageFinish <- renderImage(playerImgOut(),  deleteFile = FALSE)
  output$resultFinish <- renderText(if(!is.null(values$outcome)) paste("You", ifelse(values$outcome == "tie", "tied", values$outcome)))
  output$ftFinish <- renderDataTable(performance(), options = list(paging = F, searching = FALSE, autoWidth = T, columnDefs = list(list(width = '60px', targets = "_all"))), rownames = F)
  
  
  #tab "Data"
  output$resultTab = renderDataTable(datatable(resultTab(), filter = 'top', options = list(pageLength = 10, searching = FALSE, autoWidth = T, columnDefs = list(list(width = '70px', targets = "_all"))), rownames = F))
  
  output$hist <- renderDataTable(history(), options = list(pageLength = 20, searching = F), rownames = F)

 
})

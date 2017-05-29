library("shiny");
library("shinyjs");
library("shinythemes")
library("DT")
library("V8")


jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

shinyUI(navbarPage(theme = shinytheme("flatly"), "Rock Paper Scissors",
                   
  tabsetPanel(id = "tab1",
  
    tabPanel("Play",
      useShinyjs(),
    
      fluidRow(
        column(2, actionButton("in_rock", label = imageOutput('rockButton', height = "250px", width = "250px"))),
        column(2, actionButton("in_scissors", label = imageOutput('scissorsButton', height = "250px", width = "250px")), offset = 1),
        column(2, actionButton("in_paper", label = imageOutput('paperButton', height = "250px", width = "250px")), offset = 1)
      ),
      
      fluidRow(
        column(2, imageOutput("playerImage", width = "200px", height = "230px")),
        column(1, imageOutput("playerMove", width = "80px", height = "80px")),  
        column(3, h2("RPS - Game to 5"), textOutput("result"), align = "center"),
        column(1, imageOutput("botMove", width = "80px", height = "80px")),  
        column(2 ,imageOutput("botImage", width = "200px", height = "230px"))
      ),
      tags$style(type='text/css', "#playerMove { width:100%; margin-top: 60px;}"),
      tags$style(type='text/css', "#botMove { width:100%; margin-top: 60px;}"),
      tags$head(tags$style(
        "#result{color: red;
          font-size: 40px;
          font-style: bold;
          margin-top: 60px;
        }"
      )),
      
      fluidRow(
        column(5, dataTableOutput("ft"), offset = 2)
      )
    ),
    
    tabPanel("Finish",
      fluidRow(
        column(2, imageOutput("playerImageFinish", width = "200px", height = "230px"), offset = 3)
      ), 
      fluidRow(
        column(3, textOutput("resultFinish"), offset = 3),
        tags$head(tags$style(
          "#resultFinish{color: red;
          font-size: 60px;
          font-style: bold;
          margin-top: 20px;
        }"))
      ),
      fluidRow(
        column(2,
          align = "center",
          extendShinyjs(text = jsResetCode),  
          actionButton("refreshButton", "New Game"),
          offset = 3
        )
      ),
      fluidRow(
        column(3, h2("Game Statistics"), offset = 3)
      ),
      fluidRow(
        column(5, dataTableOutput("ftFinish"), offset = 1)
      )
    ),
    
    tabPanel("Data",
      fluidRow(
        column(2, 
          h3("All Historical Results"),     
          checkboxGroupInput("columns", "Columns to Group By",
            choices = c("Move (t-2)", "Outcome (t-2)", "Move (t-1)", "Outcome (t-1)", "Move (t)" ,"Outcome (t)")
, 
            selected = c("Move (t)", "Outcome (t)")) 
        ),
        column(4, dataTableOutput("resultTab"))
      ),     
             
      fluidRow(
        column(5, 
          h3("Your Moves"),    
          dataTableOutput("hist"))
      )

     )
  )
))
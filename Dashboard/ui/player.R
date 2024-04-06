tabItem(
  tabName = "tab_players",
  
  column(
    width = 12,
    
    box(
      title = tagList(icon("star"), "Player Stats"), status = "primary", width=NULL, solidHeader = TRUE, collapsible = FALSE,
      
      tags$hr(),
      
      fluidRow(
        
        column(
          width = 2,
          boxPad(
            color = "yellow",
            h6("Player 1")),
          pickerInput("tp_league", "League:", choices = ""),
          pickerInput("tp_team", "Team:", choices =""),
          pickerInput("tp_player", "Player:", choices ="")
          
        ),
        
        column(
          width = 8,

          fluidRow(
            column(width = 4),
            column(width = 2, uiOutput("PlayerImg")), 
            column(width = 2,offset = 1, uiOutput("PlayerImg2")),
            column(width = 4)
            ),
          
          br(),
          conditionalPanel(condition = "output.PlayerImg",hr()),
          
          column(offset = 1, width = 12,
                 fluidRow(
                   valueBoxOutput("tp_age",width = 3),
                   valueBoxOutput("tp_overall",width = 2),
                   valueBoxOutput("tp_value",width = 3),
                   valueBoxOutput("tp_contract",width = 3)
                   ),

                 fluidRow(
                   valueBoxOutput("tp_age2",width = 3),
                   valueBoxOutput("tp_overall2",width = 2),
                   valueBoxOutput("tp_value2",width = 3),
                   valueBoxOutput("tp_contract2",width = 3)
                 )
          )
          
          ),
        
        column(
          width = 2,
          boxPad(
            h6("Player 2"),
            color = "olive"),
          pickerInput("tp_league2", "League:", choices = ""),
          pickerInput("tp_team2", "Team:", choices =""),
          pickerInput("tp_player2", "Player:", choices ="")
          )
        
      ),
      
      tags$hr(),
      tags$br(),
      
      conditionalPanel(
        condition = TRUE,
        
        column(
          width = 12,
          
          tabsetPanel(type = "pills",
            
            tabPanel("Radar",
                     br(), withSpinner(plotlyOutput("tp_radar"))),
                   
            tabPanel("Bar",
                     br(), withSpinner(plotOutput("tp_bar")), plotOutput("tp_bar2")),
                   
            tabPanel("Line",
                     br(), withSpinner(plotOutput("tp_line")))
                    
                     ))
          )
                   
        )
      )
      
      
      )
  
  
  

           
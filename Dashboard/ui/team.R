tabItem(
  tabName = "tab_teams",
  
  column(
    width = 12,
    
    box(
      title = tagList(icon("star"), "Team Stats"), status = "primary", width=NULL, solidHeader = TRUE, collapsible = FALSE,
      
      tags$hr(),
      
      fluidRow(
        
        column(
          width = 2,
          pickerInput("tt_league", "League:", choices = ""),
          pickerInput("tt_team", "Team:", choices =""),
          actionBttn(inputId = "tt_select", tagList(icon("computer-mouse"),"Select"), 
                              size = "sm", color = "primary", style = "simple", block = T)
        ),
        
        column(width = 10,
               tags$img(src="premier_league.png", style = "height: 70%; width: 10%;margin-left: 15%;"),
               tags$img(src="laligalogo.png", style = "height: 70%; width: 10%; margin-left: 15%;"),
               tags$img(src="bundesliga.png", style = "height: 70%; width: 10%;margin-left: 15%;")
        )
        
      ),
      
      tags$br(),
      tags$hr(),
      tags$br(),
      
      column(
        width = 12,
        
        conditionalPanel(condition = "input.tt_select",
                         
                         tabsetPanel(type = "pills",
                                    
                                    tabPanel("Value", 
                                             br(),
                                             withSpinner(plotOutput("tt_value")),
                                             br(),
                                             withSpinner(plotOutput("tt_value2")),
                                             br(),
                                             withSpinner(plotlyOutput("tt_value3"))),
                                  
                                    
                                    tabPanel("Best Players", 
                                             br(),
                                             prettyRadioButtons(inputId = "tt_tactic", 
                                                                label = "Best Players in Team in terms of Tactics:",
                                                                choices = c("4-4-2", "3-5-2", "4-3-3"), 
                                                                shape = "curve",
                                                                status = "primary", 
                                                                selected = "4-3-3",
                                                                inline = TRUE),
                                             column(width = 6, withSpinner(tableOutput("tt_best_team"))),
                                             
                                             column(width = 6, plotOutput("tt_best_team_plot"))
                                             ),
                                    
                                    tabPanel("Stats", 
                                             br(),
                                             column(width = 5,withSpinner(tableOutput("tt_stats"))))
                                 
                                    
                                    
                         )
        )
      
      )
    
    
  
  
  )))
  
  
  
  
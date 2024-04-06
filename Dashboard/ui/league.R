tabItem(
  tabName = "tab_leagues",
  
  column(
    width = 12,
    
    box(
      title = tagList(icon("star"), "League Stats"), status = "primary", width=NULL, solidHeader = TRUE, collapsible = FALSE,
      
      tags$hr(),
      
      fluidRow(
        
        column(
          width = 2,
          pickerInput("tl_league", "League:", choices = c("")),
          actionBttn(inputId = "tl_select", tagList(icon("computer-mouse"),"Select"), 
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
        
        conditionalPanel(condition = "input.tl_select",
                         
                         fluidRow(
                           valueBoxOutput("values"),
                           valueBoxOutput("numofplayers"),
                           valueBoxOutput("teams")
                           ),
                         
                         br(),
                         
                         fluidRow(
                           
                           conditionalPanel(
                             condition = "output.values",
                             
                             column(width = 6,
                                    div(style="clear:both;  margin-left: 20px;",
                                        prettyRadioButtons(inputId = "tl_tactic", 
                                                           label = "Best Players in The League in terms of Tactics:",
                                                           choices = c("4-4-2", "3-5-2", "4-3-3"), 
                                                           shape = "curve",
                                                           status = "primary", 
                                                           inline = TRUE)
                                    ),
                                    withSpinner(tableOutput("best_team"))
                                    
                             ),
                             
                             column(width = 6,
                                    
                                    tabsetPanel(
                                      
                                      tabPanel("Nationality",
                                               br(),
                                               withSpinner(
                                                 plotlyOutput(outputId = "league_nat1"))
                                      ),
                                      
                                      tabPanel("Players", 
                                               br(),
                                               withSpinner(
                                                 plotOutput(outputId = "league_values", height = "100%")
                                                 )
                                      ),
                                      
                                      tabPanel("Comparison", 
                                               
                                               br(),
                                               
                                               fluidRow(
                                                 column(width = 6, pickerInput("comp_league", choices = c("League", "Team", "Position"))),
                                                 column(width = 6, pickerInput("comp_graph", choices = c("Bar", "Boxplot")))
                                               ),
                                               
                                               br(), withSpinner(plotOutput("league_comp1"))
                                      )
                                    )
                             )
                           )
                                            
                                            
                                            
                           )
                           
                           
                         
        )
        
      )
    )
    
  )
  
)

                           
                           
                           
                           
                           







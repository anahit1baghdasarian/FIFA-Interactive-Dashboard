tabItem(
  tabName = "tab_countries",
  
  column(
    width = 12,
    
    box(
      title = tagList(icon("star"), "Country Stats"), status = "primary", width=NULL, solidHeader = TRUE, collapsible = FALSE,
      
      tags$hr(),
      
      fluidRow(
        
        column(
          width = 2,
          boxPad(
            color = "purple",
            h6("Country 1")),
          pickerInput("tp_nationality", "Country:", choices ="")
          
        ),
        
        column(
          width = 8,
          
          fluidRow(
            column(width = 4),
            column(width = 2, uiOutput("CountryImg")),
            column(width = 2,offset = 1, uiOutput("CountryImg2")),
            column(width = 4)
          ),
          
          br(),
          conditionalPanel(condition = "output.CountryImg",hr()),
          
          column(offset = 1, width = 12,
                 fluidRow(
                   valueBoxOutput("tp_mean_age",width = 2),
                   valueBoxOutput("tp_avg_overall",width = 3),
                   valueBoxOutput("tp_average_value",width = 4),
                   valueBoxOutput("tp_avg_potential",width = 2)
                 ),
                 
                 fluidRow(
                   valueBoxOutput("tp_mean_age2",width = 2),
                   valueBoxOutput("tp_avg_overall2",width = 3),
                   valueBoxOutput("tp_average_value2",width = 4),
                   valueBoxOutput("tp_avg_potential2",width = 2)
                 )
          )
          
        ),
        
        column(
          width = 2,
          boxPad(
            h6("Country 2"),
            color = "olive"),
          pickerInput("tp_nationality2", "Country:", choices ="")
        )
        
      ),
      
      tags$hr(),
      tags$br(),
      
      conditionalPanel(
        condition = TRUE,
        
        column(
          width = 12,
          
          tabsetPanel(type = "pills",
                      
                      tabPanel("Finishing",
                               br(), withSpinner(plotOutput("tp_facet_finishing")), 
                               plotOutput("tp_facet_finishing2")),
                      tabPanel("Playmaking",
                               br(), withSpinner(plotOutput("tp_facet_playmaking")),
                               plotOutput("tp_facet_playmaking2")),
                      tabPanel("Atheletics",
                               br(), withSpinner(plotOutput("tp_facet_athletics")),
                               plotOutput("tp_facet_athletics2")),
                      
                      tabPanel("Hypothesis", 
                               br(),
                               tags$p ("Country 1's features are shifted right(greater)."),
                               br(),
                               column(width = 5,withSpinner(tableOutput("stats_table_1"))),
                               tags$p ("Country 2's features are shifted right(greater)."),
                               br(),
                               column(width = 5,withSpinner(tableOutput("stats_table_2"))))
             
          )
          
        )
      )
      
      
    )))
tabItem(
  tabName = "tab_home", 
  class = "active", 
  
  column(
    width = 12,
    
    fluidRow(
      
      # Welcome Box
      box(title = h3("⚽ 𝗗𝗮𝘁𝗮 𝗩𝗶𝘀𝘂𝗮𝗹𝗶𝘇𝗮𝘁𝗶𝗼𝗻 (𝗦𝗲𝗰. 𝗕) - 𝗙𝗮𝗹𝗹 𝟮𝟬𝟮𝟮, 𝗙𝗶𝗻𝗮𝗹 𝗣𝗿𝗼𝗷𝗲𝗰𝘁 ⚽", style = 'font-size:42px;color:white;font-weight: bold;text-align: center'), status = "primary", width = NULL, solidHeader = TRUE, collapsible = FALSE,
          
          # FIFA Image
          column(width = 8, tags$img(src="fifa.jpg", style="width: 100%; height: 100%;")),
          
          # Description
          column(width = 4,
                 
tags$p ("Welcome to our dashboard!"),
br(),
tags$p ("Here we have created an interactive dashboard based on the accumulated data from the EA Sports FIFA Football video game series."),
br(),
tags$p ("Look around and you will find several interesting comparisons and visual representations of the dynamic relationships between your favorite football leagues, teams, and players!"),
br(),
tags$p ("Through the navigation bar on the left, you will encounter some of the following key features:"),
br(),
tags$p ("Volley: an air-borne strike before the ball reaches the ground"),
br(),
tags$p ("Crossing: a medium- to-long-range pass"),
br(),
tags$p ("Finishing: scoring goals"),
br(),
tags$p ("Overall: the overall score"),
br(),
tags$p ("Dribbling: staying on the ball while moving it around"),
br(),
tags$p ("Curve:  kick to put spin on the ball and control the direction of your strike by making it curve through the air"),
br(),
tags$p ("ShortPassing: passing the ball at ground level"),
br(),
tags$p ("LongPassing: an attacking skill that allows players to switch the direction of the attack very quickly to create space, find a teammate or to catch out the opposition"),
br(),
tags$p ("As you dig deeper into our dashboard, you will also come across the possibility of hypothesis testing based upon your chosen features and elements.")
          )
      )
    )
  )
  

  
  
         
         
             
             
  
)


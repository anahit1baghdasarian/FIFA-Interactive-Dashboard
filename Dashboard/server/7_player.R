# Reactive

rvPlayer <- reactiveValues(League = NULL, Team = NULL, Player = NULL,
                           League2 = NULL, Team2 = NULL, Player2 = NULL)

observe({
  
  req(input$tp_league)
  req(input$tp_team)
  req(input$tp_player)
  
  rvPlayer$League <- input$tp_league
  rvPlayer$Team   <- input$tp_team
  rvPlayer$Player <- input$tp_player
  
  req(input$tp_league2)
  req(input$tp_team2)
  req(input$tp_player2)
  
  rvPlayer$League2 <- input$tp_league2
  rvPlayer$Team2   <- input$tp_team2
  rvPlayer$Player2 <- input$tp_player2
  
  df_Players <- rv$df %>% 
    filter(League %in% rvPlayer$League, Club %in% rvPlayer$Team, FullName %in% rvPlayer$Player) %>% head(1)
  df_Players2 <- rv$df %>% 
    filter(League %in% rvPlayer$League2, Club %in% rvPlayer$Team2, FullName %in% rvPlayer$Player2) %>% head(1)
  
  
# Image -------------------------------------------------------------------
  

  output$PlayerImg <- renderUI({
    imgplayer(df_Players)
  })
  
  output$PlayerImg2 <- renderUI({
    imgplayer(df_Players2)
  })
  
#Value Box ---------------------------------------------------------------
  
  # Player 1
  output$tp_age <- renderValueBox({
    vbox_players(df_Players, variable = 'Age', color = "yellow")
  })
  
  output$tp_overall <- renderValueBox({
    vbox_players(df_Players, variable = 'Overall', color = "yellow")
  })
  

  output$tp_value <- renderValueBox({
    vbox_players(df_Players, variable = 'ValueEUR', color = "yellow") 
  })
  

  output$tp_contract <- renderValueBox({
    vbox_players(df_Players, variable = 'ContractUntil', color = "yellow")
  })
  
  # Player 2
  
  output$tp_age2 <- renderValueBox({
    vbox_players(df_Players2, variable = 'Age', color = "olive")
  })
  
  output$tp_overall2 <- renderValueBox({
    vbox_players(df_Players2, variable = 'Overall', color = "olive")
  })
  

  output$tp_value2 <- renderValueBox({
    vbox_players(df_Players2, variable = 'ValueEUR', color = "olive")
  })
  
  output$tp_contract2 <- renderValueBox({
    vbox_players(df_Players2, variable = 'ContractUntil', color = "olive")
  })
  
  
# Radar -------------------------------------------------------------------
  
  output$tp_radar <- renderPlotly({
    
    radar <- rv$df %>% 
      transmute(FullName, League, Club, 
                Speed = (SprintSpeed + Agility + Acceleration) / 3,
                Power = (Strength + Stamina + Balance) / 3,
                Technic = (BallControl + Dribbling + Vision) / 3,
                Attack = (Finishing + ShotPower + LongShots + Curve) / 4,
                Defence = (Marking + StandingTackle + SlidingTackle) / 3)
    
    radarP1 <- radar %>% filter(League %in% rvPlayer$League, Club %in% rvPlayer$Team, FullName %in% rvPlayer$Player)
    radarP2 <- radar %>% filter(League %in% rvPlayer$League2, Club %in% rvPlayer$Team2, FullName %in% rvPlayer$Player2)
    
    
    plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) %>% 
      add_trace(
        r = c(radarP1 %>% pull(Speed),radarP1 %>% pull(Power), radarP1 %>% pull(Technic), radarP1 %>% pull(Attack), radarP1 %>% pull(Defence)),
        theta = c('Speed','Power','Technic', 'Attack', 'Defence'),
        name = rvPlayer$Player
      ) %>%
      add_trace(
        r = c(radarP2 %>% pull(Speed),radarP2 %>% pull(Power), radarP2 %>% pull(Technic), radarP2 %>% pull(Attack), radarP2 %>% pull(Defence)),
        theta = c('Speed','Power','Technic', 'Attack', 'Defence'),
        name = rvPlayer$Player2
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,100)
          ))) 
    
    
  })
  
  
  
  

# Bar Plot ----------------------------------------------------------------

  output$tp_bar <- renderPlot({

    grid.arrange(
      ncol = 2,
     
      facetReactiveBar(df_Players, fill_variable = "orange", fill_strip = "orange2"),
      
      facetReactiveBar(df_Players2, fill_variable = "springgreen3", fill_strip = "springgreen4")
      
    )

  })
  
  
  
  bar <- rbind(df_Players %>% mutate(Cat = 1), df_Players2 %>% mutate(Cat = 2)) %>% 
    select(FullName, Cat, Crossing:SlidingTackle) %>% 
    rename_all(funs(gsub("[[:punct:]]", " ", .))) %>% 
    gather(Skill, Exp, Crossing:SlidingTackle, -FullName, -Cat)
  
  output$tp_bar2 <- renderPlot({
    
    bar %>% 
      ggplot(aes(Skill, Exp, fill = factor(Cat)))+
      geom_col(position = "fill", show.legend = FALSE)+
      geom_hline(yintercept = 0.5, color = "black", size = 1)+
      coord_flip()+
      theme_minimal()+
      labs(x = NULL, y = "Ability")+
      scale_fill_manual(values = c("orange", "springgreen4"))+
      scale_y_reverse()
      
  })
  
  

# Line Plot ---------------------------------------------------------------
  output$tp_line <- renderPlot({

    grid.arrange(
      ncol = 2,
      
      facetReactiveLine(rv$fifa_year, player = df_Players, color_variable = "orange", fill_strip = "orange"),
      
      facetReactiveLine(rv$fifa_year, player = df_Players2, color_variable = "springgreen4", fill_strip = "springgreen4")
      
    )
    
  })
  
  

# Similarity --------------------------------------------------------------

  output$similarityplot <- renderPlot({

    if(length(input$sm_league) < 1) return(NULL)
    
    grid.arrange(
      ncol = 2,
      similarity(rv$df, input = input$distance, selectLeague = input$sm_league, 
                 player = rvPlayer$Player, fill_variable = "#dd4b39", fill_strip = "firebrick"),
      similarity(rv$df, input = input$distance, selectLeague = input$sm_league, 
                 player = rvPlayer$Player2, fill_variable = "royalblue", fill_strip = "navy")
    )

  })
  

})











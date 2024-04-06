rvTeam<- reactiveValues(League = NULL, Team = NULL)

observeEvent(input$tt_select,{
  
  req(input$tt_league)
  req(input$tt_team)
  
  rvTeam$League <- input$tt_league
  rvTeam$Team   <- input$tt_team
  
  


# Value -------------------------------------------------------------------

  
  output$tt_value <- renderPlot({
    
    if(is.null(rvTeam$League) | is.null(rvTeam$Team)) return(NULL)
    

    grid.arrange(

      rv$df %>% filter(League %in% rvTeam$League, Club %in% rvTeam$Team) %>% arrange(-ValueEUR) %>% head(11) %>%
      ggplot(aes(reorder(Name, ValueEUR), ValueEUR, fill = Class, label = paste0("€", ValueEUR / 1000000, "M")))+
      geom_col(show.legend = FALSE, fill = "steelblue", color = "white")+
      geom_label(position = "identity") +
      scale_y_sqrt() + 
      coord_polar(theta='x', start=0.9*pi) + 
      labs(title = "Most Valuable Players", x = NULL, y = NULL)+
      theme_minimal()+
      theme(axis.text.x=element_blank(),
            strip.text.x = element_text(size = 10, colour = "white",face = "bold.italic"),
            panel.background = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid = element_line(colour = 'black'))+
        scale_fill_brewer(palette = "Set2", name = " "),

    rv$df %>% filter(League %in% rvTeam$League, Club %in% rvTeam$Team) %>%
      ggplot(aes(ValueEUR))+
      geom_histogram(fill = "steelblue", color = "steelblue", aes(y = ..density..), alpha = 0.6)+
      geom_density(color = "darkblue") +
      labs(title = "Distribution of Player Values", x = NULL, y = NULL)+
      theme_minimal()+
      scale_x_continuous(labels = comma) +
      theme(axis.text.y = element_blank()),

    ncol = 2)
    
 })
  
  output$tt_value2 <- renderPlot({
    
    if(is.null(rvTeam$League) | is.null(rvTeam$Team)) return(NULL)
    
    grid.arrange(
      rv$df %>% filter(League %in% rvTeam$League, Club %in% rvTeam$Team) %>% 
        ggplot(aes(x="", y = ValueEUR, fill = Class))+
        geom_boxplot(alpha = 0.6)+coord_flip()+ facet_wrap(Class~.)+
        theme_minimal()+
        theme(legend.position="bottom")+ 
        scale_fill_brewer(palette = "Set2", name = " ") +
        scale_y_continuous(labels = comma)+
        labs(x = NULL, y = "Value", title = "Distribution of Class Values with Boxplot"),
      
      rv$df %>% filter(League %in% rvTeam$League, Club %in% rvTeam$Team) %>% 
        ggplot(aes(ValueEUR, fill = Class, color = Class))+
        geom_density(alpha = 0.6)+ facet_wrap(Class~.)+
        theme_minimal()+
        theme(legend.position="bottom", axis.text.y=element_blank())+ 
        scale_fill_brewer(palette = "Set2", name = " ") +
        scale_color_brewer(palette = "Set2", name = " ") +
        scale_x_continuous(labels = comma)+
        labs(y = NULL, x = "Value", title = "Distribution of Class Values with Density"),
      ncol = 2)
  })
  
  output$tt_value3 <- renderPlotly({
    
    if(is.null(rvTeam$Team)) return(NULL)
    
    f <- rv$fifa_team_year %>% filter(club %in% rvTeam$Team) %>% 
      group_by(Year, club) %>% 
      summarise(`Total Value` = sum(value_eur))
    
    ggplotly(
      ggplot(f)+
        geom_line(aes(x = Year, y = `Total Value`, group = 1), show.legend = FALSE, color = "steelblue")+
        theme_minimal()+
        theme(legend.position='none',
              strip.background =element_rect(fill="gray"),
              strip.text.x = element_text(size = 10, colour = "white",face = "bold.italic"))+
        labs(x = NULL, y = "Value, €", title = "Total Team Value by Years")+
        scale_y_continuous(labels = comma)
    )
    
  })
  
  
  
  
 
# Best Team ---------------------------------------------------------------
  
  bestTeam <- rv$df %>% filter(League %in% rvTeam$League, Club %in% rvTeam$Team)
  
  output$tt_best_team <- renderTable({
    
    if(is.null(rvTeam$League) | is.null(rvTeam$Team)) return(NULL)
    
    
    if(input$tt_tactic == "4-4-2"){
      
      tac <- input$tt_tactic
      
    }else if(input$tt_tactic == "4-3-3"){
      
      tac <- input$tt_tactic
      
    }else if(input$tt_tactic == "3-5-2"){
      
      tac <- input$tt_tactic
      
    }else{
      return(NULL)
    }
    
    teamTeam <- best_team(bestTeam, input = tac)
    
    
    if(nrow(teamTeam) < 11){
      sendSweetAlert(session = session, type = "error", title = "This tactic is not suitable for your team!",
                     text = "Try other tactics!")
    }
    
    if(nrow(teamTeam) == 11){
      sendSweetAlert(session = session, type = "success", title = "This tactical is suitable for your team!")
    }
    
    
   teamTeam %>% 
     add_row(Name = "Average Overall:", Overall = round(mean(teamTeam$Overall), digits = 2), Positions = "ALL") %>% 
     mutate(` ` = 1:length(Name),
            ` ` = if_else(` ` == max(` `), NA_integer_, ` `),
            ` ` = as.character(` `),
            ` ` = if_else(is.na(` `), "", ` `),
            ClubNumber = as.character(ClubNumber),
            ClubNumber = if_else(is.na(ClubNumber), "", ClubNumber),
            Club = as.character(Club),
            Club = if_else(is.na(Club), "", Club)) %>% 
     select(` `, everything()) %>% 
     rename_all(funs(gsub("[[:punct:]]", " ", .)))



  })
  
  
  # Best Team Plot
  output$tt_best_team_plot <- renderPlot({
    
    teamTeam <- best_team(bestTeam, input = input$tt_tactic)
    
    img <- image_read("database/pitch.png")
      
    formation442 <- data.frame(Positions = as.factor(c("GK","CB", "CB", "CB", "RM", "CM","CM", "CM", "LM", "ST", "ST")), 
                               X = c(5, 17, 17, 17, 17, 35, 35, 35, 35, 60, 60), 
                               Y = c(50 ,16 ,41, 58, 83, 16, 41, 58, 83, 33, 66))
    
    formation433 <- data.frame(Positions = as.factor(c("GK", "LB", "CB","CB", "RB", "RM", "CM", "LM", "LW", "ST", "RW")), 
                               X = c(5, 17, 17, 17, 17, 35, 35, 35, 55, 60, 55), 
                               Y = c(50 ,16 ,41, 58, 83, 25, 50, 75, 25, 50, 75))
    
    formation352 <- data.frame(Positions = as.factor(c("GK","CB", "CB", "CB", "RM", "CM","CM", "CM", "LM", "ST", "ST")), 
                               X = c(5, 17, 17, 17, 35, 35, 35, 35, 35, 60, 60), 
                               Y = c(50 ,25 ,50, 75, 16, 35, 50, 65, 83, 33, 66))
    
    tp <- rv$df %>% 
      filter(League %in% rvTeam$League, Club %in% rvTeam$Team) %>% 
      select(BestPosition, Overall) %>% 
      group_by(BestPosition) %>% 
      summarise(Overall = round(mean(Overall, na.rm = TRUE)))
    
    if(input$tt_tactic == "4-4-2"){
      
      formation <- formation442
      
    }else if(input$tt_tactic == "4-3-3"){
      
      formation <- formation433
      
    }else if(input$tt_tactic == "3-5-2"){
      
      formation <- formation352
      
    }else{
      return(NULL)
    }
    
    if(nrow(teamTeam) < 15){

      ggplot()+
        background_image(img)+
        theme(panel.background = element_rect(fill = "forestgreen",
                                              size = 0.5, linetype = "solid"))

    }else{

      ggplot(cbind(teamTeam,
                   formation %>% select(-BestPosition)), aes(X, Y, label = ClubNumber))+
        background_image(img)+
        geom_point(size = 11, shape = 21, fill = "khaki", stroke = 3, color = "gold")+
        geom_text()+
        theme_void()+
        labs(caption = paste0("TOTAL TEAM POWER \n","Goal Keeper: ", tp$Overall[1], " | ", "Defender: ", tp$Overall[2], " | ",
                              "Midfielder: ", tp$Overall[3], " | ", "Forward: ", tp$Overall[4]))+
        theme(panel.background = element_rect(fill = "forestgreen",
                                              size = 0.5, linetype = "solid"),
              plot.caption = element_text(hjust=0.5, color = "seagreen", size=rel(1.2)))+
        fill_palette("jco")+
        xlim(0,70)
        
      
      }

  })
  

  
  
# Stats -------------------------------------------------------------------   
  
  output$tt_stats <- renderTable({
    
    if(is.null(rvTeam$League) | is.null(rvTeam$Team)) return(NULL)
    
    dfTop <- rv$df  %>% filter(League %in% rvTeam$League, Club %in% rvTeam$Team) 

      suppressWarnings(bind_rows(
        
        headTail(dfTop %>% arrange(-Age), top = 1,bottom = 1) %>% na.omit() %>%  
          mutate(Top = if_else(Age == max(Age), "Oldest", "Youngest")) %>% select(Name, Top, Age) %>% rename(Feature = Age) %>% 
          mutate_at(vars(c(Name, Top, Feature)), funs(as.character)),
        
        headTail(dfTop %>% arrange(-ValueEUR), top = 1,bottom = 1) %>% na.omit() %>%  
          mutate(Top = if_else(ValueEUR == min(ValueEUR),"Most Worthless", "Most Valuable")) %>% 
          select(Name, Top, ValueEUR) %>% rename(Feature = ValueEUR)%>% 
          mutate_at(vars(c(Name, Top, Feature)), funs(as.character)),
        
        headTail(dfTop %>% arrange(-Overall), top = 1,bottom = 1) %>% na.omit() %>%  
          mutate(Top = if_else(Overall == max(Overall), "Best Player", "Worst Player")) %>% select(Name, Top, Overall) %>% rename(Feature = Overall) %>% 
          mutate_at(vars(c(Name, Top, Feature)), funs(as.character)),
        
        headTail(dfTop %>% arrange(-Penalties), top = 1,bottom = 1) %>% na.omit() %>% 
          mutate(Top = if_else(Penalties == max(Penalties), "Best Penalty Taker", "Worst Penalty Taker")) %>% 
          select(Name, Top, Penalties) %>% rename(Feature = Penalties) %>% 
          mutate_at(vars(c(Name, Top, Feature)), funs(as.character)),
        
        headTail(dfTop %>% arrange(-FKAccuracy), top = 1,bottom = 1) %>% na.omit() %>% 
          mutate(Top = if_else(FKAccuracy == max(FKAccuracy), "Best Free Kick Taker", "Worst Free Kick Taker")) %>% 
          select(Name, Top, FKAccuracy) %>% rename(Feature = FKAccuracy) %>% 
          mutate_at(vars(c(Name, Top, Feature)), funs(as.character)),
        
        dfTop %>% arrange(-Vision) %>% head(1) %>% 
          rename(Feature = Vision)%>% 
          mutate(Top = "Best Game Vision: Possible Captain") %>% select(Name, Top, Feature) %>% 
          mutate_at(vars(c(Name, Top, Feature)), funs(as.character)),
        
        
      )%>% rename_all(funs(gsub("[[:punct:]]", " ", .))))
  })
  
  
})



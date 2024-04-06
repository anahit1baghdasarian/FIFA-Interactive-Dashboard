
# Reactive

rvLeague <- reactiveValues(League = NULL)

observeEvent(input$tl_select, {
  
  
  req(input$tl_league)
  
  
  rvLeague$League <- input$tl_league
  
  
  
# Value Box ---------------------------------------------------------------
  
  
  output$values <- renderValueBox({
    
    if(is.null(rvLeague$League)) return(NULL)
    
    valueBox(
      color = "purple",
      value = rv$df %>% filter(League %in% rvLeague$League) %>% 
        summarise(total = paste0("€", round(sum(ValueEUR / 1000000000), digits = 1), "B")) %>% pull(total),
      subtitle = "Total League Value",
      icon=icon("money-bills")
      )
    
  })
  output$numofplayers <- renderValueBox({
    
    if(is.null(rvLeague$League)) return(NULL)
    
    valueBox(color = "purple",
             value = rv$df %>% filter(League %in% rvLeague$League) %>% select(Club) %>% nrow(),
             subtitle = "Number of Players",
             icon=icon("users")
    )
  })
  
  output$teams <- renderValueBox({
    
    if(is.null(rvLeague$League)) return(NULL)
    valueBox(
      color = "purple",
      value = rv$df %>% filter(League %in% rvLeague$League) %>% select(Club) %>% distinct() %>% nrow(),
      "Number of Teams",
      icon=icon("circle-nodes")
    )
  })
  
  
# Best Team ---------------------------------------------------------------
  
  bestLeague <- rv$df %>% filter(League %in% rvLeague$League)
  
  output$best_team <- renderTable({
    
    if(!is.data.frame(bestLeague)) return(NULL)
    
    leagueTeam <- best_team(bestLeague, input = input$tl_tactic)
    
    leagueTeam %>% 
      add_row(Name = "Average Overall:", Overall = round(mean(leagueTeam$Overall), digits = 2), Positions = "ALL") %>% 
      mutate(ClubNumber = as.character(ClubNumber),
             Club = as.character(Club),
             ClubNumber = if_else(is.na(ClubNumber), "", ClubNumber),
             Club = if_else(is.na(Club), "", Club)) %>% 
      rename_all(funs(gsub("[[:punct:]]", " ", .)))
  })
  
  
# Map ---------------------------------------------------------------------
  
  output$league_nat1 <-  renderPlotly({
    
    world_map <- map_data("world")
    
    numofplayers <- world_map %>% 
      mutate(region = as.character(region)) %>% 
      left_join((rv$df %>% mutate(Nationality = as.character(Nationality),
                                  Nationality = if_else(Nationality %in% "England", "UK", Nationality)) %>%
                   filter(League == rvLeague$League) %>%
                   count(Nationality, name = "Number of Player") %>%
                   rename(region = Nationality) %>%
                   mutate(region = as.character(region))), by = "region")
    
    ggplotly(
      ggplot(numofplayers, aes(long, lat, group = group))+
        geom_polygon(aes(fill = `Number of Player` ), color = "white", show.legend = FALSE)+
        scale_fill_viridis_c(option = "viridis")+
        theme_void()+
        labs(fill = "Number of Players",
             title = "Nationality of The Players in The League"))
    
  })
  
  
# Player Value -------------------------------------------------------------------
  
  output$league_values <- renderPlot(width = "auto",{
    
    if(is.null(rvLeague$League)) return(NULL)
    rv$df %>% 
        filter(League == rvLeague$League) %>% 
        arrange(-ValueEUR) %>% 
        filter(ClubPositionGlobal != 'Other') %>%
        group_by(ClubPositionGlobal) %>% 
        top_n(n = 10, wt = ValueEUR) %>% 
        ggplot(aes(reorder(Name, ValueEUR), ValueEUR, fill = ClubPositionGlobal, label = paste0("€", ValueEUR / 1000000, "M")))+
        geom_col(show.legend = FALSE)+
        geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
        coord_flip()+
        theme_minimal()+
        facet_wrap(ClubPositionGlobal~., scales = "free")+
        scale_fill_brewer(palette = "Set1")+
        theme(axis.text.x=element_blank(),
              strip.background =element_rect(fill="gray"),
              strip.text.x = element_text(size = 10, colour = "white",face = "bold.italic"))+
        labs(title = "Most Valuable Players", x = NULL, y = NULL)
    
  }, height = function() {
    session$clientData$output_league_values_width})
  
  
# Comprasion --------------------------------------------------------------
  
  output$league_comp1 <- renderPlot({
    
    if(is.null(rvLeague$League)) return(NULL)
    
    if(input$comp_league == "League" && input$comp_graph == "Bar"){
      
      rv$df %>% 
        group_by(League) %>% 
        summarise(Total.Value = sum(as.integer(ValueEUR), na.rm = TRUE)) %>% 
        ggplot(aes(reorder(League, Total.Value), Total.Value, fill = Total.Value))+
        geom_col(show.legend = FALSE)+
        coord_flip()+
        theme_minimal()+
        labs(x = NULL, y = "League Total Value")+
        scale_fill_gradient(low = "yellow", high = "seagreen4")+
        theme(axis.line.y = element_line(colour = "darkslategray"),
              axis.ticks.x = element_line(colour = "darkslategray"))+
        scale_y_continuous(labels = c("0 €", "2 Billion €", "4 Billion €", "6 Billion €"))
      
      
    }else if(input$comp_league == "Team" && input$comp_graph == "Bar"){
      
      p <- rv$df %>%
        filter(League == rvLeague$League) %>% 
        group_by(Club) %>% 
        summarise(Total.Value = sum(ValueEUR)) %>% 
        ggplot(aes(reorder(Club, Total.Value), Total.Value, fill = Total.Value))+
        geom_col(show.legend = FALSE)+
        coord_flip()+
        theme_minimal()+
        labs(x = NULL, y = "Team Total Value")+
        scale_fill_gradient(low = "yellow", high = "seagreen4")+
        theme(axis.line.y = element_line(colour = "darkslategray"),
              axis.ticks.x = element_line(colour = "darkslategray"))
      
      if(rvLeague$League %in% c("Bundesliga", "Serie A")){
      
        p+scale_y_continuous(labels = c("€0", "€200M", "€400M", "€600M", '€800M')) # Bundesliga & Serie A 
        
      }else if(rvLeague$League == "Ligue 1"){ 
        
        p+scale_y_continuous(labels = c("€0", "€200M", "€400M", "€600M", '€800M')) # Ligue 1
        
      }else if(rvLeague$League == "La Liga"){
        
        p+scale_y_continuous(labels = c("€0", "€250M", "€500M", "€750M", '€1B')) # "La Liga"
        
      }else if(rvLeague$League == "Süper Lig"){
        
        p+scale_y_continuous(labels = c("€0", "€50M", "€100M", "€150M", '€200M')) # Süper Lig
        
      }else if(rvLeague$League == "Liga Nos"){
        
        p+scale_y_continuous(labels = c("€0", "€100M", "€200M", "€300M", '€400M')) # Liga Nos
        
      }else if(rvLeague$League == "Eredivisie"){
        
        p+scale_y_continuous(labels = c("€0", "€50M", "€100M", "€150M", "€200M")) # Eredivisie
        
      }else if(rvLeague$League == "Premier League"){
        
        p+scale_y_continuous(labels = c("€0", "€200M", "€400M", "€600M")) # Premier League
        
      }else{return(NULL)}

    }else if(input$comp_league == "Position" && input$comp_graph == "Bar"){
      
      p <- rv$df %>%
        filter(League == rvLeague$League) %>% 
        filter(ClubPositionGlobal != 'Other') %>%
        group_by(ClubPositionGlobal) %>% 
        summarise(Total.Value = sum(as.numeric(ValueEUR))) %>% 
        ggplot(aes(reorder(ClubPositionGlobal, Total.Value), Total.Value, fill = Total.Value))+
        geom_col(show.legend = FALSE)+
        coord_flip()+
        theme_minimal()+
        labs(x = NULL, y = "Position Total Value")+
        scale_fill_gradient(low = "yellow", high = "seagreen4")+
        theme(axis.line.y = element_line(colour = "darkslategray"),
              axis.ticks.x = element_line(colour = "darkslategray"))
      
      if(rvLeague$League == "Bundesliga"){
        
        p+scale_y_continuous(labels = c("€0", "€500M", "€1B", "€1.5B")) # Bundesliga % Serie A
        
      }else if ( rvLeague$League =="Serie A"){
        
        p+scale_y_continuous(labels = c("€0", "€500M", "€1B", "€1.5B", '€2B'))
        
      }else if(rvLeague$League == "La Liga"){
        
        p+scale_y_continuous(labels = c("€0", "€500M", "€1B", "€1.5B", "€2B")) # La Liga
        
      }else if(rvLeague$League == "Süper Lig"){
        
        p+scale_y_continuous(labels = c("€0", "€200M", "€400M", "€600M", '€800M', '€1000M'))# Süper Lig
        
      }else if(rvLeague$League == "Liga Nos"){
        
        p+scale_y_continuous(labels = c("€0", "€200M", "€400M", "€600M", "€800M"))# Liga Nos
        
      }else if(rvLeague$League == "Eredivisie"){
        
        p+scale_y_continuous(labels = c("€0", "€100M", "€200M", "€300M", '€400M'))# Eredivisie
          
      }else if(rvLeague$League == "Premier League"){
        
        p+scale_y_continuous(labels = c("€0", "€1.4B", "€2B", "€2.5B")) # Premier League
        
      }else if(rvLeague$League == "Ligue 1"){
        
        p+scale_y_continuous(labels = c("€0", "€500M", "€1B", "€1.5B", '€2B')) # Ligue 1
        
      }else{return(NULL)}
      
    
    }else if(input$comp_league == "League" && input$comp_graph == "Boxplot"){
      
      value_wage(rv$df, variable = "Value", x = "League")

      
    }else if(input$comp_league == "Team" && input$comp_graph == "Boxplot"){
      
      value_wage(rv$df %>% filter(League == rvLeague$League), variable = "Value", x = "Club")
      
    }else if(input$comp_league == "Position" && input$comp_graph == "Boxplot"){
      
      rv$df %>% filter(League == rvLeague$League) %>% 
        ggplot(aes(reorder(ClubPositionGlobal, ValueEUR), ValueEUR, fill = ClubPositionGlobal))+
        geom_boxplot()+
        coord_flip()+
        theme_minimal()+
        labs(x = NULL, y = "Position Value Range")+
        scale_fill_viridis_d(option = "E")+
        theme(axis.line.y = element_line(colour = "darkslategray"),
              axis.ticks.x = element_line(colour = "darkslategray"),
              legend.position = "bottom")
      
    }else{return(NULL)}
    
  })
  

  

}) 

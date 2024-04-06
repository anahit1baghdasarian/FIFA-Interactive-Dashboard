# Reactive

rvCountry <- reactiveValues(Nationality = NULL, Nationality2 = NULL)

observe({
  
  req(input$tp_nationality)
  
  rvCountry$Nationality <- input$tp_nationality
  
  req(input$tp_nationality2)
  
  rvCountry$Nationality2 <- input$tp_nationality2
  
  df_countries <- rv$df %>% 
    filter(Nationality %in% rvCountry$Nationality)
  
  df_countries2 <- rv$df %>% 
    filter(Nationality %in% rvCountry$Nationality2)
  
  
  # Value Box ---------------------------------------------------------------
  
  # Player 1
  grouped <- df_countries %>% group_by(Nationality) %>%
    summarise(Age=round(mean(Age), digits = 0),
              Overall=round(mean(Overall), digits =0),
              ValueEUR=paste0('€', round((mean(ValueEUR)/1000000), digits = 1), 'M'),
              IntReputation=round(mean(IntReputation), digits =  2))
  
  output$tp_mean_age <- renderValueBox({
    vbox_countries(grouped, variable = 'Age', color = "purple")
  })
  
  output$tp_avg_overall <- renderValueBox({
    vbox_countries(grouped, variable = 'Overall', color = "purple")
  })
  
  
  output$tp_average_value <- renderValueBox({
    vbox_countries(grouped, variable = 'ValueEUR', color = "purple")
  })
  
  
  output$tp_avg_potential <- renderValueBox({
    vbox_countries(grouped, variable = 'IntReputation', color = "purple")
  })
  
  # Player 2
  
  
  grouped2<- df_countries2%>% group_by(Nationality) %>%
    summarise(Age=round(mean(Age), digits =0),
              Overall=round(mean(Overall), digits = 0),
              ValueEUR=paste0('€', round((mean(ValueEUR)/1000000), digits = 1), 'M'), 
              IntReputation=round(mean(IntReputation), 2))
  
  output$tp_mean_age2 <- renderValueBox({
    vbox_countries(grouped2, variable = 'Age', color = "olive")
  })
  
  output$tp_avg_overall2 <- renderValueBox({
    vbox_countries(grouped2, variable = 'Overall', color = "olive")
  })
  
  
  output$tp_average_value2 <- renderValueBox({
    vbox_countries(grouped2, variable = 'ValueEUR', color = "olive")
  })
  
  output$tp_avg_potential2 <- renderValueBox({
    vbox_countries(grouped2, variable = 'IntReputation', color = "olive")
  })
  
  
  #Facet -------------------------------------------------------------------
  ball_handling <- c('Dribbling','BallControl', 'DribblingTotal')
  play_making <- c('LongPassing', 'ShortPassing', 'Crossing', 'PassingTotal', 'BallControl')
  athletics <- c('Height', 'Weight', 'Acceleration', 'SprintSpeed', 'Agility',
                 'PaceTotal', 'Stamina', 'Strength','PhysicalityTotal')
  finishing <- c('FKAccuracy', 'HeadingAccuracy', 'Volleys', 'Curve', 'Finishing', 'ShootingTotal')
  fnsh <- c('FKAccuracy', 'HeadingAccuracy', 'Finishing')
  full <- c(ball_handling, play_making, athletics, finishing)
  
  library(reshape2)
  output$tp_facet_finishing <- renderPlot({   
    gg <- rv$df %>% 
      filter(Nationality %in% c(rvCountry$Nationality, rvCountry$Nationality2)) %>%
      select(all_of(c(fnsh, 'Nationality', 'Overall', 'Potential', 'Age'))) %>%  
      melt(id.vars=c('Nationality', 'Overall')) %>%  
      ggplot(aes(x=value, y=Overall, color=Nationality)) + 
      geom_point(size = 1, alpha = 0.3) + geom_smooth() + facet_wrap(~variable, scales='free') + 
      theme_minimal() +
      scale_color_manual(values = c("mediumpurple3", "springgreen4")) +
      ggtitle("Overall Rating according to Finishing Features") + theme(plot.title = element_text(size = 30, face = "bold"), legend.position = "none")+
      ylim(0,NA) + xlab(" ") + ylab("Overall Reating")
    gg
    
  })
  
  
  output$tp_facet_finishing2 <- renderPlot({   
    gg2 <- rv$df %>% 
      filter(Nationality %in% c(rvCountry$Nationality, rvCountry$Nationality2)) %>%
      select(all_of(c(fnsh, 'Nationality', 'Overall', 'Potential', 'Age', 'WageEUR'))) %>%  
      melt(id.vars=c('Nationality', 'WageEUR')) %>%  
      ggplot(aes(x=value, y=WageEUR, color=Nationality)) + 
      geom_point(size = 1, alpha = 0.3) + geom_smooth() + facet_wrap(~variable, scales='free') +
      theme_minimal() +
      scale_color_manual(values = c("mediumpurple3", "springgreen4"), name = " ") +
      ggtitle("Wage according to Finishing Features") + theme(plot.title = element_text(size = 30, face = "bold"), legend.position = "none") +
      ylim(0,NA) + xlab(" ") + ylab("Wage in Euros")
    gg2
    
  })
  
  
  
  ## PlayMaking
  
  output$tp_facet_playmaking <- renderPlot({   
    gg2 <- rv$df %>% 
      filter(Nationality %in% c(rvCountry$Nationality, rvCountry$Nationality2)) %>%
      select(all_of(c(play_making, 'Nationality', 'Overall', 'Potential', 'Age', 'WageEUR'))) %>%  
      melt(id.vars=c('Nationality', 'Overall')) %>%  
      ggplot(aes(x=value, y=Overall, color=Nationality)) + 
      geom_point(size = 1, alpha = 0.3) + geom_smooth() + facet_wrap(~variable, scales='free') +
      theme_minimal() +
      scale_color_manual(values = c("mediumpurple3", "springgreen4"), name = " ") +
      ggtitle("Overall Rating according to PlayMaking Features") + theme(plot.title = element_text(size = 30, face = "bold"), legend.position = "none") +
      ylim(0,NA) + xlab(" ") + ylab("Overall Reating")
    gg2
    
  })
  
  output$tp_facet_playmaking2 <- renderPlot({   
    gg2 <- rv$df %>% 
      filter(Nationality %in% c(rvCountry$Nationality, rvCountry$Nationality2)) %>%
      select(all_of(c(play_making, 'Nationality', 'Overall', 'Potential', 'Age', 'WageEUR'))) %>%  
      melt(id.vars=c('Nationality', 'WageEUR')) %>%  
      ggplot(aes(x=value, y=WageEUR, color=Nationality)) + 
      geom_point(size = 1, alpha = 0.3) + geom_smooth() + facet_wrap(~variable, scales='free') +
      theme_minimal() +
      scale_color_manual(values = c("mediumpurple3", "springgreen4"), name = " ") +
      ggtitle("Wage according to PlayMaking Features") + theme(plot.title = element_text(size = 30, face = "bold"), legend.position = "none") +
      ylim(0,NA) + xlab(" ") + ylab("Wage in Euros")
    gg2
    
  })
  
  ## Physical
  
  output$tp_facet_athletics <- renderPlot({   
    gg2 <- rv$df %>% 
      filter(Nationality %in% c(rvCountry$Nationality, rvCountry$Nationality2)) %>%
      select(all_of(c(athletics, 'Nationality', 'Overall', 'WageEUR'))) %>%  
      melt(id.vars=c('Nationality', 'Overall')) %>%  
      ggplot(aes(x=value, y=Overall, color=Nationality)) + 
      geom_point(size = 1, alpha = 0.3) + geom_smooth() + facet_wrap(~variable, scales='free') +
      theme_minimal() +
      scale_color_manual(values = c("mediumpurple3", "springgreen4"), name = " ") +
      ggtitle("Overall Rating according to Physical Characteristics") + theme(plot.title = element_text(size = 30, face = "bold"), legend.position = "none") +
      ylim(0,NA) + xlab(" ") + ylab("Overall Reating")
    gg2
    
  })
  
  output$tp_facet_athletics2 <- renderPlot({   
    gg2 <- rv$df %>% 
      filter(Nationality %in% c(rvCountry$Nationality, rvCountry$Nationality2)) %>%
      select(all_of(c(athletics, 'Nationality', 'Overall', 'WageEUR'))) %>%  
      melt(id.vars=c('Nationality', 'WageEUR')) %>%  
      ggplot(aes(x=value, y=WageEUR, color=Nationality)) + 
      geom_point(size = 1, alpha = 0.3) + geom_smooth() + facet_wrap(~variable, scales='free') +
      theme_minimal() +
      scale_color_manual(values = c("mediumpurple3", "springgreen4"), name = " ") +
      ggtitle("Wage according to Physical Characteristics") + theme(plot.title = element_text(size = 30, face = "bold"), legend.position = "none") +
      ylim(0,NA) + xlab(" ") + ylab("Wage in Euros")
    gg2
    
  })
  
  ## Stats
  
  output$stats_table_1 <- renderTable({
    
    rv$df %>% 
      filter(Nationality %in% c(rvCountry$Nationality, rvCountry$Nationality2)) %>%
      perform_tests(features = full, reference = rvCountry$Nationality)
    
  })
  
  
  
  output$stats_table_2 <- renderTable({
    
    rv$df %>% 
      filter(Nationality %in% c(rvCountry$Nationality, rvCountry$Nationality2)) %>%
      perform_tests(features = full, reference = rvCountry$Nationality2)
    
  })
  
  
  

  
  
  
})







 


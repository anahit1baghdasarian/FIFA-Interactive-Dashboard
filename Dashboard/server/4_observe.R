# League ---------------------------------------------------------------

observe({
  updatePickerInput(session, "tl_league", choices = get_league(rv$df))
})



observe({
  
  updatePickerInput(session, "tt_league", choices = get_league(rv$df), selected = get_league_sample(rv$df))
})


observe({
  updatePickerInput(session, "tt_team", choices = get_team(rv$df, x = input$tt_league))
})


# Player ---------------------------------------------------------------

# Player 1
observe({
  updatePickerInput(session, "tp_league", choices = get_league(rv$df %>% filter(Name %in% rv$fifa_year$Name)), 
                    selected = get_league_sample(rv$df %>% filter(Name %in% rv$fifa_year$Name)))
})


observe({
  updatePickerInput(session, "tp_team", choices = get_team(rv$df %>% filter(Name %in% rv$fifa_year$Name), x = input$tp_league),
                    selected = (get_team(rv$df %>% filter(Name %in% rv$fifa_year$Name), x = input$tp_league)[sample(1:length(get_team(rv$df %>% filter(Name %in% rv$fifa_year$Name), x = input$tp_league)),1)]))
})

observe({
  updatePickerInput(session, "tp_player", choices = get_player(rv$df %>% filter(Name %in% rv$fifa_year$Name), x = input$tp_league, y = input$tp_team),
                    selected = (get_player(rv$df %>% filter(Name %in% rv$fifa_year$Name), x = input$tp_league, y = input$tp_team)[sample(1:length(get_player(rv$df %>% filter(Name %in% rv$fifa_year$Name), x = input$tp_league, y = input$tp_team)),1)]))
})


#Player 2
observe({
  updatePickerInput(session, "tp_league2", choices = get_league(rv$df %>% filter(Name %in% rv$fifa_year$Name)),
                    selected = get_league_sample(rv$df %>% filter(Name %in% rv$fifa_year$Name)))
})

observe({
  updatePickerInput(session, "tp_team2", choices = get_team(rv$df %>% filter(Name %in% rv$fifa_year$Name), x = input$tp_league2),
                    selected = (get_team(rv$df %>% filter(Name %in% rv$fifa_year$Name), x = input$tp_league2)[sample(1:length(get_team(rv$df %>% filter(Name %in% rv$fifa_year$Name), x = input$tp_league2)),1)]))
})

observe({
  updatePickerInput(session, "tp_player2", choices = get_player(rv$df %>% filter(Name %in% rv$fifa_year$Name), x = input$tp_league2, y = input$tp_team2),
                    selected = (get_player(rv$df %>% filter(Name %in% rv$fifa_year$Name), x = input$tp_league2, y = input$tp_team2)[sample(1:length(get_player(rv$df %>% filter(Name %in% rv$fifa_year$Name), x = input$tp_league2, y = input$tp_team2)),1)]))
})


# 3.2 Countries

# C 1
observe({
  updatePickerInput(session, "tp_nationality", choices = get_country(rv$df), 
                    selected = get_country_sample(rv$df, 7))
})



# C 2
observe({
  updatePickerInput(session, "tp_nationality2", choices = get_country(rv$df), 
                    selected = get_country_sample(rv$df, 14))
})




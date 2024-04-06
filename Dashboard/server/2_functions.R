#Best Team ------------------------------------------------------------

best_team <- function(df, input){
  
  team <- NULL
  
  if(missing("df") | missing("input")) return(res)
  if(is.null(df) | is.null(input)) return(res)
  
  team <- tibble()
  team_copy <- df %>% select(ClubNumber, Name, Overall, BestPosition, Positions, Club) %>% arrange(-Overall) 
  
  tac442 <- c("GK","RB", "CB", "CB", "LB", "RM", "CM", "CM", "LM", "ST", "ST")
  tac352 <- c("GK","CB", "CB", "CB", "RM", "CM", "CM", "CM", "LM", "ST", "ST")
  tac433 <- c("GK","RB", "CB", "CB", "LB", "CM", "CDM", "CM", "LW", "RW", "ST")
  
  tactic <- if(input == "4-4-2"){
    tac442
  }else if(input == "3-5-2"){
    tac352
  }else{
    tac433
  }
  
  for (i in tactic) {
    
    team %<>%  bind_rows(team_copy %>% filter(Positions %in% i) %>% head(1))
    team_copy %<>% filter(!Name %in% (team %>% pull(Name)))
    
  }
  
  return(team)
  
}





# Value & Wage Leagues Boxplot Visualization ---------------------------

value_wage <- function(df, variable = c("Value", "Wage"), x = c("League", "Club")){
  
  res <- NULL
  
  if(missing("df") | missing("variable") | missing("x")) return(res)
  if(is.null(df) | is.null(variable) | is.null(x)) return(res)
  
  if(variable == "Value"){
    
    variable <- "ValueEUR"
    
  }else if(variable == "Wage"){
    
    variable <- "WageEUR"
    
  }else{
    
    return(NULL)
    
  }
  
  if(x == "League"){
    
    xvar <- "League"
    
  }else if(x == "Club"){
    
    xvar <- "Club"
    
  }else{
    
    return(NULL)
    
  }
  
  res <- df %>%
    ggplot(aes(reorder(!!rlang::parse_expr(xvar), !!rlang::parse_expr(variable)), !!rlang::parse_expr(variable), fill = !!rlang::parse_expr(xvar)))+
    geom_boxplot(show.legend = FALSE)+
    coord_flip()+
    theme_minimal()+
    labs(x = NULL, y = str_sub(variable, start = 1, end = (str_length(variable)-1) ))+
    scale_fill_viridis_d(begin = 0.2,end = 0.6)+
    theme(axis.line.y = element_line(colour = "darkslategray"),
          axis.ticks.x = element_line(colour = "darkslategray"))
  
  return(res)
}

  


# Best Overall Class ------------------------------------------------------

best_overall <- function(df, class = c("GK", "CAM", "CM", "LF")){
  
  if(missing("df") | missing("class")) return(NULL)
  if(is.null(df) | is.null(class)) return(NULL)
  
  class <- df %>%
    arrange(-Overall) %>% 
    filter(ClubPosition%in% class) %>% 
    head(10)
  
  
  images <-  class %>% pull(PhotoURL)
  names <- class %>% pull(FullName)
  clubs <- class %>% pull(Club)
  overall <- class %>% pull(Overall)
  
  suppressWarnings(widgetUserBox(
    title = tags$p(names[1], style = "font-size: 60%;"),
    color = "purple",
    subtitle = div(tags$p(clubs[1], style = "font-size: 80%;"), tags$img(src = notfound(logo[1], type = "club")), overall[1]),
    type = 2,
    width = 3,
    src = notfound(images[1], type = "player"),
    collapsible = FALSE,
    closable = FALSE,
    
    footer = productList(
      productListItem(
        src = notfound(images[2], type = "player"), 
        productTitle = names[2], 
        productPrice = overall[2], 
        priceColor = "success",
        tags$img(src = notfound(logo[2], type = "club")),clubs[2]
      ),
      productListItem(
        src = notfound(images[3], type = "player"), 
        productTitle = names[3], 
        productPrice = overall[3], 
        priceColor = "success",
        tags$img(src = notfound(logo[3], type = "club")),clubs[3]
      ),
      productListItem(
        src = notfound(images[4], type = "player"), 
        productTitle = names[4], 
        productPrice = overall[4], 
        priceColor = "success",
        tags$img(src = notfound(logo[4], type = "club")), clubs[4]
      ),
      productListItem(
        src = notfound(images[5], type = "player"), 
        productTitle = names[5], 
        productPrice = overall[5], 
        priceColor = "success",
        tags$img(src = notfound(logo[5], type = "club")), clubs[5]
      ),
      productListItem(
        src = notfound(images[6], type = "player"), 
        productTitle = names[6], 
        productPrice = overall[6], 
        priceColor = "success",
        tags$img(src = notfound(logo[6], type = "club")), clubs[6]
      ),
      productListItem(
        src = notfound(images[7], type = "player"), 
        productTitle = names[7], 
        productPrice = overall[7], 
        priceColor = "success",
        tags$img(src = notfound(logo[7], type = "club")),clubs[7]
      ),
      productListItem(
        src = notfound(images[8], type = "player"), 
        productTitle = names[8], 
        productPrice = overall[8], 
        priceColor = "success",
        tags$img(src = notfound(logo[8], type = "club")), clubs[8]
      ),
      productListItem(
        src = notfound(images[9], type = "player"), 
        productTitle = names[9], 
        productPrice = overall[9], 
        priceColor = "success",
        tags$img(src = notfound(logo[9], type = "club")), clubs[9]
      ),
      productListItem(
        src = notfound(images[10], type = "player"), 
        productTitle = names[10], 
        productPrice = overall[10], 
        priceColor = "success",
        tags$img(src = notfound(logo[10], type = "club")), clubs[10]
      )
      
    )
  )
  )
  
}

# Value Box for Players Page -------------------------------------------

vbox_players <- function(df, variable = c('Age', 'Overall', 'Nationality', 'ValueEUR', 'ContractUntil'), 
                         color = c("blue", "green")){
  
  res <- NULL
  
  if(missing("df") | missing("variable") | missing("color")) return(res)
  if(is.null(df) | is.null(variable) | is.null(color)) return(res)
  
  if(variable == 'Age'){
    icon_vb <- "heart-pulse"
  }else if(variable == 'Overall'){
    icon_vb <- "bolt"
  }else if(variable == 'Nationality'){
    icon_vb <- "flag"
  }else if(variable == 'ValueEUR'){
    icon_vb <- "sack-dollar"
  }else if(variable == 'ContractUntil'){
    df <- df %>% rename(Contract = ContractUntil)
    variable <- "Contract"
    icon_vb <- "file-contract"
  }else{
    return(NULL)
    }
  
  res <- valueBox(
    color = color,
    value = tags$p(df %>% pull(!!rlang::parse_expr(variable)), style = "font-size: 80%;"),
    subtitle = variable,
    icon = icon(icon_vb)
  )
  
  return(res)
  
}

# Players Image --------------------------------------------------------


imgplayer <- function(df){
  
  images <- NULL
  
  if(missing("df")) return(images)
  if(is.null(df)) return(images)
  
  images <- df %>% pull(PhotoURL)
  unknown <- httr::GET(images)
  
  if(unknown$status_code == 200){
    
    images <- tags$img(src= images, width = 70, height = 70)
    
  }else{
    
    images <- tags$img(src= "unknown.png", width = 70, height = 70)
    
  }
  

}

# Facet Wrap Reactive --------------------------------------------------

facetReactiveBar <- function(df, fill_variable, fill_strip){
  
  res <- NULL
  
  if(missing("df") | missing("fill_variable") | missing("fill_strip")) return(res)
  if(is.null(df) | is.null(fill_variable) | is.null("fill_strip")) return(res)
  

  res <- df %>% 
    select(FullName, Crossing:SlidingTackle) %>% 
    rename_all(funs(gsub("[[:punct:]]", " ", .))) %>% 
    gather(Skill, Exp, Crossing:SlidingTackle, -FullName) %>% 
    ggplot(aes(Skill, Exp))+
    geom_col(fill = fill_variable, position = "dodge")+
    scale_y_sqrt() +
    coord_polar(theta='x', start=0.9*pi)+
    theme(panel.background = element_blank(),
          axis.title = element_blank(),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid = element_line(colour = 'black'),
          strip.background =element_rect(fill=fill_strip,color = "black"),
          strip.text.x = element_text(size = 10, colour = "white",face = "bold.italic")) +
    facet_wrap(~(df %>% pull(FullName)))+
    labs(x = NULL, y = "Ability")
  
  return(res)
  
}

facetReactiveLine <- function(df, player, color_variable, fill_strip){
  
  res <- NULL
  
  if(missing("df") | missing("player") | missing("color_variable") | missing("fill_strip")) return(res)
  if(is.null(df) | is.null("player") | is.null(color_variable) | is.null("fill_strip")) return(res)
  p1 <- df %>% filter(Name == player$Name) %>% rename("Year" = fifa, "Value" = value_eur)
  
  res <- ggplot(p1)+
    geom_line(aes(x = Year, y = Value, group = 1), color = color_variable)+
    geom_point(aes(x = Year, y = Value, group = 1), color = color_variable) +
    facet_wrap(~p1$Name[length(p1$Name)])+
    theme_minimal()+
    theme(legend.position='none',
          strip.background =element_rect(fill=fill_strip),
          strip.text.x = element_text(size = 10, colour = "white",face = "bold.italic"))+
    labs(x = NULL, y = "Value, €")+
    scale_y_continuous(labels = comma)
  
  return(res)
  
}

#  404 Not Found URL Problems ------------------------------------------

notfound <- function(url, type = c("player", "club")){
  
  if(is.null(url)) return(NULL)
  if(missing("url")) return(NULL)
  
  unknown <- httr::GET(url)
  
  if(unknown$status_code == 200){
    
    images 
    
  }else if(unknown$status_code != 200){
    
    if(type == "player"){
      images <- "unknown.png"
    }else{
      return(NULL)
    }

  }else{
    return(NULL)
  }
  
  return(images)
  
}
  
vbox_countries <- function(df, variable = c('Age', 'Overall', 'Nationality', 'Value', 'International.Reputation'),
                           color = c("blue", "green")){
  res <- NULL
  if(missing("df") | missing("variable") | missing("color")) return(res)
  if(is.null(df) | is.null(variable) | is.null(color)) return(res)
  if(variable == 'Age'){
    icon_vb <- "heart-pulse"
  }else if(variable == 'Overall'){
    icon_vb <- "battery-full"
  }else if(variable == 'Nationality'){
    icon_vb <- "globe"
  }else if(variable == 'ValueEUR'){
    icon_vb <- "piggy-bank"
  }else if(variable == 'IntReputation'){
    df <- df %>% rename(Reputation = IntReputation)
    variable <- "Reputation"
    icon_vb <- "chess-king"
  }else{
    return(NULL)
  }
  res <- valueBox(
    color = color,
    value = tags$p(df %>% pull(!!rlang::parse_expr(variable)), style = "font-size: 80%;"),
    subtitle = variable,
    icon = icon(icon_vb)
  )
  return(res)
}


#  Wilcoxon

perform_tests <- function(data, features, reference = 'FC Bayern München', alpha=0.1){
  significants <- c()
  compData <- data.frame(Feature= character(0), P.Value= numeric(0))
  sub <- data %>% select(all_of(c(features, 'Nationality')))
  uniques <- distinct(sub, Nationality)
  w_s <- c()
  ind <- 0
  for (col in features){
    if (reference!=uniques[1, 'Nationality']){
      sub_1 <- filter(sub, Nationality==uniques[2, 'Nationality'])[[col]]
      sub_2 <- filter(sub, Nationality==uniques[1, 'Nationality'])[[col]]
    }else {
      sub_1 <- filter(sub, Nationality==uniques[1, 'Nationality'])[[col]]
      sub_2 <- filter(sub, Nationality==uniques[2, 'Nationality'])[[col]]
    }
    w <- wilcox.test(sub_1, sub_2, paired = F, alternative = 'greater', exact=FALSE)
    w_s <- c(w_s, w)
    if (w$p.value < alpha){
      ind <- ind + 1
      significants <- c(significants, c(col, round(w$p.value, 3)))
      compData[ind, ] <- c(col, round(w$p.value, 3))
      
    }
    
  }
  return(compData)
}

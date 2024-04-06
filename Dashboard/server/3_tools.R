
# Tools to Get Leagues --------------------------------------------------


get_league <- function(df, x = "League"){
  
  res <- NULL
  
  if(is.null(x)) return(res)
  if(missing("df") ) return(res)
  if(is.null(df)) return(res)
  
  res <- df[,x] %>% unique() %>% sort()
  
  return(res)
}

get_league_sample <- function(df, x = "League"){
  
  res <- NULL
  
  if(is.null(x)) return(res)
  if(missing("df") ) return(res)
  if(is.null(df)) return(res)
  
  res <- df[,x] %>% unique() %>% sort()
  res <- sample(res,1)
  
  return(res)
}

get_country <- function(df){
  
  res <- NULL
  if(missing("df") ) return(res)
  if(is.null(df)) return(res)
  
  res <- df  %>% mutate(Nationality = as.character(Nationality))
  res <- res[,"Nationality"] %>% unique() %>% sort()
  return(res)
}

get_country_sample <- function(df, ii){
  
  res <- NULL
  if(missing("df") ) return(res)
  if(is.null(df)) return(res)
  
  res <- df  %>% mutate(Nationality = as.character(Nationality))
  res <- res[,"Nationality"] %>% unique() %>% sort()
  resv <- sample(res, ii)  # not tidyverse overwritten sample.int
  return(res)
}



# Tools to Get Teams ----------------------------------------------------



get_team <- function(df, x){
  
  res <- NULL
  
  if(missing("x") ) return(res)
  if(is.null(x)) return(res)
  if(missing("df") ) return(res)
  if(is.null(df)) return(res)
  
  res <- df %>% filter(League %in% x) %>% mutate(Club = as.character(Club))
  res <- res[,"Club"] %>% unique() %>% sort()
  return(res)
}

get_team_sample <- function(df, x){
  
  res <- NULL
  
  if(missing("x") ) return(res)
  if(is.null(x)) return(res)
  if(missing("df") ) return(res)
  if(is.null(df)) return(res)
  
  res <- df %>% filter(League %in% x) %>% mutate(Club = as.character(Club))
  res <- res[,"Club"] %>% unique() %>% sort()
  res <- sample(res,1)
  return(res)
}

# Tools to Get Players ---------------------------------------------------
get_player <- function(df, x, y){

  res <- NULL
  
  if(missing("x") ) return(res)
  if(is.null(x)) return(res)
  if(missing("y") ) return(res)
  if(is.null(y)) return(res)
  if(missing("df") ) return(res)
  if(is.null(df)) return(res)
  
  res <- df %>% filter(League %in% x, Club %in% y) %>% pull(FullName)
  return(res)
}

get_player_sample <- function(df, x, y){
  
  res <- NULL
  
  if(missing("x") ) return(res)
  if(is.null(x)) return(res)
  if(missing("y") ) return(res)
  if(is.null(y)) return(res)
  if(missing("df") ) return(res)
  if(is.null(df)) return(res)
  
  res <- df %>% filter(League %in% x, Club %in% y) %>% pull(FullName)
  res <- sample(res)
  return(res)
}




# Tools for X & Y Variables ------------------------------------------

get_variables <- function(df, league, class){
  
  res <- NULL
  
  if(missing("league") ) return(res)
  if(is.null(league)) return(res)
  if(missing("class") ) return(res)
  if(is.null(class)) return(res)
  if(missing("df") ) return(res)
  if(is.null(df)) return(res)

  corp <- df %>% filter(League == league, ClubPosition == class)
  
  if(class == "GK"){
    
    corp <- corp %>% select(Agility, Reactions, Jumping, Strength, Vision, Composure, GKDiving:GKReflexes)
    
  }else if(class %in% c("CF", 'RF', 'LF')){
    
    corp <- corp %>% select(Crossing, HeadingAccuracy, Agility, Reactions, Balance, Jumping, Stamina,
                            Strength, Aggression, Interceptions, Positioning, Vision, Composure, 
                            Marking, StandingTackle, SlidingTackle)
    
  }else if(class %in% c("CDM", 'CAM', 'CM', 'LM', 'RM')){
    
    corp <- corp %>% select(Crossing:SlidingTackle)
    
  }else{
    
    corp <- corp %>% select(Crossing:Aggression, Positioning:Composure)
    
  }
  res <- names(corp) %>% sort()
  
  return(res)
}


# Get Picker List ------------------------------------------------------

get_picker_list <- function(x){
  
  res <- NULL
  
  if(missing("x") ) return(res)
  if(is.null(x)) return(res)
  
  res <-  as.list(setNames(x, x %>% gsub("[[:punct:]]", " ", .) ))
  
  return(res)
}



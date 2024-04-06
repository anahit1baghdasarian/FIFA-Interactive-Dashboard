

# Reactive Values ------------------------------------------------------


rv <- reactiveValues(df = NULL, # Data Frame
                     fifa_year = NULL, fifa_team_year = NULL # Other FIFA Games
                     )


#  ObserveEvent ---------------------------------------------------------


observe({
  
  
# Read Data ------------------------------------------------------------

  bundesliga <- c(
    "1. FC Nürnberg", "1. FSV Mainz 05", "Bayer 04 Leverkusen", "FC Bayern München",
    "Borussia Dortmund", "Borussia Mönchengladbach", "Eintracht Frankfurt",
    "FC Augsburg", "FC Schalke 04", "Fortuna Düsseldorf", "Hannover 96",
    "Hertha BSC", "RB Leipzig", "SC Freiburg", "TSG 1899 Hoffenheim",
    "VfB Stuttgart", "VfL Wolfsburg", "SV Werder Bremen"
  )
  
  premierLeague <- c(
    "Arsenal", "Bournemouth", "Brighton & Hove Albion", "Burnley",
    "Cardiff City", "Chelsea", "Crystal Palace", "Everton", "Fulham",
    "Huddersfield Town", "Leicester City", "Liverpool", "Manchester City",
    "Manchester United", "Newcastle United", "Southampton", 
    "Tottenham Hotspur", "Watford", "West Ham United", "Wolverhampton Wanderers"
    
  )
  
  laliga <- c(
    "Athletic Club de Bilbao", "Atlético Madrid", "CD Leganés",
    "Deportivo Alavés", "FC Barcelona", "Getafe CF", "Girona FC", 
    "Levante UD", "Rayo Vallecano", "RC Celta", "RCD Espanyol", 
    "Real Betis", "Real Madrid", "Real Sociedad", "Real Valladolid CF",
    "SD Eibar", "SD Huesca", "Sevilla FC", "Valencia CF", "Villarreal CF"
  )
  
  seriea <- c(
    "Atalanta","Bologna","Cagliari","Chievo Verona","Empoli", "Fiorentina","Frosinone","Genoa",
    "Inter","Juventus","Lazio","Milan","Napoli","Parma","Roma","Sampdoria","Sassuolo","SPAL",
    "Torino","Udinese"
    
  )
  
  superlig <- c(
    "Akhisar Belediyespor","Alanyaspor", "Antalyaspor","Medipol Başakşehir FK","BB Erzurumspor","Beşiktaş JK",
    "Bursaspor","Çaykur Rizespor","Fenerbahçe SK", "Galatasaray SK","Göztepe SK","Kasimpaşa SK",
    "Kayserispor","Atiker Konyaspor","MKE Ankaragücü", "Sivasspor","Trabzonspor","Yeni Malatyaspor"
  )
  
  ligue1 <- c(
    "Amiens SC", "Angers SCO", "AS Monaco", "AS Saint-Étienne", "Dijon FCO", "En Avant de Guingamp",
    "FC Nantes", "FC Girondins de Bordeaux", "LOSC Lille", "Montpellier HSC", "Nîmes Olympique", 
    "OGC Nice", "Olympique Lyonnais","Olympique de Marseille", "Paris Saint-Germain", 
    "RC Strasbourg Alsace", "Stade Malherbe Caen", "Stade de Reims", "Stade Rennais FC", "Toulouse Football Club"
  )
  
  eredivisie <- c(
    "ADO Den Haag","Ajax", "AZ Alkmaar", "De Graafschap","Excelsior","FC Emmen","FC Groningen",
    "FC Utrecht", "Feyenoord","Fortuna Sittard", "Heracles Almelo","NAC Breda",
    "PEC Zwolle", "PSV","SC Heerenveen","Vitesse","VVV-Venlo","Willem II"
  )
  
  liganos <- c(
    "Os Belenenses", "Boavista FC", "CD Feirense", "CD Tondela", "CD Aves", "FC Porto",
    "CD Nacional", "GD Chaves", "Clube Sport Marítimo", "Moreirense FC", "Portimonense SC", "Rio Ave FC",
    "Santa Clara", "SC Braga", "SL Benfica", "Sporting CP", "Vitória Guimarães", "Vitória de Setúbal"
  )
  
  # Read FIFA 19 #
  temp <- read.csv("database/fifa19_data.csv", encoding = "UTF-8",stringsAsFactors = FALSE)[-1]
  temp <- read.csv("database/players_fifa23.csv", encoding = "UTF-8",stringsAsFactors = FALSE)[-1]
  teams23 <- read.csv("database/teams_fifa23.csv", encoding = "UTF-8",stringsAsFactors = FALSE)[-1]
  
  temp <- temp %>% mutate(Sliding.Tackle=SlidingTackle)
  temp <- temp %>% mutate(Position=Positions)
  temp <- temp %>% mutate(League = if_else(Club %in% bundesliga, "Bundesliga",
                                    if_else(Club %in% premierLeague, "Premier League", 
                                            if_else(Club %in% laliga, "La Liga", 
                                                    if_else(Club %in% seriea, "Serie A", 
                                                            if_else(Club %in% superlig, "Süper Lig", 
                                                                    if_else(Club %in% ligue1, "Ligue 1", 
                                                                            if_else(Club %in% eredivisie, "Eredivisie",
                                                                                    if_else(Club %in% liganos, "Liga Nos", NA_character_)))))))))
  
  midfielder <- c('CAM', 'CDM','LM', 'CM', 'RM', 'LW', 'RW')
  defender <- c('DM', 'CM', 'DM', 'CB', 'RB', 'LB', 'RWB', 'LWB')
  attacker <- c('CF', 'LF', 'RF', 'ST')                
  temp <- temp %>% mutate(ClubPositionGlobal = if_else(ClubPosition %in% midfielder, "MidFielder",
                                      ifelse(ClubPosition == 'GK', "GoalKeeper", 
                                             ifelse(ClubPosition %in% defender, 'Defender', 
                                                    ifelse(ClubPosition %in% attacker, 'Attacker', 'Other')))))
  temp <- temp %>% mutate(Class = ClubPositionGlobal)
  rv$df <- temp
  
  # Read FIFA Series for Players (2016, 2017, 2018, 2019, 2020)
  f <- read.csv("database/fifa_series_players.csv", encoding = "UTF-8")[-1]
  separate(data = f, col = Name.Pos, into = c("Name", "Position"), sep = ",") -> f
  f21 <- read.csv("database/players_21.csv", encoding = "UTF-8")
  f22 <- read.csv("database/players_22.csv", encoding = "UTF-8")
  f21 <- f21 %>% mutate(Name=short_name, fifa='FIFA 21')
  f22 <- f22 %>% mutate(Name=short_name, fifa='FIFA 22')
  cols <- c('sofifa_id', 'Name', 'team_position', 'value_eur','fifa', 'club')
  f21 <- f21 %>% select(all_of(cols)) %>% rename(Position=team_position)
  f22 <- f22 %>% select(all_of(cols)) %>% rename(Position=team_position)
  f <- rbind(f, f21, f22)
  
  rv$fifa_year <- f
  
  # Read FIFA Series for Teams (2016, 2017, 2018, 2019, 2020)
  f_t <- read.csv("database/fifa_series_teams.csv", encoding = "UTF-8")[-1]
  rv$fifa_team_year <- f_t
# Transfer -------------------------------------------------------------
  
  write.csv(rv$df[0,], "database/transfer.csv", fileEncoding = "UTF-8")
  

# Enable Select Buttons ------------------------------------------------

  
  enable("tl_select")
  enable("tt_select")
  enable("tp_select")
  
})


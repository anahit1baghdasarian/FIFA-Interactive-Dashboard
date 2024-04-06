# PACKAGES -------------------------------------------------------------
source("library/library.R")

# CONFIGURATION --------------------------------------------------------

options(scipen = 999)

#FUNCTIONS ------------------------------------------------------------

source("server/2_functions.R") 


#SIDEBAR --------------------------------------------------------------

sidebar <- tagList(
    sidebarMenu(id="tabs",
                menuItem("Home", tabName = "tab_home", icon = icon("house-user")), 
                menuItem("Leagues", tabName = "tab_leagues", icon = icon("trophy")),
                menuItem("Teams", tabName = "tab_teams", icon = icon("people-group")),
                menuItem("Players", tabName = "tab_players", icon = icon("person")),
                menuItem("Countries", tabName = "tab_countries", icon = icon("globe"))
    )
)

#BODY -----------------------------------------------------------------

body <- tagList(br(), useShinyjs(),useShinyalert(),
                # Slider Color
                chooseSliderSkin(skin="Modern", color = "rosybrown"),
                

                tabItems(

                  # Home
                  source(file.path("ui", "home.R"),  local = TRUE, encoding = "UTF-8" )$value,

                  # League
                  source(file.path("ui", "league.R"),  local = TRUE, encoding = "UTF-8" )$value,

                  # Team
                  source(file.path("ui", "team.R"),  local = TRUE, encoding = "UTF-8" )$value,

                  # Player
                  source(file.path("ui", "player.R"),  local = TRUE, encoding = "UTF-8" )$value,
                  
                  # Country
                  source(file.path("ui", "country.R"),  local = TRUE, encoding = "UTF-8" )$value
  
                )
)


# UI -------------------------------------------------------------------

ui <-  dashboardPage(
  
   skin = 'blue',
    
  # Header
  dashboardHeader(),
  
  
  # Sidebar
  dashboardSidebar(
    width = 220,br(), 
    uiOutput("mySidebarUI")
    ),
  
  # Body
  dashboardBody(
    uiOutput("myBodyUI"),
    tags$head(
      
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), 
      tags$style(HTML("input[type='search']:disabled {visibility:hidden}"),
                 HTML(".shiny-output-error { visibility: hidden; }"), 
                 HTML(".shiny-output-error:before { visibility: hidden; }"),
                 HTML('.small-box .icon-large {font-size: 500%;}'), 
                 HTML(".navbar-default {background-color: #203354; border-color: white;}"),
                 HTML('.navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:focus, .navbar-default .navbar-nav>.active>a:hover {color: #1261A0;background-color: white;}'),
                 HTML(".navbar-default .navbar-nav>li>a:focus, .navbar-default .navbar-nav>li>a:hover {color: white;background-color: #1261A0;}"),
                 HTML(".navbar-default .navbar-nav>li>a {color: white;}"),
                 HTML(".skin-blue .sidebar-menu>li.active>a {border-left-color: white;}"),
                 HTML(".skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li.menu-open>a, .skin-blue .sidebar-menu>li:hover>a {color: #fff;background: #1261A0;}"),
                 HTML(".pl1, .alert-success, .bg-blue, .callout.callout-success, .label-success, .modal-success .modal-body {background-color: crimson;}"),
                 HTML(".bg-blue {color: white;background-color: blue;}"),
                 HTML(".nav-pills>li.active>a, .nav-pills>li.active>a:focus, .nav-pills>li.active>a:hover {color: #fff;background-color: blue;}"),
                 HTML(".nav-pills>li.active>a, .nav-pills>li.active>a:focus, .nav-pills>li.active>a:hover {border-top-color: #203354;}"),
                 HTML(".alert-success, .bg-blue, .callout.callout-success, .label-success, .modal-success .modal-body {background-color: blue!important;}")
                 )
      )
    ),
  
  controlbar=dashboardControlbar()
  )



# SERVER ---------------------------------------------------------------

server <- function(input, output, session) {
  
  # Session
  session$onSessionEnded(stopApp)
  
  # Render UI
  observe({
    output$mySidebarUI <- renderUI({ sidebar })
    output$myBodyUI <- renderUI({  body })
    
    isolate({updateTabItems(session, "tabs", "tab_home")})
    
  })
  
  # Server Operations --------------------------------------------------
  
  
  # 1. Data & Manipulation
  source(file.path("server", "1_tidy_data.R"),  local = TRUE, encoding = "UTF-8")$value
  
  # 2. Tools
  source("server/3_tools.R") 
  
  #3 Observe
  source(file.path("server", "4_observe.R"),  local = TRUE, encoding = "UTF-8")$value

  # 4. League
  source(file.path("server", "5_league.R"),  local = TRUE, encoding = "UTF-8")$value

  # # 5. Team
  source(file.path("server", "6_team.R"),  local = TRUE, encoding = "UTF-8")$value
  # 
  # # 6. Player
  source(file.path("server", "7_player.R"),  local = TRUE, encoding = "UTF-8")$value
  #  
  # # 7. Country
  source(file.path("server", "8_country.R"),  local = TRUE, encoding = "UTF-8")$value
  # 
}


#SHINY APP ------------------------------------------------------------

shinyApp(ui, server)   
# Import Librairies
install.packages("shinydashboard")
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)

# Directory
setwd("/Users/rs777/Documents/Projet Reporting")

# Import Data
twitch <- read.csv("twitchdata.csv")

# Nettoyage : le jeu de données vient de Kaggle, il est plutôt propre
sum(is.na(twitch)) # 0 valeurs manquantes 
sum(duplicated(twitch))  # 0 doublons

# Partie UI : 
library(shiny)
library(shinydashboard)

# CSS pour le mode nuit
night_mode_css <- "
body.night-mode {
  background-color: #1e1e1e;
  color: #ffffff;
}
body.night-mode .content-wrapper, 
body.night-mode .main-sidebar, 
body.night-mode .left-side {
  background-color: #1e1e1e;
}
body.night-mode .skin-green .main-header .navbar {
  background-color: #1e1e1e;
}
body.night-mode .skin-green .main-header .logo {
  background-color: #1e1e1e;
  color: #ffffff;
}
body.night-mode .skin-green .main-sidebar .sidebar-menu>li.active>a {
  background-color: #4c4c4c;
}
body.night-mode .skin-green .main-sidebar .sidebar-menu>li:hover>a {
  background-color: #333333;
}
"

ui <- dashboardPage(
  dashboardHeader(
    title = "Guide d'investissement Twitch",
    titleWidth = 300,
    tags$li(class = "dropdown",
            tags$button(id = "toggleNightMode", "Mode Nuit", class = "btn btn-default")
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "Accueil", icon = icon("home")),
      menuItem("Dictionnaire des données", tabName = "Dictionnaire", icon = icon("book")),
      menuItem("Statistiques", icon = icon("chart-bar"),
               menuSubItem("Statistique descriptive", tabName = "Descriptive"),
               menuSubItem("Statistique bivariée", tabName = "Bivariee")
      ),
      menuItem("Profil des streamers", tabName = "Profil"),
      menuItem("Synthèse", tabName = "Synthese")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(night_mode_css)),
      tags$script(HTML("
        $(document).on('shiny:connected', function() {
          $('#toggleNightMode').on('click', function() {
            $('body').toggleClass('night-mode');
          });
        });
      "))
    ),
    tabItems(
      tabItem(tabName = "Accueil", 
              fluidRow(
                column(width = 12, align = "center",
                       tags$img(src = "twitch.jpeg", height = "200px")
                )
              ),
              fluidRow(
                box(
                  title = "Grande Boîte", width = 12, solidHeader = TRUE, status = "primary",
                  background = "purple", # Couleur de fond
                  p("Contenu de la grande boîte.")
                )
              ),
              fluidRow(
                valueBoxOutput("indicator1", width = 3),
                valueBoxOutput("indicator2", width = 3),
                valueBoxOutput("indicator3", width = 3),
                valueBoxOutput("indicator4", width = 3)
              )
      ),
      tabItem(tabName = "Dictionnaire", 
              h2("Dictionnaire des données"),
              p("Voici une description des données utilisées dans cette application.")
      ),
      tabItem(tabName = "Descriptive", 
              h2("Statistique descriptive"),
              p("Cette section contient des statistiques descriptives sur les données.")
      ),
      tabItem(tabName = "Bivariee", 
              h2("Statistique bivariée"),
              p("Cette section contient des analyses bivariées des données.")
      ),
      tabItem(tabName = "Profil", 
              h2("Profil des streamers"),
              p("Découvrez les profils des différents streamers.")
      ),
      tabItem(tabName = "Synthese", 
              h2("Synthèse"),
              p("Synthèse des principales conclusions.")
      )
    )
  ),
  skin = "green"
)

server <- function(input, output) {
  output$indicator1 <- renderValueBox({
    valueBox(
      value = "42", subtitle = "Indicateur 1", icon = icon("chart-line"),
      color = "aqua"
    )
  })
  output$indicator2 <- renderValueBox({
    valueBox(
      value = "85%", subtitle = "Indicateur 2", icon = icon("percent"),
      color = "green"
    )
  })
  output$indicator3 <- renderValueBox({
    valueBox(
      value = "99", subtitle = "Indicateur 3", icon = icon("thumbs-up"),
      color = "yellow"
    )
  })
  output$indicator4 <- renderValueBox({
    valueBox(
      value = "7", subtitle = "Indicateur 4", icon = icon("users"),
      color = "red"
    )
  })
}

shinyApp(ui, server)





















#################################################### Analyse avant de construire l'app ##########################################################################

# 1- About
# Mettre 3 filtre : nombre d'abonnées, Moyenne viwevers, stream.time.minutes







# 2- Analyse des stremaears

# 3- Profilage


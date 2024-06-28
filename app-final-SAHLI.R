install.packages("fresh")
install.packages("DT")
install.packages("shinyjs")
install.packages("shinycssloaders")
install.packages('rsconnect')
library(rsconnect)
library(shinycssloaders)
library(shiny)
library(shinydashboard)
library(fresh)
library(dplyr)
library(DT)
library(corrplot)
library(ggplot2)
library(shinyjs)
library(FactoMineR)
library(factoextra)

# Directory
setwd("/Users/rs777/Documents/Reporting")


# Import Data
twitchdata <- read.csv("twitchdata.csv")

attach(twitchdata)

# Renommage de variables
twitchdata <- twitchdata %>% rename(Watch_Time = "Watch.time.Minutes.",
                                    Stream_Time = "Stream.time.minutes.",
                                    Peak_viewers = "Peak.viewers",
                                    Average_viewers = "Average.viewers",
                                    Followers_gained = "Followers.gained",
                                    Views_gained = "Views.gained")

# Conversion en heures
twitchdata <- twitchdata %>%
  mutate(Watch_Time = Watch_Time / 60) #Conversion des minutes en heures

# Conversion en heures
twitchdata <- twitchdata %>%
  mutate(Stream_Time = Stream_Time / 60) #Conversion des minutes en heures

## Création de classes dans un nouveau dataset
twitchdata2 <- twitchdata %>%
  mutate(Watch_Time_Class = case_when(
    
    # Watch time
    Watch_Time < 20000000 ~ " 0 - 20",
    Watch_Time >= 20000000 & Watch_Time < 40000000 ~ " 20-40",
    Watch_Time >= 40000000 & Watch_Time < 60000000 ~ " 40-60",
    Watch_Time >= 60000000 & Watch_Time < 80000000 ~ " 60-80",
    Watch_Time >= 80000000 & Watch_Time < 100000000 ~ " 80-100",
    Watch_Time >= 100000000 & Watch_Time < 120000000 ~ "100-120",
    Watch_Time >= 120000000 ~ "120 +"),
    
    
    # Stream time
    Stream_Time_Class = case_when(
      Stream_Time < 1000 ~ "0-1000",
      Stream_Time >= 1000 & Stream_Time < 2000 ~ "1000-2000",
      Stream_Time >= 2000 & Stream_Time < 3000 ~ "2000-3000",
      Stream_Time >= 3000 & Stream_Time < 4000 ~ "3000-4000",
      Stream_Time >= 4000 & Stream_Time < 5000 ~ "4000-5000",
      Stream_Time >= 5000 & Stream_Time < 6000 ~ "5000-6000",
      Stream_Time >= 6000 & Stream_Time < 7000 ~ "6000-7000",
      Stream_Time >= 7000 & Stream_Time < 8000 ~ "7000-8000",
      Stream_Time >= 8000 & Stream_Time < 9000 ~ "8000-9000",
      Stream_Time >= 9000 ~ "9000+"),
    
    # Followers
    Followers_Class = case_when(
      Followers < 500000 ~ " 0-500K",
      Followers >= 500000 & Followers < 1000000 ~ " 500K - 1M",
      Followers >= 1000000 & Followers < 1500000 ~ "1 - 1.5M",
      Followers >= 1500000 & Followers < 2000000 ~ "1.5M - 2M",
      Followers >= 2000000 & Followers < 2500000 ~ "2 - 2.5M",
      Followers >= 2500000 & Followers < 3000000 ~ "2.5M - 3M",
      Followers > 3000000 ~ "3M et +"),
    
    # Peak viewers
    Peak_viewers_Class = case_when(
      Peak_viewers >= 50000 & Peak_viewers < 100000 ~ " 50-99",
      Peak_viewers >= 100000 & Peak_viewers < 150000 ~ "100-149",
      Peak_viewers >= 150000 & Peak_viewers < 200000 ~ "150-199",
      Peak_viewers >= 200000 & Peak_viewers < 250000 ~ "200-249",
      Peak_viewers >= 250000 & Peak_viewers < 300000 ~ "250-299",
      Peak_viewers >= 300000 & Peak_viewers < 350000 ~ "300-349",
      Peak_viewers >= 350000 & Peak_viewers < 400000 ~ "350-399",
      Peak_viewers >= 400000 & Peak_viewers < 450000 ~ "400-449",
      Peak_viewers >= 450000 & Peak_viewers < 500000 ~ "450-499",
      Peak_viewers >= 500000 & Peak_viewers < 550000 ~ "500-549",
      Peak_viewers >= 550000 & Peak_viewers < 600000 ~ "550-599",
      Peak_viewers >= 600000 & Peak_viewers < 650000 ~ "600-649",
      Peak_viewers >= 650000 & Peak_viewers < 700000 ~ "650-699",
      Peak_viewers >= 700000 ~ "700+"),
    
    # Average viewers
    average_viewers_class = case_when(
      Average_viewers >= 0 & Average_viewers <= 10000 ~ " 0-10",
      Average_viewers >= 10000 & Average_viewers <= 29999 ~ " 10-29",
      Average_viewers >= 30000 & Average_viewers <= 59999 ~ " 30-59",
      Average_viewers >= 60000 & Average_viewers <= 99999 ~ "60-99",
      Average_viewers >= 100000 & Average_viewers <= 129999 ~ "100-129",
      Average_viewers >= 130000 ~ "130+"),
    
    # Followers gained
    Followers_gained_class = case_when(
      Followers_gained >= 0 & Followers_gained <= 200000 ~ " 0-200K",
      Followers_gained > 200000 & Followers_gained <= 400000 ~ " 201K-400K",
      Followers_gained > 400000 & Followers_gained <= 600000 ~ " 401K-600K",
      Followers_gained > 600000 & Followers_gained <= 800000 ~ " 601K-800K",
      Followers_gained > 800000 & Followers_gained <= 1000000 ~ " 801K-1M",
      Followers_gained > 1000000 & Followers_gained <= 1200000 ~ "1.1M-1.2M",
      Followers_gained > 1200000 & Followers_gained <= 1400000 ~ "1.2M-1.4M",
      Followers_gained > 1400000 & Followers_gained <= 1600000 ~ "1.4M-1.6M",
      Followers_gained > 1600000 & Followers_gained <= 1800000 ~ "1.6M-1.8M",
      Followers_gained > 1800000 & Followers_gained <= 2000000 ~ "1.8M-2M",
      Followers_gained > 2000000 & Followers_gained <= 2200000 ~ "2M-2.2M",
      Followers_gained > 2200000 & Followers_gained <= 2400000 ~ "2.2M-2.4M",
      Followers_gained > 2400000 & Followers_gained <= 2600000 ~ "2.4M-2.6M",
      Followers_gained > 2600000 & Followers_gained <= 2800000 ~ "2.6M-2.8M",
      Followers_gained > 2800000 & Followers_gained <= 3000000 ~ "2.8M-M",
      Followers_gained > 3000000 ~ "3M +"))

# Création dataframe revenus des streamers
Revenus <- data.frame(
  Spectateurs = c("5-10", "20", "50", "100", "1000", "5000", "10000", "50000+"),
  Revenus = c("50-200$ par mois", "200-400$ par mois", "500-750$ par mois", "1.000-1.500$ par mois", "5.000$ par mois", "13.000$ par mois", "30.000$ par mois", "100.000-200.000+$ par mois")
)

shinyApp(
  ui = dashboardPage(
    
    dashboardHeader(
      title = "Invest in Twitch",
      titleWidth = 300,
      dropdownMenu(
        type = "notifications"
      )
    ),
    
    dashboardSidebar(
      tags$a(href = "https://www.twitch.tv", 
             tags$img(src = "https://www.wideagency.ch/sites/default/files/2020-10/twitch.jpg", height = 160, width = 300)
      ),
      useShinyjs(),                      
      width = 300,
      sidebarMenu(
        menuItem("Accueil", tabName = "Accueil", icon = icon("home")),
        menuItem("Dictionnaire des données", tabName = "Dictionnaire", icon = icon("book")),
        menuItem("Analyse des streamers", icon = icon("chart-simple"),
                 menuSubItem("Analyse descriptive", 
                             tabName = "Descriptive", icon = icon("chart-simple")),
                 menuSubItem("Analyse bivariée", 
                             tabName = "Bivariee", icon = icon("chart-simple"))
        ),
        menuItem("Profil des streamers", tabName = "Profil", icon = icon("user")),
        menuItem("Synthèse", tabName = "Synthese", icon = icon("hourglass-end")), # FIN DU NOM DES PAGES
        
        menuItem(actionButton("quit",label = "Au revoir et à bientôt", width = "80%"))
      )
    ),
    
    dashboardBody(
      tabItems(
        # Page Accueil
        tabItem(tabName = "Accueil",
                h1("Bienvenue sur Invest in Twitch !", style="color:purple"),
                fluidRow(
                  
                  # Box accueil (Page accueil)
                  box(
                    id = "boxaccueil",
                    title = "Présentation de l'application", width = 6, solidHeader = TRUE,
                    background = "purple", # Couleur de fond
                    div(
                      style = "height: 500px; overflow-y: auto;", # Définir la hauteur inline et le défilement
                      tags$h4("Cette application est conçue spécialement pour vous aider à investir de la meilleure des manières dans les streamers Twitch. 
                      Dans l'univers en constante évolution du streaming en direct, choisir les bonnes chaînes sur lesquelles investir peut s'avérer complexe. 
                      Notre application vous offre des outils puissants et des analyses approfondies pour vous aider à prendre des décisions éclairées."),
                      tags$h2("Pourquoi Investir dans Twitch?"),
                      tags$h4("Twitch est la principale plateforme de streaming en direct au monde, attirant des millions de spectateurs chaque jour. 
                      Les streamers populaires y développent des communautés engagées et fidèles, créant ainsi des opportunités uniques pour les investisseurs. 
                      En investissant dans les chaînes Twitch, vous pouvez :"),
                      tags$ul(
                        tags$li("Soutenir des créateurs de contenu prometteurs"),
                        tags$li("Bénéficier d'une visibilité accrue dans une communauté dynamique"),
                        tags$li("Accéder à des revenus potentiels via des partenariats et des collaborations")
                      ),
                      tags$h4("Twitch est la principale plateforme de streaming en direct au monde, attirant des millions de spectateurs chaque jour. 
                      Les streamers populaires y développent des communautés engagées et fidèles, créant ainsi des opportunités uniques pour les investisseurs. 
                      En investissant dans les chaînes Twitch, vous pouvez :"),
                      tags$h4("Nous vous invitons à explorer notre application et à découvrir les chaînes Twitch les plus prometteuses. 
                      Utilisez les outils et les analyses disponibles pour trouver les investissements qui correspondent le mieux à vos objectifs.
                      Merci de faire confiance à notre plateforme pour vous accompagner dans vos décisions d'investissement sur Twitch."),
                      tags$a(
                        href = "https://www.twitch.tv", 
                        class = "btn-twitch-logo",
                        tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/2/26/Twitch_logo.svg", alt = "Twitch Logo", width = "100px",height = "100px")
                      ),
                    ),
                    collapsible = TRUE, collapsed = FALSE
                  ),
                  
                  # Box vidéo (Page accueil)
                  box(
                    id = "boxvideo",
                    title = "Qu'est ce que Twitch ?",
                    width = 6,
                    solidHeader = TRUE,
                    status = "warning",
                    background = "orange",
                    collapsible = TRUE,
                    div(
                      style = "overflow-y: auto;",
                      tags$iframe(
                        width = "100%", 
                        height = "500px",
                        src = "https://www.youtube.com/embed/OWFaV8Nymzs?si=4OzJdBIun6jLf52",
                        frameborder = "0",
                        allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
                        allowfullscreen = TRUE
                      ),
                    ),
                  ),
                  
                  # Box iut (Page accueil)
                  box(
                    id = "boxiut",
                    title = "IUT Paris Rives-de-Seine",
                    width = 6,
                    solidHeader = TRUE,
                    background = "purple",
                    div(
                      style = "overflow-y: auto;",
                      tags$h4("Cette application est un projet réalisée dans le cours 'Reporting d'une Analayse multivariée', au sein du BUT SD 2 (parcours Exploration Modélisation Statistique)."),
                      tags$br(),
                      tags$h4("Author : Rachid SAHLI"),
                      tags$br(),
                      tags$a(
                        href = "https://iutparis-seine.u-paris.fr/", 
                        class = "btn-iut-logo",
                        tags$img(src = "https://u-paris.fr/wp-content/uploads/2022/03/UniversiteParisCite_logo_horizontal_couleur_CMJN.jpg", alt = "Iut Logo", width = "170px",height = "100px")
                      ),
                      tags$a(
                        href = "https://github.com/rachidsahli", 
                        class = "btn-github-logo",
                        tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/9/91/Octicons-mark-github.svg", alt = "Github Logo", width = "80px",height = "80px")
                      ),
                      tags$a(
                        href = "https://www.linedin.com/in/rachidsahli/", 
                        class = "btn-lindin-logo",
                        tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/8/81/LinkedIn_icon.svg", alt = "Lindin Logo", width = "80px",height = "80px")
                      )
                    ),
                  ),
                  
                  # 4 box infos sur le gaming
                  h2("Quelques infos sur le marché du Gaming", style="color:#4B4453"),
                  h4("N'hésitez pas à cliquer sur une box !", style="color:purple"),
                  valueBoxOutput("indicateur1", width = 3),
                  valueBoxOutput("indicateur2", width = 3),
                  valueBoxOutput("indicateur3", width = 3),
                  valueBoxOutput("indicateur4", width = 3)
                ),
        ),
        
        # Page Dictionnaire
        tabItem(tabName = "Dictionnaire",
                fluidPage(
                  h1("Dictionnaire des données", style="color:purple"),
                  h4("Ce jeu de données rassemble les 1000 meilleurs streamers de l'année écoulée ayant diffusé sur Twitch. 
                     Il inclut 11 variables détaillées, fournissant une vue complète et approfondie de leur performance et de leur activité. Il est disponible sur Kaggle."),
                  h4(tags$a(href = "https://www.aggle.com/datasets/aayushmishra1512/twitchdata/data", "Twitch dataset")),
                  
                  # Tableau description des variables
                  DTOutput("dataDictTable"),
                  tags$br(),
                  
                  h1("Un petit aperçu de nos données", style="color:purple"),
                  h4("Obtenez un premier aperçu des données clés nécessaires à une analyse approfondie des investissements potentiels."),
                  
                  # Tableau head des données
                  DTOutput("dataPreview")
                )
        ),
        
        # Page Descriptive
        tabItem(tabName = "Descriptive",
                fluidPage(
                  h1("Analyse descriptive", style = "color:purple"),
                  h4("Explorez les performances clés des streamers Twitch les plus influents de l'année dernière grâce à une analyse descriptive détaillée. Découvrez les audiences, les revenus et les tendances essentielles pour guider vos décisions d'investissement en toute connaissance de cause."),
                  downloadButton("downloadPlot", "Télécharger le graphique"),
                  
                  tabsetPanel(
                    id = "descriptiveTabs",
                    
                    # Onglet vue global (page descriptive)
                    tabPanel("VUE GLOBALE",
                             
                             # box contenant le summary de chaque variable
                             column(width = 4, 
                                    box(
                                      id = "boxrésumé",
                                      title = "Résumé de la variable",
                                      status = "warning",
                                      solidHeader = TRUE,
                                      style = "background-color: #f0f0f0; border: 1px solid #ccc;",
                                      width = NULL,
                                      selectInput("variable", "Choisissez une variable :", 
                                                  choices = names(twitchdata)),
                                      tableOutput("summaryTable")
                                    )
                             ),
                             
                             # box contenant la présentation du summary
                             column(width = 7,
                                    box(
                                      id = "boxaccueil",
                                      title = "Présentation de l'application",solidHeader = TRUE, status = "danger",
                                      background = "red",
                                      div(
                                        tags$h4("Choisissez une variable afin de pouvoir l'explorer graphiquement. Vous disposez également d'un résumé statistiques (minimimum, maximum...).
                                                    Il est important de préciser que sur les milles meilleurs streamers de Twitch, environ 300 se démarque avec des valeurs élevées. La moyenne peut donc 
                                                    être impactée par ces streamers. (La variable Channel n'a pas de représentation graphique)."),
                                      ),
                                      collapsible = TRUE, collapsed = FALSE
                                    )
                             ),
                             
                             # box contenant le graphique descriptif de chaque variable
                             column(width = 12,
                                    box(
                                      id = "graphdescriptif",
                                      title = "Représentation graphique de la variable",
                                      status = "warning",
                                      solidHeader = TRUE,
                                      style = "background-color: #f0f0f0; border: 1px solid #ccc;",
                                      width = NULL,
                                      plotOutput("variablePlot")
                                    )
                             )
                    ),
                    
                    # Onglet temps de visionnage (page descriptive)
                    tabPanel("TEMPS DE VISIONNAGE",
                             fluidRow(
                               # box contenant le graphique de watch time
                               column(width = 7,
                                      box(
                                        id = "graphwatch",
                                        title = "Les chaînes des 1000 meilleurs streamers ont été visionnées pendant combien de temps ?",
                                        status = "warning",
                                        solidHeader = TRUE,
                                        style = "background-color: #f0f0f0; border: 1px solid #ccc;",
                                        width = NULL,
                                        plotOutput("watchTimePlot")
                                      )
                               ),
                               
                               # box contenant l'interpretation de watch time
                               column(width = 5,
                                      box(
                                        id = "interpretationwatch",
                                        title = "Temps total de visionnage", solidHeader = TRUE, status = "warning",
                                        background = "orange",
                                        div(
                                          tags$h4("La majorité des streamers ont un temps total de visionnage total compris entre 0 et 20 millions d'heures. 
                                                  À mesure que le temps de visionnage augmente, le nombre de streamers dans chaque classe diminue de manière significative. 
                                                  Parmi les 1000 meilleurs streamers de Twitch, un tout petit nombre se démarque avec des temps de visionnage très élevé. Ces durées de visionnage sont très élevées. 
                                                  Dans le cadre d'un investissement, il peut être intéressant d'observer les streamers qui ont entre 20 et 60 millions d'heures de visionnage en vue d'une potentielle croissance.")
                                        ),
                                        collapsible = TRUE, collapsed = FALSE
                                      )
                               )
                             ),
                             
                             fluidRow(
                               # box contenant le graphique de stream time
                               column(width = 7,
                                      box(
                                        id = "graphstream",
                                        title = "Combien de temps ont-ils diffusé en direct en 2023 ?",
                                        status = "danger",
                                        solidHeader = TRUE,
                                        style = "background-color: #f0f0f0; border: 1px solid #ccc;",
                                        width = NULL,
                                        plotOutput("streamTimePlot")
                                      )
                               ),
                               
                               # box contenant l'intepretation de stream time
                               column(width = 5,
                                      box(
                                        id = "interpretationstream",
                                        title = "Temps total de diffusion en direct", solidHeader = TRUE, status = "danger",
                                        background = "red",
                                        div(
                                          tags$h4("La classe la plus fréquente est celle des 1000 à 2000 millions d'heures de diffusion en direct. 
                                          Suivie de près par la classe des 2000 à 3000 millions d' heures. À mesure que le temps de stream augmente, le nombre de streamers dans chaque classe diminue, avec quelques fluctuations. Cela reflète probablement les défis et l'engagement nécessaires pour accumuler un temps de stream aussi élevé. 
                                                  Parmi tous ces streamers qui sont les meilleures certains se démarquent par des performances incroyables telles que 8000 millions d'heures de stream.")
                                        ),
                                        collapsible = TRUE, collapsed = FALSE
                                      )
                               )
                             ),
                    ),
                    
                    # Onglet viewers (page descriptive)
                    tabPanel("VIEWERS",
                             fluidRow(
                               # box contenant le tableau des revenus estimés des streamers
                               column(width = 6,
                                      box(
                                        id = "revenus",
                                        title = "Estimation des revenus des streamers", solidHeader = TRUE, status = "success",
                                        style = "background-color: #f0f0f0; border: 1px solid #ccc;",
                                        DTOutput("revenus_table"),
                                        width = NULL
                                      ),
                               ),
                               
                               # box contenant l'intepretation des revenus des streamers
                               column(width = 4,
                                      box(
                                        id = "interpretationrevenus",
                                        title = "Revenus estimés d'un streamer sur Twitch", solidHeader = TRUE, status = "success",
                                        width = NULL,
                                        div(
                                          tags$h4("Les streamers Twitch sont rémunérés en fonction de leur nombre de vues. Ici, nous avons une estimation des revenus des streamers en fonction de leur nombre de vues. 
                                          Source : https://www.shopify.com/fr/blog/comment-gagner-de-largent-sur-twitch. On remarque une forte corrélation entre le nombre de spectateurs et les revenus estimés des streamers sur Twitch. En effet, plus un streamer 
                                          a de spectateurs, plus ses revenus potentiels seront élevés. 
                                          Cela souligne l'importance d'attirer et de fidéliser une grande audience pour maximiser les revenus sur la plateforme.
                                          En conclusion, investir dans des streamers avec une croissance d'audience constante et une base de fans engagée peut offrir des opportunités lucratives sur le marché en expansion du streaming.")
                                        ),
                                        collapsible = TRUE, collapsed = FALSE
                                      ),
                               )
                             ),
                             
                             fluidRow(
                               # box contenant le graphique du pic de streamers
                               column(width = 7,
                                      box(
                                        id = "graphpeakview",
                                        title = "Quels sont les pics de spectateurs des streamers ?",
                                        status = "success",
                                        solidHeader = TRUE,
                                        style = "background-color: #f0f0f0; border: 1px solid #ccc;",
                                        width = NULL,
                                        plotOutput("Peak")
                                      )
                               ),
                               
                               # box contenant l'interpretation du pic de streamers
                               column(width = 5,
                                      box(
                                        id = "interpretationpeakview",
                                        title = "Pic de spectateurs", solidHeader = TRUE, status = "success",
                                        background = "green",
                                        div(
                                          tags$h4("Il y a une concentration majeure dans les classes de 50 à 99 spectateurs, avec une diminution progressive à mesure que le nombre de spectateurs augmente.  
                                          Cela suggère que les streamers qui parviennent à atteindre des niveaux de spectateurs élevés sont rares, mais extrêmement influents et potentiellement très rentables. Les streamers dans les classes supérieures (200+ spectateurs) représentent des opportunités d'investissement particulièrement attractives, 
                                                  offrant une portée et une influence maximales sur la plateforme Twitch.")
                                        ),
                                        collapsible = TRUE, collapsed = FALSE
                                      )
                               )
                             ),
                             # box contenant le graphique du nombres de spectateurs moyens
                             fluidRow(
                               column(width = 7,
                                      box(
                                        id = "graphaverage",
                                        title = "Quel est le nombre de spectateurs moyens des stremers ?",
                                        status = "danger",
                                        solidHeader = TRUE,
                                        style = "background-color: #f0f0f0; border: 1px solid #ccc;",
                                        width = NULL,
                                        plotOutput("average")
                                      )
                               ),
                               
                               # box contenant l'interpretation du nombres de spectateurs moyens
                               column(width = 5,
                                      box(
                                        id = "interpretationaverage",
                                        title = "Nombre de spectateurs moyens", solidHeader = TRUE, status = "danger",
                                        background = "red",
                                        div(
                                          tags$h4("La quasi totalité des meilleurs streamers ont un nombre de spectateurs moyens compris entre 0 et 10000.
                                                           Cependant, quelques stremars arrivent à faire mieux avec un nombre de viewers moyens beaucoup plus élevée. En tant qu'investisseurs,
                                                           Il peut donc être intéressant d'investir chez des streamers qui ont un nombre de spectateurs relativement faible mais qui ont un public engagé et fidèle.
                                                           Il est également important de noter que les 1000 meilleurs streamers n'ont pas un nombre de spectateurs en direct très élevé.")
                                        ),
                                        collapsible = TRUE, collapsed = FALSE
                                      )
                               )
                             ),
                    ),
                    
                    # Onglet abonnées (page descriptive)
                    tabPanel("ABONNÉES",
                             fluidRow(
                               # box contenant le graphique du nombre d'abonnées
                               column(width = 6,
                                      box(
                                        id = "graphefollowers",
                                        title = "Combien d'abonnées ont les 1000 meilleurs streamers ?",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        style = "background-color: #f0f0f0; border: 1px solid #ccc;",
                                        width = NULL,
                                        plotOutput("plotfollowers")
                                      )
                               ),
                               # box contenant l'interpretation du nombre d'abonnées
                               column(width = 6,
                                      box(
                                        id = "interpretationfollowers",
                                        title = "Les abonnées des streamers", solidHeader = TRUE, status = "primary",
                                        div(
                                          tags$h4("Les 1000 meilleurs streamers ont majoritairement entre 0 et 5 millions de followers. 
                                          Il y a également des groupes de streamers moins nombreux, mais avec des niveaux de popularité plus élevés sur la plateforme avec 2 ou 3 millions d'abonnés. 
                                          On peut aussi noter quelques streamers très populaires avec un nombre de followers très élevé qui sont sûrement les leaders de la plateforme. 
                                          En tant qu'investisseur, afin de maximiser son rendement, il peut être intéressant d'investir dans des créateurs en croissance qui pourraient rapidement devenir des figures majeures sur la plateforme avec le bon soutien et les bonnes stratégies de croissance.")
                                        ),
                                        collapsible = TRUE, collapsed = FALSE
                                      )
                               )
                             ),
                             
                             # box contenant le graphique du nombre d'abonnées gagnées
                             box(
                               id = "graphefollowersgained",
                               title = "Combien d'abonnées ont gagnées les streamers l'année passée ?",
                               status = "warning",
                               solidHeader = TRUE,
                               style = "background-color: #f0f0f0; border: 1px solid #ccc;",
                               width = NULL,
                               plotOutput("plotfollowersgained")
                             ),
                             # box contenant l'interpretation du nombre d'abonnées gagnées
                             box(
                               id = "interpretationfollowersgained",
                               title = "Abonnées gagnées par les streamers", solidHeader = TRUE, status = "warning",
                               background = "yellow",
                               div(
                                 tags$h4("71,2 % ont gagné entre 0 et 200 K followers au cours de l'année passée. Cela indique une croissance modérée pour la plupart des créateurs sur la plateforme. Mais un nombre significatif de créateurs montrent des performances exceptionnelles. Certains ont même réussi à gagner plus de 2 millions d'abonnés. Ils représentent de belles opportunités en termes de visibilité et d'influence. 
                                                  L'investissement sur un streamer peut être difficile à prédire avec de nombreux facteurs impactant l'évolution du streamer.")
                               ),
                               collapsible = TRUE, collapsed = FALSE
                             ),
                             # box contenant une valeur visible pour la partie abonnees
                             valueBoxOutput("abonnesdescriptive", width = 5),
                    ),
                    
                    # Onglet langues et partenariats (page descriptive)
                    tabPanel("LANGUES ET PARTENARIATS",
                             
                             # box contenant le graphique de la langue des streamers
                             column(width = 7,
                                    box(
                                      id = "graphelangue",
                                      title = "Quelles sont les langues les plus parlées par les meilleurs streamers ? ",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      style = "background-color: #f0f0f0; border: 1px solid #ccc;",
                                      width = NULL,
                                      plotOutput("plotlangue")
                                    )
                             ),
                             # box contenant l'interpretation de la langue des streamers
                             column(width = 5,
                                    box(
                                      id = "interpretationlangue",
                                      title = "Langue des streamers", solidHeader = TRUE,
                                      background = "purple",
                                      div(
                                        tags$h4("La langue la plus représentée est sans aucune surprise avec plus de 600 streamers parlant cette langue. Ensuite, les autres langues sont plus ou moins
                                                  au même niveau.")
                                      ),
                                      collapsible = TRUE, collapsed = FALSE
                                    )
                             ),
                             
                             fluidRow(
                               # box contenant le graphique mature
                               column(width = 6,
                                      box(
                                        id = "graphemature",
                                        title = "Jeu +18",
                                        status = "warning",
                                        solidHeader = TRUE,
                                        style = "background-color: #f0f0f0; border: 1px solid #ccc;",
                                        width = NULL,
                                        plotOutput("plotmature")
                                      )
                               ),
                               # box contenant l'interpretation mature ou non
                               column(width = 6,
                                      box(
                                        id = "interpretationlangue",
                                        title = "Jeu +18 ?", solidHeader = TRUE, status = "warning",
                                        background = "red",
                                        div(
                                          tags$h4("L'écrasante majorité des jeux joués par les 1000 meilleurs streamers ne sont pas dans la catégorie +18.
                                                  Cela laisse donc penser que leurs succès n'y sont pas liés.")
                                        ),
                                        collapsible = TRUE, collapsed = FALSE
                                      )
                               )
                             ),
                             
                             fluidRow(
                               # box contenant le graphique du partenariat
                               column(width = 6,
                                      box(
                                        id = "graphepartnered",
                                        title = "Partenaire de Twitch",
                                        status = "success",
                                        solidHeader = TRUE,
                                        style = "background-color: #f0f0f0; border: 1px solid #ccc;",
                                        width = NULL,
                                        plotOutput("plotpartenaire")
                                      )
                               ),
                               # box contenant l'interpretation du partenariat
                               column(width = 6,
                                      box(
                                        id = "interpretationlangue",
                                        title = "Partenaire de Twitch ?", solidHeader = TRUE, status = "success",
                                        div(
                                          tags$h4("Un streamer partenaire de Twitch signifie qu'il bénéficie d'un certain nombre d'avantages.
                                                  Il peut par exemple être mis en avant par la plateforme afin de le faire découvrir à un plus grand nombre.
                                                  Pratiquement, les 1000 meilleurs streamers de Twitch sont partenaires de la plateforme. Cela peut donc contribuer fortement à leur succès.")
                                        ),
                                        collapsible = TRUE, collapsed = FALSE
                                      )
                               )
                             ),
                    ),
                  ),
                )
        ),
        # Page Descriptive
        tabItem(tabName = "Bivariee",
                fluidPage(
                  h1("Analyse bivariée", style = "color:purple"),
                  h4("Explorez les corrélations et les relations clés entre les différentes métriques des streamers Twitch les plus influents de l'année dernière grâce à une analyse bivariée approfondie. 
                      Découvrez comment les variables telles que le temps de streaming, le nombre de followers et les revenus interagissent 
                     entre elles, afin de guider vos décisions d'investissement avec des insights précis et informés."),
                  tabsetPanel(id = "bivarieetabs",
                              
                              # Onglet corrélation (page bivariee)
                              tabPanel("CORRÉLATION",
                                       # box contenant la matrice de corrélation
                                       column(width = 8, 
                                              box(
                                                id = "boxgraphcorrelation",
                                                title = "Matrice de corrélation",
                                                status = "info",
                                                solidHeader = TRUE,
                                                style = "background-color: #f0f0f0; border: 1px solid #ccc;",
                                                width = NULL,
                                                plotOutput("matricecorrelation")
                                              ),
                                       ),
                                       valueBoxOutput("matrice1", width = 4),
                                       valueBoxOutput("matrice2", width = 4),
                                       # box contenant l'interpretation de la matrice de corrélation
                                       column(width = 12,
                                              box(
                                                id = "interpretationcorrelation",
                                                title = "Qu'est ce que la matrice de corrélation ?",solidHeader = TRUE, status = "danger",
                                                div(
                                                  tags$h4("On utilise une matrice de corrélation pour mesurer la dépendance entre toutes nos variables en même temps. Dans chaque carrée, il y a le coefficient de corrélation entre deux variables. 
                                                                 Cet outil statistique nous montre la force et la direction de la relation entre deux ou plusieurs variables."),
                                                  tags$h4("Comment s'intèrprète t-il ?"),
                                                  tags$br(),
                                                  tags$h4("Les coefficients sont compris entre -1 et 1 : "),
                                                  tags$ul(
                                                    tags$li("Si le résultat obtenu est proche de 0 : la corrélation linéaire est nulle"),
                                                    tags$li("Si le résultat obtenu est proche de -1 : la corrélation linéaire est forte mais négative."),
                                                    tags$li("Si le résultat obtenu est proche de 1 : la corrélation linéaire est forte et positive.")
                                                  )
                                                ),
                                                collapsible = TRUE, collapsed = FALSE
                                              ))
                              ),
                              
                              # Onglet followers vs nombres de vues (page bivariee)
                              tabPanel("FOLLOWERS VS NOMBRES DE VUES",
                                       # box contenant graphique bivariee
                                       column(width = 8,
                                              box(
                                                id = "boxgraphfol",
                                                title = "Le nombre d'abonnées influence-t-il le nombre de vues moyen ?",
                                                status = "success",
                                                solidHeader = TRUE,
                                                style = "background-color: #f0f0f0; border: 1px solid #ccc;",
                                                width = NULL,
                                                
                                                sliderInput("vues", "Nombre de Vues Moyennes :", 
                                                            min = min(twitchdata$Average_viewers), 
                                                            max = max(twitchdata$Average_viewers), 
                                                            value = c(min(twitchdata$Average_viewers), max(twitchdata$Average_viewers)),
                                                            step = 1  # Définir le step à 1 pour n'accepter que des valeurs entières
                                                ),
                                                
                                                plotOutput("plotfolview")
                                              )
                                       ),
                                       valueBoxOutput("plotfolview1", width = 4),
                                       valueBoxOutput("plotfolview2", width = 4),
                                       # box contenant interpretation de graphique bivariee
                                       column(width = 11,
                                              box(
                                                id = "interpretationfol",
                                                title = "Interpretation",solidHeader = TRUE, status = "success",
                                                div(
                                                  tags$h4("Le nombre de vue moyen est légèrement influencé par les followers. Il y a une concentration de streamers autour des 10 000 vues moyens avec entre 1M et 2m d'abonnées.
                                                          (Bougez le curseur pour mieux apercevoir la relation sans les valeurs extrêmes.).
                                                          En tant qu'investisseur, il est important de prendre en compte la liaison entre ces deux variables.
                                                          Cependant, on voit ici que le nombre de followers n'influence pas énormément le nombre de vues moyen."),
                                                ),
                                                collapsible = TRUE, collapsed = FALSE))
                                       
                              ),
                              
                              # Onglet followers vs langue (page bivariee)
                              tabPanel("FOLLOWERS VS LANGUE",
                                       # box contenant graphique bivariee
                                       column(width = 8, 
                                              box(
                                                id = "boxgraphvuelangue",
                                                title = "La langue du streamer influence t-elle le nombre de vues ?",
                                                status = "warning",
                                                solidHeader = TRUE,
                                                style = "background-color: #f0f0f0; border: 1px solid #ccc;",
                                                width = NULL,
                                                selectInput("langue", "Choisir une langue :", 
                                                            choices = c("Toutes les langues", unique(twitchdata$Language))),
                                                plotOutput("plotfollangue"))
                                       ),
                                       valueBoxOutput("languevue", width = 4),
                                       valueBoxOutput("languevue2", width = 4),
                                       valueBoxOutput("languevue3", width = 4),
                                       # box contenant interpretation de graphique bivariee
                                       column(width = 7,
                                              box(
                                                id = "interpretationvuelangue",
                                                title = "Boîtes à moustaches ?",solidHeader = TRUE, status = "success",
                                                background = "green",
                                                div(
                                                  tags$h4("Outil visuel utile pour 
                                                          comprendre la distribution et la variation des mesures dans nos streamers, la répartition de la langue en fonction du nombres de vues."),
                                                ),
                                                collapsible = TRUE, collapsed = FALSE)),
                                       # box contenant graphique bivariee n*2
                                       
                              ),
                  ),
                  
                  # Onglet temps de visionnage vs abonnees (page bivariee)
                  tabPanel("TEMPS DE VISIONNAGE VS ABONNEES"))
        ),
        tabItem(tabName = "Profil",
                fluidPage(
                  h2("Profil des streamers", style = "color:purple"),
                  h4("Ici, vous trouverez une analyse plus approfondie des streamers."),
                  h4("Cette page contient l'algorithme des k-means et l'analyse en composantes principales (ACP) pour regrouper et visualiser les caractéristiques des streamers. Explorez les données et découvrez des insights 
                     fascinants sur les tendances et les comportements des créateurs de contenu les plus influents de la plateforme."),
                  
                  # Graphe de l'ACP
                  box(
                    id = "acpgraph",
                    title = "Analyses en composantes principales (ACP)",
                    solidHeader = TRUE,
                    status = "success",
                    background = "green",
                    plotOutput("plotgraphacp"),
                    collapsible = TRUE,
                    collapsed = FALSE
                  ),
                  # Widget pour le nombre de clusters (K-means)
                  box(
                    id = "kmeans_controls",
                    title = "Clustering K-means",
                    solidHeader = TRUE,
                    status = "danger",
                    numericInput("num_clusters", "Nombre de clusters", min = 2, max = 10, value = 3)
                  ),
                  infoBox(
                    title = "Qu'est ce que les k-means ?",
                    value = "Méthode informatique qui aide à regrouper des données en plusieurs groupes en fonction de leurs similitudes, e
                    n s'assurant que chaque groupe est aussi compact que possible.",
                    icon = icon("calculator"),
                    color = "red",
                  ),
                  
                  # Graphique du clustering K-means
                  box(
                    id = "kmeans_plot",
                    title = "Résultats du clustering K-means",
                    solidHeader = TRUE,
                    status = "danger",
                    plotOutput("cluster_plot")
                  ),
                  infoBox(
                    title = "Qu'est ce que l'ACP ?",
                    value = "C'est une méthode statistique permettant de réduire la dimensionnalité des données tout en préservant au mieux la variance des variables originales.",
                    icon = icon("chart-bar"),
                    color = "orange",
                  ),

                  # Résumé de l'analyse PCA
                  box(
                    id = "acpvaleurs",
                    title = "Résumé de l'analyse PCA",
                    solidHeader = TRUE,
                    status = "warning",
                    background = "yellow",
                    collapsible = TRUE,
                    collapsed = FALSE,
                    verbatimTextOutput("summary_output")
                  )
                  
                  
                )
        ),
        tabItem(tabName = "Synthese",
                fluidPage(
                  h2("Synthèse", style = "color:purple"),
                  h4("Bienvenue sur la page 'Synthèse' de notre application RShiny, spécialement conçue pour aider les investisseurs à identifier les streamers Twitch les plus prometteurs. Ici, nous compilons et analysons les informations essentielles issues des différentes analyses, notamment descriptives et bivariées, ainsi que les profils détaillés des streamers. Cette synthèse vous fournira une vue d'ensemble claire et concise, facilitant ainsi des décisions d'investissement éclairées et stratégiques."),
                  
                  # Infoboxes
                  fluidRow(
                    infoBox(
                      title = "Total des streamers analysés",
                      value = "1 000",
                      icon = icon("users"),
                      color = "purple"
                    ),
                    infoBox(
                      title = "TOP 1 Langue",
                      value = "Anglais",
                      icon = icon("language"),
                      color = "green"
                    ),
                    infoBox(
                      title = "Plateforme n°1",
                      value = "Les streamers sur la plateforme Twitch font partie des plus importants",
                      icon = icon("twitch"),
                      color = "blue"
                    )
                  ),
                  fluidRow(
                    infoBox(
                      title = "Les gros streamers gagnent le plus d'abonnées",
                      value = "Forte corrélation entre le nombre d'abonnées et les abonnées gagnées",
                      icon = icon("chart-line"),
                      color = "yellow"
                    ),
                    infoBox(
                      title = "Parenaire de Twitch ?",
                      value = "Il est conseillé d'investir dans des streamers partenaires",
                      icon = icon("thumbs-up"),
                      color = "red"
                    ),
                    infoBox(
                      title = "Streamers émergents",
                      value = "150 streamers sont en pleine croissance !",
                      icon = icon("chart-bar"),
                      color = "orange"
                    )
                  )
                ),
                fluidRow(
                  box(
                    title = "Merci !",
                    status = "success",
                    solidHeader = TRUE,
                    width = 12,
                    tags$img(src = "https://img.freepik.com/vecteurs-libre/merci-pour-votre-illustration-signe-attention_23-2150308632.jpg?w=1380&t=st=1719501893~exp=1719502493~hmac=ed3aebc18f9e98294881d6106c59f991b07e1f355676e9ce49091772ec33d58c", 
                             height = "900px", width = "100%")
              )
            ),
        )
      ),
      
    ),
    skin = "purple"
  ),
  
  
server = function(input, output, session) {
    
    # Bouton quitter de l'application
    observeEvent(input$quit, {
      showModal(modalDialog(
        title = "Confirmation",
        "Êtes-vous sûr de vouloir fermer l'application ?",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Annuler"),
          actionButton("confirmQuit", "Oui")
        )
      ))
    })
    
    observeEvent(input$confirmQuit, {
      removeModal()
      stopApp()
    })
    
    # Indicateur 1 (page d'accueil)
    output$indicateur1 <- renderValueBox({
      valueBox(
        value = "1.8B", subtitle = "JOUEURS DE GAMING À TRAVERS LE MONDE", icon = icon("gamepad"),href = "https://fr.statista.com/themes/9063/le-marche-du-jeu-video/#topicOverview",
        color = "red"
      )
    })
    
    # Indicateur 2 (page d'accueil)
    output$indicateur2 <- renderValueBox({
      valueBox(
        value = "187B", subtitle = "RECETTES DU SECTEUR DU JEU VIDEO EN 2023", icon = icon("dollar-sign"),href = "https://www.lindependant.fr/2023/12/28/bilan-complet-dune-annee-2023-completement-dingue-en-matiere-de-jeux-video-11667038.php",
        color = "yellow"
      )
    })
    
    # Indicateur 3 (page d'accueil)
    output$indicateur3 <- renderValueBox({
      valueBox(
        value = "10%", subtitle = "CROISSANCE SUR LE MARCHÉ DU GAMING EN 2023",href = "https://www.lesechos.fr/tech-medias/hightech/jeu-video-record-historique-pour-le-marche-francais-en-2023-2084039", icon = icon("chart-line"),
        color = "green"
      )
    })
    
    # Indicateur 4 (page d'accueil)
    output$indicateur4 <- renderValueBox({
      valueBox(
        value = "530M", subtitle = "TÉLÉSPÉCTATEURS CHAQUE ANNÉE", icon = icon("eye"),
        color = "light-blue"
      )
    })
    
    output$abonnesdescriptive <- renderValueBox({
      valueBox(
        value = "Entre 10k et 200K", subtitle = "d'abonnées gagnées l'année dernière par la majorité des streamers", icon = icon("eye"),
        color = "red"
      )
    })
    
    # Indicateur Follower (page descriptive onglet abonnees)
    output$matrice1 <- renderValueBox({
      valueBox(
        value = "Forte corrélation (+)", subtitle = "Nombre d'abonnées vs Nombre d'abonnées gagnées (0.72)", icon = icon("person"),
        color = "green"
      )
    })
    
    output$matrice2 <- renderValueBox({
      valueBox(
        value = "Forte corrélation (-)", subtitle = "Temps de stream vs Nombre de spectateurs moyens (-0,25)", icon = icon("eye"),
        color = "red"
      )
    })
    
    output$plotfolview1 <- renderValueBox({
      valueBox(
        value = "Très légère influence", subtitle = "du nombre de followers sur le nombre de vue moyen", icon = icon("person"),
        color = "yellow"
      )
    })
    
    output$plotfolview2 <- renderValueBox({
      valueBox(
        value = "Best score", subtitle = "147 643 vues moyen et 483 530 abonnées (dota2ti)", icon = icon("person"),
        color = "purple"
      )
    })
    
    output$languevue <- renderValueBox({
      valueBox(
        value = "Influence de la langue", subtitle = "L'anglais et l'espagnol sont deux langues largement employées. Il en résulte beaucoup de vues pour elles.", icon = icon("language"),
        color = "orange"
      )
    })
    
    output$languevue2 <- renderValueBox({
      valueBox(
        value = "48,5%", subtitle = "des 1000 meilleurs streamers parle anglais", icon = icon("language"),
        color = "red"
      )
    })
    
    output$languevue3 <- renderValueBox({
      valueBox(
        value = "L'anglais gagne le + d'abonnées", subtitle = "", icon = icon("person"),
        color = "green"
      )
    })
    
    
    
    # Contenu du tableau de description de chaque variable
    data_dict <- data.frame(
      Variable = c("Channel", "Watch_time", " Stream_time",
                   "Peak_viewers","Average_viewers","Followers",
                   "Followers_gained","Views_gained","Partnered",
                   "Mature","Language"),
      Signification = c("Chaîne twitch du streamer", "Temps total de visionnage de la chaîne en heures",
                        "Temps total de streaming de la chaîne en heures","Pic du nombre de vues sur la chaîne",
                        "Nombre moyen de vues lors d'un stream","Nombre de followers de la chaîne",
                        "Nombre de followers gagnés par la châine durant l'an passée","
                        Nombre de vues gagnées par la châine durant l'an passée","Le streamer est-il partenaire de twitch ?",
                        "Le jeu streamer est-il dans la catégorie des 18 ans et +","Langue du stream"),
      Type = c("Qualitative","Quantitative","Quantitative","Quantitative","Quantitative",
               "Quantitative","Quantitative","Quantitative","Qualitative","Qualitative","Qualitative"))
    
    # Tableau explication de chaque variable
    output$dataDictTable <- renderDT({
      datatable(data_dict, 
                options = list(
                  stripeClasses = list('striped', 'bg-purple')
                )
      )
    })
    
    # Head aperçu des données
    output$dataPreview <- renderDT({
      datatable(head(twitchdata, 50), 
                options = list(
                  stripeClasses = list('striped', 'bg-gray')
                ),
      )
    })
    
    # Tableau summary pour chaque variable
    output$summaryTable <- renderTable({
      selected_variable <- input$variable
      if (is.null(input$variable) || input$variable == "Channel") return("Cette variable est catégorielle et nous ne disposons pas de résumé statistique pour ce type de variable.")
      if (is.null(input$variable) || input$variable == "Partnered") return("Cette variable est catégorielle et nous ne disposons pas de résumé statistique pour ce type de variable.")
      if (is.null(input$variable) || input$variable == "Mature") return("Cette variable est catégorielle et nous ne disposons pas de résumé statistique pour ce type de variable.")
      if (is.null(input$variable) || input$variable == "Language") return("Cette variable est catégorielle et nous ne disposons pas de résumé statistique pour ce type de variable.")
      if (!is.null(selected_variable) && selected_variable != "") {
        summary_stats <- summary(twitchdata[[selected_variable]])
        summary_df <- data.frame(
          Statistique = c("Minimum", "1st Quartile", "Médiane", "Moyenne", "3rd Quartile", "Maximum"),
          Valeur = as.vector(summary_stats)
        )
        summary_df
      }
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Graphique descriptif de chaque variable
    output$variablePlot <- renderPlot({
      if (is.null(input$variable) || input$variable == "Channel") return(NULL)
      
      var_data <- twitchdata[[input$variable]]
      
      if (is.numeric(var_data)) {
        hist(var_data,
             main = paste("Histogramme de", input$variable),
             xlab = input$variable,
             ylab = "Count",
             col = "#1f77b4",
             border = "white",
             breaks = 30)  # 30 bins pour l'histogramme
      } else if (is.factor(var_data) || is.character(var_data)) {
        barplot(table(var_data),
                main = paste("Diagramme en barres de", input$variable),
                xlab = input$variable,
                ylab = "Count",
                col = "#2ca02c",
                border = "white")
      } else {
        plot(var_data,
             main = paste("Graphique de", input$variable),
             xlab = input$variable,
             ylab = "",
             col = "#d62728",
             pch = 19)
      }
    })
    
    output$downloadPlot <- downloadHandler(
      filename = function() {
        "graphique.png"
      },
      content = function(file) {
        ggsave(file, plot = output$variablePlot())
      }
    )
    
    # Graphique de la distribution du temps de visionnage 
    output$watchTimePlot <- renderPlot({
      Watc_freq <- table(twitchdata2$Watch_Time_Class)
      barplot(Watc_freq, main = "Répartition du temps de visionnage des chaînes des streamers", ylab = "Streamers",xlab = "Heures (en Million)", col = c('blue','red','yellow',"orange",'brown','purple'))
    })
    
    # Graphique de la distribution du temps de streaming
    output$streamTimePlot <- renderPlot({
      Stream_freq <- table(twitchdata2$Stream_Time_Class)
      barplot(Stream_freq, main = "Répartition du temps de streaming des les chaînes des streamers",ylab = "Streamers", xlab = "Heures (en Million)", col = c('blue','red','yellow',"orange",'brown','purple',
                                                                                                                                                              'green','magenta','beige'))
    })
    
    # Tableau des revenus des streamers
    output$revenus_table <- renderDT({
      datatable(Revenus)
    })
    
    # Graphique distribution du pic de spectateurs
    output$Peak<- renderPlot({
      Peak_freq <- table(twitchdata2$Peak_viewers_Class)
      barplot(Peak_freq, main = 'Répartition des pics de spectateurs', ylab = "Streamers", xlab = 'Pic de spectateurs (en milliers)', col = c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf'))
    })
    
    # Graphique distribution des spectateurs moyens
    output$average <- renderPlot({
      average_freq <- table(twitchdata2$average_viewers_class)
      # Plot
      barplot(average_freq,
              main = 'Répartition du nombre de spectateurs moyens',
              xlab = 'Nombres moyens de spectateurs (en milliers)',
              ylab = "Streamers",
              col = c('#92c6ff', '#97f0aa', '#ff9f9a', '#d0bbff', '#fffea3', '#ffdfba'),
              ylim = c(0, 1000))
    })
    
    # Graphique distribution des followers
    output$plotfollowers <- renderPlot({
      followers_freq <- table(twitchdata2$Followers_Class)
      barplot(followers_freq, main = "Répartition du nombre d'abonnées des streamers", ylab = 'Streamers',xlab = 'Abonnées', col = c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b'))
    })
    
    #Graphique distribution des followers gagnées
    output$plotfollowersgained <- renderPlot({
      followersgained_freq <- table(twitchdata2$Followers_gained_)
      barplot(followersgained_freq, main = "Répartition du nombre d'abonnées gagnées par les streamers", ylab = 'Streamers',xlab = 'Abonnées gagnées', col = c('#e41a1c', '#377eb8', '#4daf4a', '#984ea3', '#ff7f00', '#ffff33'))
    })
    
    # Graphique distribution de la langue
    output$plotlangue <- renderPlot({
      language_counts <- table(twitchdata2$Language)
      language_counts <- as.data.frame(language_counts)
      colnames(language_counts) <- c("Language", "Freq")
      
      language_counts <- language_counts[order(-language_counts$Freq), ]
      
      top_languages <- head(language_counts, 10)
      
      lang_names <- top_languages$Language
      freqs <- top_languages$Freq
      
      barplot(
        freqs,
        names.arg = lang_names,
        main = 'Les 10 langues les plus parlées par les streamers',
        xlab = 'Langue',
        ylab = 'Fréquence',
        col = rainbow(length(freqs))
      )
    })
    
    # Graphique statut mature
    output$plotmature <- renderPlot({
      mature_counts <- table(twitchdata$Mature)
      pie(mature_counts, labels=c('Not Mature', 'Mature'), main='Distribution des chaînes matures', col=c('lightcoral', 'lightyellow'))
    })
    
    # Graphique statut de partenaire
    output$plotpartenaire <- renderPlot({
      partnered_counts <- table(twitchdata$Partnered)
      pie(partnered_counts, labels=c('Non-Partnered', 'Partnered'), main='Distribution des chaînes partenaires de Twitch', col=c('lightblue', 'lightgreen'))
    })
    
    # Matrice de corrélations
    output$matricecorrelation <- renderPlot({
      numeric_columns <- sapply(twitchdata, is.numeric)
      numeric_data <- twitchdata[, numeric_columns]
      cor_matrix <- cor(numeric_data)
      corrplot(cor_matrix, type = "upper", tl.col = "black", tl.srt = 45, method = "color", addCoef.col = "black")
    })
    
    # Boxplot des followers en fonction des langues
    output$plotfollangue <- renderPlot({
      if (input$langue == "Toutes les langues") {
        ggplot(data = twitchdata, aes(x = Language, y = Followers, fill = Language)) +
          geom_boxplot() +
          scale_y_continuous(labels = scales::comma, breaks = seq(0, 8000000, by = 1000000)) +
          labs(x = "Langue", y = "Followers", title = "Distribution des Followers par Langue") +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        ggplot(data = twitchdata[twitchdata$Language == input$langue, ], aes(x = Language, y = Followers, fill = Language)) +
          geom_boxplot() +
          scale_y_continuous(labels = scales::comma, breaks = seq(0, 8000000, by = 1000000)) +
          labs(x = "Langue", y = "Followers", title = paste("Distribution des Followers pour la langue", input$langue)) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    })
    
    followers_gained_by_language <- reactive({
      if (input$langue == "Toutes les langues") {
        aggregate(Followers_gained ~ Language, data = twitchdata, sum)
      } else {
        aggregate(Followers_gained ~ Language, data = twitchdata[twitchdata$Language == input$langue, ], sum)
      }
    })
    
    # Graphique nb de followers gagnées en fonction de la langue
    output$plotfolgainedlangue <- renderPlot({
      barplot(followers_gained_by_language()$Followers_gained,
              names.arg = followers_gained_by_language()$Language,
              col = "orange",
              main = "Nombre de Followers Gagnés par Langue",
              ylab = "Followers Gagnés",
              las = 2,  # Rotate x-axis labels
              cex.axis = 0.7)  # Shrink x-axis labels
    })
    
    # Graphique relation entre vues moyennes et followers
    output$plotfolview <- renderPlot({
      filtered_data <- twitchdata[twitchdata$Average_viewers >= input$vues[1] & twitchdata$Average_viewers <= input$vues[2], ]
      plot(filtered_data$Average_viewers, filtered_data$Followers,
           xlab = "Vues Moyennes",
           ylab = "Followers",
           main = "Relation entre Vues Moyennes et Followers",
           pch = 14, col = "lightgreen")
    })
    
    data_num <- reactive({
      twitchdata %>% select_if(is.numeric)
    })
    
    data_num <- reactive({
      twitchdata %>% select_if(is.numeric)
    })
    
    # Analyse PCA et affichage du graphique
    output$plotgraphacp <- renderPlot({
      res <- PCA(data_num())
      fviz_pca_ind(res, geom = "point", col.ind = "contrib")
    })
    
    # Résumé de l'analyse PCA
    output$summary_output <- renderPrint({
      res <- PCA(data_num())
      summary(res)
    })
    
    # Clustering K-means
    kmeans_result <- reactive({
      kmeans(data_num(), centers = input$num_clusters)
    })
    
    # Affichage du graphique de clustering K-means
    output$cluster_plot <- renderPlot({
      res <- kmeans_result()
      fviz_cluster(res, data = data_num(), geom = "point", stand = FALSE)
    })
    
  }
)


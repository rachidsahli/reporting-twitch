library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

library(tidyr)
iris

iris_table_longue = iris %>% gather(-Species, key = "var", value = "Valeur")

df = iris_table_longue %>% group_by(Species, var) %>% 
  summarise(
    Moyenne = round(mean(Valeur, na.rm = T),2)
  )


ui = dashboardPage(
  dashboardHeader(
    title = h3("Les iris de Fisher"),
    titleWidth = 230
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Résumé des informations", 
               tabName  = "resume"
      ),
      menuItem("Description des données",
               tabName = "description"
      )
    )
  ),
  dashboardBody(
    tabItems( 
      tabItem("resume",
              valueBox(
                value = "Mesures sur Sépale et Pétale",
                subtitle = "Etude des variables",
                icon = icon("leaf"),
                color = "yellow",
                width = 8
              ),
              infoBox(
                title = "Les données",
                value = textOutput("donnees"),
                icon = icon("leaf"),
                color = "yellow",
                width = 4
              ),
              box(
                title = "Distribution des espèces",
                plotOutput("diagramme"),
                width = 6
              ),
              tabBox(
                title = "Résumé des mesures",
                width = 6,
                tabPanel(title = "Largeur de Sépale", 
                         dataTableOutput("sepal_width")
                ),
                tabPanel(title = "Largeur de Pétale", 
                         dataTableOutput("petal_width")
                )
              )),
      tabItem("description",
              box(
                title = "Distribution",
                plotOutput("histo")),
              box(
                selectInput("choix",
                            "Variable à visualiser",
                            list("Choisir une variable" = 1,
                                 "Largeur de Sepale" = 2,
                                 "Longueur de Sepale" = 3,
                                 "Largeur de Petale" = 4,
                                 "Longueur de Petale" = 5
                            )
                )
              )
              
      ) 
      
    ),
    title = "Informations sur les iris de Fisher",
    skin = "purple"
  ))

server = function(input, output) {
  
  variable_a_afficher = reactive({
    if(input$choix == 2) var = "Sepal.Width"
    if(input$choix == 3) var = "Sepal.Length"
    if(input$choix == 4) var = "Petal.Width"
    if(input$choix == 5) var = "Petal.Length"
    var
  })
  
  
  
  
  output$donnees = renderText({
    paste(c(nrow(iris)," iris, dont ",
            iris %>% filter(Species=="setosa") %>% nrow,
            "Setosa, ",
            iris %>% filter(Species=="versicolor") %>% nrow,
            "Versicolor et ",
            iris %>% filter(Species=="virginica") %>% nrow,
            "Virginica"
    )
    )
  })
  
  output$diagramme = renderPlot({
    ggplot(iris, aes("", fill=Species)) +
      geom_bar(aes(y = ..count.. / sum(..count..) * 1)) +
      labs(y = "Pourcentage", fill = "Espèce", x = NULL, title = "Distribution des espèces en pourcentage") +
      scale_y_continuous(labels = scales::percent_format())
  })
  
  
  output$sepal_width = renderDataTable({
    datatable(
      data.frame(
        df %>% filter(var == "Sepal.Width"))
    ) 
    
  })
  
  
  output$petal_width = renderDataTable({
    datatable(
      data.frame(df %>% filter(var == "Petal.Width"))
      
    )
  })
  output$histo = renderPlot({
    if(input$choix>1){
      ggplot(iris, aes_string(variable_a_afficher()))+
        geom_histogram(aes(y=..density..),
                       bins=input$nb_classe,
                       colour = "red",
                       fill = "steelblue"
        )+
        labs(x = "",
             y = "Densite")+
        theme_light()} 
  })   
  
}

shinyApp(ui = ui, server = server)

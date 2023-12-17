

amsterdam <- read.csv("../data/amsterdam_weekends.csv")
athens <- read.csv("../data/athens_weekends.csv")
barcelona <- read.csv("../data/barcelona_weekends.csv")
berlin <- read.csv("../data/berlin_weekends.csv")
budapest <- read.csv("../data/budapest_weekends.csv")
lisbon <- read.csv("../data/lisbon_weekends.csv")
london <- read.csv("../data/london_weekends.csv")
paris <- read.csv("../data/paris_weekends.csv")
rome <- read.csv("../data/rome_weekends.csv")
vienna <- read.csv("../data/vienna_weekends.csv")

cityList = list(amsterdam, athens, barcelona, berlin, budapest, lisbon, london, paris, rome, vienna)
colorVector = c("orange", "blue", "red", "yellow", "chartreuse1", "seagreen", "brown", "thistle","cadetblue1", "gold1")

combined_data <- bind_rows(cityList, .id = "city")

combined_data$city <- replace(combined_data$city, combined_data$city==1, "Amsterdam")
combined_data$city <- replace(combined_data$city, combined_data$city==2, "Athènes")
combined_data$city <- replace(combined_data$city, combined_data$city==3, "Barcelone")
combined_data$city <- replace(combined_data$city, combined_data$city==4, "Berlin")
combined_data$city <- replace(combined_data$city, combined_data$city==5, "Budapest")
combined_data$city <- replace(combined_data$city, combined_data$city==6, "Lisbonne")
combined_data$city <- replace(combined_data$city, combined_data$city==7, "Londres")
combined_data$city <- replace(combined_data$city, combined_data$city==8, "Paris")
combined_data$city <- replace(combined_data$city, combined_data$city==9, "Rome")
combined_data$city <- replace(combined_data$city, combined_data$city==10, "Vienne")

avg_price_vector = c()
avg_dist_vector = c()
max_dist_vector = c()

cityListNames = c("Amsterdam","Athènes","Barcelone","Berlin","Budapest","Lisbonne","Londres","Paris","Rome","Vienne")

for (i in cityList) {
  avg_price_vector <- append(avg_price_vector, round(mean(i$realSum), digits=2))
  avg_dist_vector <- append(avg_dist_vector, mean(i$dist))
  max_dist_vector <- append(max_dist_vector, max(i$dist))
}

global = data.frame(city <- cityListNames, avg_realSum <- avg_price_vector, avg_dist <- avg_dist_vector, city_size <- max_dist_vector)

combined_data_by_density <- combined_data %>% arrange(factor(city, levels = c('Amsterdam', 'Berlin', 'Barcelone', 'Vienne', 'Budapest', 'Athènes', 'Lisbonne', 'Paris', 'Rome', 'Londres')))
density_values <- density(combined_data_by_density$realSum)
peak_x <- density_values$x[which.max(density_values$y)]

## app.R ##
library(shinydashboard)

ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Airbnb Evaluator"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
      menuItem("Base de données", tabName = "donnée", icon = icon("database")),
      menuItem("Comparaison villes entre elles ",tabName = "compaVille",icon = icon("balance-scale")),
      menuItem("Airbnb dans une même ville  ",tabName = "airbnb",icon = icon("balance-scale")),
      menuItem("satisfaction",tabName = "satis", icon = icon("balance-scale")),
      conditionalPanel(
        condition = "input.sidebar === 'donnée'",
        tagList(selectInput(inputId = "data_d",
                            label = "choisir :",
                            choices = c("amsterdam", "athens", "barcelona",
                                        "berlin", "budapest", "lisbon",
                                        "london", "paris", "rome", "vienna"),
                            selected = "amsterdam"
                            )
                
        )
       ),
      conditionalPanel(
        condition = "input.sidebar === 'airbnb'",
        tagList(selectInput(inputId = "data_a",
                            label = "choisir :",
                            choices = c("amsterdam", "athens", "barcelona",
                                        "berlin", "budapest", "lisbon",
                                        "london", "paris", "rome", "vienna"),
                            selected = "amsterdam"
        )
        
        )
      ),
      conditionalPanel(
        condition = "input.sidebar === 'satis'",
        tagList(selectInput(inputId = "data_s",
                            label = "choisir :",
                            choices = c("amsterdam", "athens", "barcelona",
                                        "berlin", "budapest", "lisbon",
                                        "london", "paris", "rome", "vienna"),
                            selected = "amsterdam"
        )
        
        )
      )
      
      )
  ),
  ## Body content
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "donnée", DT::dataTableOutput(outputId = "cityTable")),
      tabItem(tabName ="satis",fluidRow(
        column(width = 12,
               tabBox(title = span(icon("circle-plus"), "Comparaison des données de satisfaction"),
                      width = 12,
                      selected = "note",
                      tabPanel("note", 
                               plotOutput("noteprop")
                      ),
                      tabPanel("satisfaction", 
                               plotOutput("satisfac")
                      ),
                      tabPanel("satisfaction/propreté", 
                               plotOutput("satiprop")
                      )
               )
               
        )
      ),h2("description")
      ),
      
      tabItem(tabName ="compaVille",fluidRow(
        column(width = 12,
               tabBox(title = span(icon("circle-plus"), "comparaison des villes entre elles"),
                      width = 12,
                      selected = "prix",
                      tabPanel("prix", 
                               plotOutput("prixdiste")
                      ),
                      tabPanel("prix moyen", 
                               plotOutput("prixmoyen")
                      ),
                      tabPanel("prix moyens ville", 
                               plotOutput("prixmoyenville")
                      ),
                      tabPanel("prix probable", 
                               plotOutput("densiter")
                      )
               )
        )
      )
      ),
      
       tabItem(tabName ="airbnb",fluidRow(
        column(width = 12,
               tabBox(title = span(icon("circle-plus"), "variation du prix en fonction de leur proximité du centre-ville, variation de la taille et du nombre de chambre"),
                      width = 12,
                      selected = "prix",
                      tabPanel("prix", 
                               plotOutput("prixdist")
                      ),
                      tabPanel("arrêt de metro", 
                               plotOutput("metro")
                      ),
                      tabPanel("Nombre personnes", 
                               plotOutput("pers")
                      )
              )
      )
    )
  )
)
)
)


server <- function(input, output) {
  
  
  
  # Chargement des données en fonction de la ville sélectionnée
  selected_city_data_d <- reactive({
    switch(input$data_d,
           "amsterdam" = read.csv("../data/amsterdam_weekends.csv"),
           "athens" = read.csv("../data/athens_weekends.csv"),
           "barcelona" = read.csv("../data/barcelona_weekends.csv"),
           "berlin" = read.csv("../data/berlin_weekends.csv"),
           "budapest" = read.csv("../data/budapest_weekends.csv"),
           "lisbon" = read.csv("../data/lisbon_weekends.csv"),
           "london" = read.csv("../data/london_weekends.csv"),
           "paris" = read.csv("../data/paris_weekends.csv"),
           "rome" = read.csv("../data/rome_weekends.csv"),
           "vienna" = read.csv("../data/vienna_weekends.csv"))
    
  })
  selected_city_data_a <- reactive({
    switch(input$data_a,
           "amsterdam" = read.csv("../data/amsterdam_weekends.csv"),
           "athens" = read.csv("../data/athens_weekends.csv"),
           "barcelona" = read.csv("../data/barcelona_weekends.csv"),
           "berlin" = read.csv("../data/berlin_weekends.csv"),
           "budapest" = read.csv("../data/budapest_weekends.csv"),
           "lisbon" = read.csv("../data/lisbon_weekends.csv"),
           "london" = read.csv("../data/london_weekends.csv"),
           "paris" = read.csv("../data/paris_weekends.csv"),
           "rome" = read.csv("../data/rome_weekends.csv"),
           "vienna" = read.csv("../data/vienna_weekends.csv"))
    
  })
  selected_city_data_s <- reactive({
    switch(input$data_s,
           "amsterdam" = read.csv("../data/amsterdam_weekends.csv"),
           "athens" = read.csv("../data/athens_weekends.csv"),
           "barcelona" = read.csv("../data/barcelona_weekends.csv"),
           "berlin" = read.csv("../data/berlin_weekends.csv"),
           "budapest" = read.csv("../data/budapest_weekends.csv"),
           "lisbon" = read.csv("../data/lisbon_weekends.csv"),
           "london" = read.csv("../data/london_weekends.csv"),
           "paris" = read.csv("../data/paris_weekends.csv"),
           "rome" = read.csv("../data/rome_weekends.csv"),
           "vienna" = read.csv("../data/vienna_weekends.csv"))
    
  })
  
  
  output$cityTable <- DT::renderDataTable({
    DT::datatable(selected_city_data_d()[, 1:7], 
                  options = list(pageLength = 10), 
                  rownames = FALSE) 
  })
  
  output$prixdist <- renderPlot({
    ggplot(selected_city_data_a(),aes(x=dist, y=realSum))+
      geom_point(color="blue")+
      # Regression line
      geom_smooth(method = "lm", se = FALSE, color = "red")+
      labs(title="Prix des Airbnb en fonction de la distance au
         centre-ville",
           x="Distance au centre-ville",
           y="Prix total de l'annonce") +
      theme_minimal()
    
  }) 
  
  output$metro <- renderPlot({
    ggplot(selected_city_data_a(), aes(x = dist)) +
      
      # Price in relation to distance to the nearest metro station
      geom_line(aes(y = realSum, color = "blue"), size = 1) +
      
      # Distance to the city center on the secondary y-axis
      geom_line(aes(y = metro_dist, color = "red"), size = 1) +
      
      scale_y_continuous(
        name = "Prix total de l'annonce",
        sec.axis = sec_axis(~., name = "Distance au métro le plus proche")
      ) +
      
      labs(
        title = "Prix des Airbnb en fonction de la distance au métro et au centre-ville",
        x = "Distance au centre-ville"
      ) +
      
      theme_minimal() +
      scale_color_manual(values = c("blue", "red"))
  })
  
  
  output$pers <- renderPlot({
    ggplot(selected_city_data_a(), aes(x = dist)) +
      geom_histogram(binwidth = 1, fill = "#0072B2", color = "white") +
      
      labs(
        title = "Histogramme du nombre de personnes en fonction de la distance",
        x = "Distance au centre-ville",
        y = "Nombre de personnes"
      ) +
      
      theme_minimal()
  })
  
  output$noteprop <- renderPlot({
    ggplot(selected_city_data_s(),
           mapping = aes(x = cleanliness_rating, color = room_type, fill = room_type)) +
      geom_bar() +
      facet_wrap(vars(room_type))+
      labs(title = "la note de propreté que les  clients ont doonné",
           x = "Note de la propreté",
           y = "Compte")+
      theme_minimal()
  })
  
  output$satisfac <- renderPlot({
    ggplot(selected_city_data_s(), mapping = aes(x = guest_satisfaction_overall, color = room_type, fill = room_type)) +
      geom_bar() +
      facet_wrap(vars(room_type))+
      labs(title = "satisfaction des clients",
           x = "Satisfaction",
           y = "Compte")+
      theme_minimal()
  })
  
  output$satiprop <- renderPlot({
    ggplot(selected_city_data_s(), mapping = aes(x = guest_satisfaction_overall,y=cleanliness_rating,color=guest_satisfaction_overall))+
      geom_point()+
      labs(title = "satisfaction en fonction de la propreté",
         x = "Note de la satisfaction",
         y = "Note de la propreté")
  })
  
  output$prixdiste <- renderPlot({
    ggplot(combined_data, aes(x = dist, y = realSum, color = city)) +
      geom_point(alpha = 0) +
      labs(
        title = "Prix des Airbnbs en fonction de la distance au centre-ville en lignes de Regression",
        x = "Distance au centre-ville",
        y = "Prix total de l'annonce",
        color = "Ville"
      ) +
      scale_color_manual(values = colorVector) +
      theme_minimal() +
      geom_smooth(method = "loess", formula = y ~ x) +
      facet_wrap(~city, scales = "free_y") +
      ylim(0,1000)
  })
  
  output$prixmoyen <- renderPlot({
    ggplot(global, aes(x = avg_realSum, y = avg_dist, color = city, size = city_size)) +
      geom_point() +
      labs(
        title = "Prix moyen d'un Airbnb par ville en fontion de la distance moyenne",
        x = "Prix moyen",
        y = "Distance moyenne",
        color = "Ville",
        size = "Rayon de présence"
      ) +
      scale_color_manual(values = colorVector) +
      theme_minimal()
  })
  
  output$prixmoyenville <- renderPlot({
    ggplot(global, aes(x = reorder(city, avg_realSum), y = avg_realSum, fill = city)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label=avg_realSum), vjust=-0.3, size=3.5) +
      labs(
        title = "Prix moyen d'un Airbnb par ville",
        y = "Prix",
        color = "Ville"
      ) +
      scale_color_manual(values = colorVector) +
      theme_minimal()
  })
  output$densiter <- renderPlot({
    ggplot(combined_data, aes(x = realSum, after_stat(count), fill = city)) +
      geom_density(position="stack") +
      labs(
        title = "Prix des Airbnb",
        x = "Prix total de l'annonce",
        color = "Ville"
      ) +
      scale_color_manual(values = colorVector) +
      theme_minimal() +
      xlim(0,2300) +
      geom_vline(xintercept = peak_x, color = "red") +
      annotate("text", x = peak_x+650, y = 100, label = sprintf("Valeur la plus probable à %.2f", peak_x), vjust = -1)
  })
  
}

shinyApp(ui, server)

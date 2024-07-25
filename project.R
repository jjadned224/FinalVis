library(shiny)
library(ggplot2)
library(dplyr)
library(treemapify)
library(plotly)
library(tidyr)

ui <- fluidPage(
  tags$h2("Warhammer 40k Toughness Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("chosen_faction", "Choose a faction:",
                  choices = c("All", unique(read.csv("creatures.csv", header = TRUE)$Faction))),
      uiOutput("chosen_army")),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Comparison of Vehicle and Monster Toughness", plotOutput("creature_comp", width = "100%", height = "500px")),
        tabPanel("Comparison of Weapon Strength", plotOutput("weapon_comp", width = "100%", height = "500px")),
        tabPanel("Highest Probability to Hit", plotOutput("hits_gone_through", width = "100%", height = "500px")),
        tabPanel("Highest Probability to Damage", plotOutput("damage_gone_through", width = "100%", height = "500px")),
        tabPanel("Comparison of Health Vs. Toughness", plotOutput("health_v_toughness", width = "100%", height = "500px")),
        tabPanel("Units Weapon Analysis", plotOutput("units_weapon_analysis", width = "100%", height = "500px"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  creaturesData <- reactive({
    read.csv("creatures.csv", header = TRUE)
  })
  
  weaponsData <- reactive({
    req(input$chosen_faction)
    read.csv("weapons.csv", header = TRUE)
  })
  
  unitsData <- reactive({
    read.csv("units.csv", header = TRUE)
  })
  
  hitsData <- reactive({
    hits <- read.csv("hits.csv", header = TRUE)
    if (input$chosen_faction != "All") {
      hits <- hits[hits$Faction == input$chosen_faction, ]
    }
    if (!is.null(input$chosen_army) && input$chosen_army != "All") {
      hits <- hits[hits$Army == input$chosen_army, ]
    }
    hits <- hits[hits$Hits > 0, ]
    hits
  })
  
  damageData <- reactive({
    damage <- read.csv("hits.csv", header = TRUE)
    if (input$chosen_faction != "All") {
      damage <- damage[damage$Faction == input$chosen_faction, ]
    }
    if (!is.null(input$chosen_army) && input$chosen_army != "All") {
      damage <- damage[damage$Army == input$chosen_army, ]
    }
    damage <- damage[damage$Damage > 0, ]
    damage
  })
  
  observe({
    updateSelectInput(session, "chosen_faction", choices = c("All", unique(creaturesData()$Faction)))
  })
  
  output$chosen_army <- renderUI({
    req(input$chosen_faction)
    if (input$chosen_faction != "All") {
      army_choices <- sort(unique(creaturesData() %>%
                                    filter(Faction == input$chosen_faction) %>%
                                    pull(Army)))
      selectInput("chosen_army", "Choose an army:",
                  choices = c("All", army_choices))
    } else {
      NULL
    }
  })
  
  filteredcreaturesData <- reactive({
    req(input$chosen_faction)
    if (input$chosen_faction == "All") {
      filtered_data <- creaturesData()
    } else {
      filtered_data <- creaturesData() %>%
        filter(Faction == input$chosen_faction)
    }
    
    if (!is.null(input$chosen_army) && input$chosen_army != "All") {
      filtered_data <- filtered_data %>%
        filter(Army == input$chosen_army)
    }
    
    filtered_data %>%
      arrange(Army, Name)
  })
  
  
  filteredUnits <- reactive({
    req(input$chosen_faction)
    if (input$chosen_faction == "All") {
      filtered_data <- unitsData()
    } else {
      filtered_data <- unitsData() %>%
        filter(Faction == input$chosen_faction)
    }
    
    if (!is.null(input$chosen_army) && input$chosen_army != "All") {
      filtered_data <- filtered_data %>%
        filter(Army == input$chosen_army)
    }
    
    filtered_data %>%
      arrange(Army, Weapon)
  })
  
  output$creature_comp <- renderPlot({
    p <- ggplot(filteredcreaturesData(), aes(x = Name, y = Toughness)) +
      geom_bar(stat = "identity") +
      labs(title = "Toughness of Units", x = "Unit", y = "Toughness") +
      theme_grey() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.width = unit(1, "npc")) +
      coord_cartesian(ylim = c(0, max(filteredcreaturesData()$Toughness) * 1.1))
    
    if (input$chosen_faction == "All") {
      p <- p + aes(fill = Faction) +
        theme(axis.text.x = element_blank()) +
        theme(legend.position = "bottom")
    } else if (input$chosen_army == "All") {
      p <- p + aes(fill = Army) +
        theme(axis.text.x = element_blank()) +
        theme(legend.position = "bottom")
    } else {
      p <- p + aes(fill = Name) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12)) +
        theme(legend.position = "none")
    }
    
    p
  })
  
  output$weapon_comp <- renderPlot({
    ggplot(weaponsData(), aes(x = Name, y = Strength, fill = Name)) +
      geom_bar(stat = "identity") +
      labs(title = "Weapon Strength", x = "Weapon", y = "Strength") +
      theme_grey() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12)) +
      theme(legend.position = "none")
  })
  
  output$hits_gone_through <- renderPlot({
    hits_filtered <- hitsData()
    if (nrow(hits_filtered) > 0) {
      if (input$chosen_faction == "All") {
        ggplot(hits_filtered, aes(area = Hits, fill = Faction, label = Weapon)) +
          geom_treemap() +
          geom_treemap_text(colour = "white", place = "middle", reflow = TRUE) +
          labs(title = "Hits Penetrating Creatures by Weapon") +
          theme_gray() + theme(plot.title = element_text(hjust = 0.5))
      } else if (input$chosen_army == "All") {
        ggplot(hits_filtered, aes(area = Hits, fill = Army, label = Weapon)) +
          geom_treemap() +
          geom_treemap_text(colour = "white", place = "middle", reflow = TRUE) +
          labs(title = "Hits Penetrating Creatures by Weapon") +
          theme_gray() + theme(plot.title = element_text(hjust = 0.5))
      } else {
        ggplot(hits_filtered, aes(area = Hits, fill = Unit, label = Weapon)) +
          geom_treemap() +
          geom_treemap_text(colour = "white", place = "middle", reflow = TRUE) +
          labs(title = "Hits Penetrating Creatures by Weapon") +
          theme_gray() + theme(plot.title = element_text(hjust = 0.5))
      }
    } else {
      ggplot() + theme_gray() + theme(plot.title = element_text(hjust = 0.5)) +
        ggtitle("No Data Available")
    }
  })
  
  output$damage_gone_through <- renderPlot({
    damage_filtered <- damageData()
    if (nrow(damage_filtered) > 0) {
      if (input$chosen_faction == "All") {
        ggplot(damage_filtered, aes(area = Damage, fill = Faction, label = Weapon)) +
          geom_treemap() +
          geom_treemap_text(colour = "white", place = "middle", reflow = TRUE) +
          labs(title = "Damage Taken by Creatures") +
          theme_gray() + theme(plot.title = element_text(hjust = 0.5))
      } else if (input$chosen_army == "All") {
        ggplot(damage_filtered, aes(area = Damage, fill = Army, label = Weapon)) +
          geom_treemap() +
          geom_treemap_text(colour = "white", place = "middle", reflow = TRUE) +
          labs(title = "Damage Taken by Creatures") +
          theme_gray() + theme(plot.title = element_text(hjust = 0.5))
      } else {
        ggplot(damage_filtered, aes(area = Damage, fill = Unit, label = Weapon)) +
          geom_treemap() +
          geom_treemap_text(colour = "white", place = "middle", reflow = TRUE) +
          labs(title = "Damage Taken by Creatures") +
          theme_gray() + theme(plot.title = element_text(hjust = 0.5))
      }
    } else {
      ggplot() + theme_minimal() + ggtitle("No Data Available")
    }
  })
  
  output$health_v_toughness <- renderPlot({
    ggplot(filteredcreaturesData(), aes(x = Toughness, y = Health)) +
      geom_point(aes(color = Name, size = Health)) +
      scale_size_area() +
      labs(title = "Health vs Toughness", x = "Toughness", y = "Health") +
      theme_gray() + theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = "none")
  })
  
output$units_weapon_analysis <- renderPlot({
  p <- ggplot(filteredUnits(), aes(x = Weapon, y = Shots)) +
    geom_bar(stat = "identity") +
    labs(title = "Shots of Weapons", x = "Weapon", y = "Shots") +
    theme_grey() +
    theme(plot.title = element_text(hjust = 0.5))
  
  if (input$chosen_faction == "All") {
    p <- p + aes(fill = Faction) +
      facet_wrap(~ Faction, scales = "free", nrow = 1) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
            legend.position = "bottom")
  } else if (input$chosen_army == "All") {
    p <- p + aes(fill = Army) +
      facet_wrap(~ Army, scales = "free", nrow = 1) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
            legend.position = "none")
  } else {
    p <- p + aes(fill = Weapon) +
      facet_wrap(~ Army, scales = "free", nrow = 1) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
            legend.position = "none")
  }
  
  p
})

  

  
}

shinyApp(ui, server)

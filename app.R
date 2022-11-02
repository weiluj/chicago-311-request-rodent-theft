#--------------------
# Objective: PPHA 30536 HW2
# Date: 23rd Oct, 2022
#--------------------

# Access Shiny App Here: https://weiluj-uchicago.shinyapps.io/chciago-city-dashboard-theft-rodent/

# Clear Global Environment
rm(list = ls())
options(
  scipen = 999,
  digits = 3
)

# Load packages
library(scales)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)
library(sf)
library(spData)
library(shinyWidgets)

# User Interface
ui <- fluidPage(
  setBackgroundColor(color = "#FFF5EE"),
  titlePanel(div("Chicago City Dashboard", style = "color: steelblue")),
  sidebarLayout(
    sidebarPanel(
      selectInput("type", "Select Data Type", choices = c("theft", "311 service rodent complain")),
      selectInput("method", "Select Calculation Method", choices = c("total", "annual")),
      sliderInput("year", "Select a Year", value = 2019, min = 2019, max = 2022, step = 1),
      radioButtons("street", "Add Major Street", choices = c("Yes", "No")),
      submitButton("Refresh")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", plotOutput("sf", height = 650)),
        tabPanel("Top Case Table", plotOutput("top_case_area", height = 650))
      )
    )
  )
)

# Server
server <- function(input, output) {
  df <- st_read("merged_theft_rodent_sf.shp")
  major_street <- st_read("Major_Streets.shp") %>%
    select(geometry)
  
  data <- reactive({
    df %>%
      filter(year == input$year & type == input$type & cal_type == input$method)
  })
  street <- reactive({
    major_street
  })
  
  output$sf <- renderPlot({
    plt <- ggplot() +
      geom_sf(data = data(), aes(fill = case)) +
      scale_y_continuous(labels = comma) +
      labs(
        title = str_to_title(str_c(input$type, "Cases in Chicago", input$year, sep = " ")),
        fill = element_blank(),
        caption = "Source: City of Chicago"
      ) +
      scale_fill_distiller(
        palette = "RdPu",
        direction = 1
      ) +
      theme_void() +
      theme(
        plot.title = element_text(
          size = 20, face = "bold",
          vjust = -0.5
        ),
        legend.text = element_text(size = 12),
        legend.key.size = unit(1.5, "cm"),
        plot.caption = element_text(
          size = 12, face = "italic",
          vjust = 10, hjust = 1.5
        )
      )
    
    plt_street <- plt + geom_sf(
      data = street(), color = "dodgerblue3",
      alpha = 0.8, size = 0.6
    )
    
    ifelse(input$street == "Yes",
           print(plt_street),
           print(plt)
    )
  })
  
  output$top_case_area <- renderPlot({
    ggplot(data() %>%
             ungroup() %>%
             top_n(5, case) %>%
             arrange(desc(case))) +
      geom_col(aes(x = community, y = case), fill = "skyblue") +
      scale_y_continuous(labels = comma) +
      labs(
        title = str_to_title(str_c("Top", input$type, "Cases in Chicago", input$year, sep = " ")),
        x = "Community",
        y = "Case Number",
        color = element_blank(),
        caption = "Source: City of Chicago"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          size = 20, face = "bold",
          vjust = -0.5, hjust = 0.5
        ),
        plot.caption = element_text(
          size = 10, face = "italic",
          vjust = 5, hjust = 1
        ),
        axis.title = element_text(
          size = 14, face = "bold"
        ),
        axis.text = element_text(
          size = 10
        )
      )
  })
}

# Run Shiny App
shinyApp(ui = ui, server = server)

library(shiny)
library(bslib)
library(tidyverse)

root = "B:/50 - OVG/OVG7/analyses/results/MoMo"
data = read.csv2(paste0(root, "/OVG7_ModalSplit.csv"))

motieven = distinct(data, motief11) %>% pull()
hoofdmodus = distinct(data, hfdvm2) %>% pull()

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  # App title ----
  title = "Modale split",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    title = "Filters",
    position = "right",
    # Input: Slider for the number of bins ----
    sliderInput(
      inputId = "bins",
      label = "Number of bins:",
      min = 1,
      max = 50,
      value = 30
    ),
    selectInput(inputId = "motiefSelect", 
                label = "Motieven", 
                choices = motieven, 
                selected = c("werken"), multiple = F, selectize = TRUE)
  ),
  # Output: Histogram ----
  card(
    card_header("Enkele statistieken"),
    layout_columns(
      value_box(title = "Aantal verplaatsingen",
                value = textOutput("aantalVerplaatsingen")),
      value_box(title = "Steekproefgrootte",
                value = 23421)
    )
  ),
  navset_card_tab(
    nav_panel("Motief",
              plotOutput(outputId = "distPlot")),
    nav_panel("Afstand",
              "Hier komt een andere grafiek")
  ),
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
  output$aantalVerplaatsingen = renderText({
    as.character(data %>%  summarise(n = n()) %>%  pull())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


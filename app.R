
library(shiny)
library(bslib)
library(tidyverse)
library(shinyWidgets)
library(DT)
library(plotly)

data = read.csv2("./data/OVG7_ModalSplit.csv")

motieven = distinct(data, motief11) %>% pull()
hoofdmodus = distinct(data, hfdvm2) %>% pull()
meettype = distinct(data, MeetType) %>% pull()
DagType = distinct(data, DagType) %>% pull()

# graph_data = data %>% 
#   filter(DagType == "alle dagen" & MeetType == "in aantal verplaatsingen per dag") %>% 
#   mutate(totaal = sum(waarde_modal_split)) %>% 
#   group_by(hfdvm2) %>% 
#   summarise(percentage = round(sum(waarde_modal_split) / mean(totaal) * 100, digits = 1)) %>% 
#   arrange(desc(percentage)) %>%
#   mutate(hfdvm2 = factor(hfdvm2, levels = rev(hfdvm2))) %>% 
#   as.data.frame()

# plot_ly(data = graph_data) %>%
#   add_segments(x = 0, xend = ~percentage,
#                y = ~hfdvm2, yend = ~hfdvm2,
#                line = list(color = 'gray'),
#                showlegend = FALSE) %>%
#   add_markers(x = ~percentage, y = ~hfdvm2,
#               marker = list(color = 'steelblue', size = 10),
#               text = ~paste0(percentage, "%"),
#               hoverinfo = "text") %>%
#   layout(
#     title = "Modal Split of Trips",
#     xaxis = list(title = "Percentage of trips"),
#     yaxis = list(title = "", categoryorder = "array", categoryarray = rev(graph_data$hfdvm2)),
#     margin = list(l = 100)
#   )


# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  
  # App title ----
  title = "Modale split",
  
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    title = "Filters",
    position = "right",
    # Input: Slider for the number of bins ----
    # sliderInput(
    #   inputId = "bins",
    #   label = "Number of bins:",
    #   min = 1,
    #   max = 50,
    #   value = 30
    # ),
    
    radioButtons(
      inputId = "indicatorSelect", 
      label = "Selecteer indicator",
      choices = meettype, 
      selected = "in aantal verplaatsingen per dag"
      
    ),
    
    selectInput(
      inputId = "motiefSelect", 
      label = "Motieven", 
      choices = motieven, 
      selected = c("werken"), multiple = F, selectize = TRUE
    ),

  
    selectInput(
      inputId = "dagtypeSelect", 
      label = "Type dag", 
      choices = DagType, 
      selected = c("alle dagen"), multiple = F, selectize = TRUE
    ),
    
    virtualSelectInput(
      inputId = "hoofdmodusSelect",
      label = "Hoofdmodus",
      choices = list(
        "Duurzaam" = c("trein", "te voet", "elektrische fiets", "niet-elektrische fiets", "tram", "bus"),
        "Niet-duurzaam" = c("autobestuurder", "autopassagier -18", "brom-/snorfiets of motor")
      ),
      selected = c("trein", "te voet", "elektrische fiets", "niet-elektrische fiets", "tram", "bus",
                   "autobestuurder", "autopassagier -18", "brom-/snorfiets of motor"),
      showValueAsTags = F,
      search = F,
      multiple = TRUE
    )
    
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
    # nav_panel("Grafiek",
    #           plotOutput(outputId = "modaleSplit")),
    nav_panel("Grafiek",
              plotlyOutput(outputId = "plotly")),
    nav_panel("Tabel",
              dataTableOutput(outputId = "table"))
  ),
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # output$modaleSplit = renderPlot({
  # 
  # 
  #   graph_data = data %>%
  #     filter(DagType == input$dagtypeSelect & MeetType == input$indicatorSelect) %>%
  #     mutate(totaal = sum(waarde_modal_split)) %>%
  #     group_by(hfdvm2) %>%
  #     summarise(percentage = round(sum(waarde_modal_split) / mean(totaal) * 100, digits = 1)) %>%
  #     filter(hfdvm2 %in% input$hoofdmodusSelect) %>%
  #     arrange(desc(percentage)) %>%
  #     mutate(hfdvm2 = factor(hfdvm2, levels = rev(hfdvm2)))
  # 
  #   ggplot(graph_data, aes(x = percentage, y = hfdvm2)) +
  #     geom_segment(aes(x = 0, xend = percentage, y = hfdvm2, yend = hfdvm2), color = "grey70") +
  #     geom_point(color = "steelblue", size = 4) +
  #     labs(x = "Percentage", y = "Hoofdmodus") +
  #     theme_minimal() +
  #     theme(axis.text=element_text(size=14, face = 'bold'))
  # 
  # })
  
  output$plotly = renderPlotly({
    
    graph_data2 = data %>% 
      filter(DagType == input$dagtypeSelect & MeetType == input$indicatorSelect) %>% 
      mutate(totaal = sum(waarde_modal_split)) %>% 
      group_by(hfdvm2) %>% 
      summarise(percentage = round(sum(waarde_modal_split) / mean(totaal) * 100, digits = 1)) %>% 
      filter(hfdvm2 %in% input$hoofdmodusSelect) %>% 
      arrange(desc(percentage)) %>%
      mutate(hfdvm2 = factor(hfdvm2, levels = rev(hfdvm2))) %>% 
      as.data.frame()
    
    annotations = lapply(1:nrow(graph_data2), function(i) {
      list(
        x = graph_data2$percentage[i],
        y = graph_data2$hfdvm2[i],
        text = paste0(graph_data2$percentage[i], "%"),
        xanchor = "left",
        yanchor = "middle",
        xshift = 10, 
        showarrow = FALSE,
        bgcolor = "steelblue",       # background color
        borderpad = 2,
        font = list(size = 12, color = "white")
      )
    })
    
    plot_ly(data = graph_data2) %>%
      add_segments(x = 0, xend = ~percentage,
                   y = ~hfdvm2, yend = ~hfdvm2,
                   line = list(color = 'gray'),
                   showlegend = FALSE) %>%
      add_markers(x = ~percentage, y = ~hfdvm2,
                  marker = list(color = 'steelblue', size = 10),
                  showlegend = FALSE,
                  hover = "none") %>%
      layout(
        title = "Modal Split of Trips",
        xaxis = list(title = "Percentage of trips"),
        yaxis = list(title = "", categoryorder = "array", categoryarray = rev(graph_data2$hfdvm2)),
        margin = list(l = 100),
        annotations = annotations
      )
    
  })

  
  output$table <- 
    renderDataTable({datatable(graph_data)}) 
  
  output$aantalVerplaatsingen = renderText({
    graph_data = data %>% 
      filter(DagType == input$dagtypeSelect & MeetType == input$indicatorSelect) %>% 
      filter(hfdvm2 %in% input$hoofdmodusSelect)
    
    as.character(graph_data %>%  summarise(verplaatsingen = sum(verplaatsingen)) %>%  pull())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


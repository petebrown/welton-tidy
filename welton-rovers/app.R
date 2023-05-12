library(shiny)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(plotly)
library(DT)

source("./R/get_data.R")
source("./R/get_streaks.R")
source("./R/get_pts_plot.R")
source("./R/get_ppg_plot.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Season Records"),

  # Sidebar panel for inputs
  sidebarPanel(
    # First drop-down where a season is selected
    selectInput(
      "season", "Select season(s):", get_season_list(),
      selected = "2022/23",
      multiple = TRUE
    ),

  # Second drop-down where user selects chart type
  selectInput(
    "chart", "Choose chart type:", get_chart_options()
    )
  ),

  mainPanel(
    # Display chart
    plotlyOutput("pts_plot"),

    hr(),

    plotlyOutput("ppg_plot"),

    hr(),

    h1("Streaks"),
    DT::dataTableOutput("streaks_table"),

    hr(),

    h1("Results"),
    DT::dataTableOutput("results_table")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  output$pts_plot <- renderPlotly(
    get_pts_plot(input$season)
  )

  output$ppg_plot <- renderPlotly(
    get_ppg_plot(input$season)
  )

  output$streaks_table <- DT::renderDataTable(
    get_streaks(input$season),
    rownames = FALSE,
    options = list(
      pageLength = 5,
      dom = 'tip',
      info = FALSE,
      paging = FALSE
      )
  )

  output$results_table <- DT::renderDataTable(
    filter_results(input$season),
    rownames = FALSE,
    options = list(
      pageLength = 5,
      dom = 'tip',
      info = FALSE,
      paging = TRUE
    )
  )

  output$scorers_plot <- renderPlotly(
    get_scorers_plot(input$seasons)
  )
}

# Run the application
shinyApp(ui = ui, server = server)

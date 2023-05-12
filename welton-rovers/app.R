library(shiny)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(plotly)
library(DT)

source("./R/get_data.R")
source("./R/get_streaks.R")

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
    plotlyOutput("seasonsPlot"),

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
      paging = FALSE
    )
  )
}

# Run the application
shinyApp(ui = ui, server = server)

library(shiny)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(plotly)
library(DT)
library(forcats)

source("./R/get_data.R")
source("./R/get_streaks.R")
source("./R/plot_ssn_pts.R")
source("./R/plot_ssn_ppg.R")
source("./R/plot_ssn_scorers.R")

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
  # selectInput(
  #   "chart", "Choose chart type:", get_chart_options()
  #   )
  ),

  mainPanel(
    h1("Point Accumulation"),
    plotlyOutput("pts_plot"),

    hr(),

    h1("Points-per-Game"),
    plotlyOutput("ppg_plot"),

    hr(),

    h1("Streaks"),
    DT::dataTableOutput("streaks_table"),

    hr(),

    h1("Results"),
    DT::dataTableOutput("results_table"),

    hr(),

    h1("Top Scorers"),
    plotlyOutput("scorers_plot")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  output$pts_plot <- renderPlotly(
    plot_ssn_pts(input$season)
  )

  output$ppg_plot <- renderPlotly(
    plot_ssn_ppg(input$season)
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
    # rownames = FALSE,
    # options = list(
    #   pageLength = 10,
    #   filter = ("top"),
    #   dom = 'tip',
    #   info = FALSE,
    #   paging = TRUE
    # )
    rownames= FALSE,
    options = list()
  )

  output$scorers_plot <- renderPlotly(
    plot_ssn_scorers(input$season)
  )

}

# Run the application
shinyApp(ui = ui, server = server)

library(shiny)
library(shinydashboard)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(plotly)
library(DT)
library(forcats)
library(lubridate)

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
    )
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

    h1("Results by Season"),
    uiOutput("ssn_tabs"),

    hr(),

    # h1("All Results"),
    # DT::dataTableOutput("results_table"),
    #
    # hr(),

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

  output_ssn_results <- function(season) {
    DT::renderDataTable(filter_results(season) %>%
                          mutate(date = format(date, format = "%d %b %Y")),
                        options = list(paging = TRUE,    ## paginate the output
                                       pageLength = 10,  ## number of rows to output for each page
                                       scrollX = TRUE,   ## enable scrolling on X axis
                                       scrollY = TRUE,   ## enable scrolling on Y axis
                                       autoWidth = FALSE, ## use smart column width handling
                                       server = FALSE,   ## use client-side processing
                                       dom = 'frtip',
                                       columnDefs = list(list(targets = c(0, 2, 3, 6, 10, 11), className = 'dt-left'),
                                                         list(targets = c(1, 4, 5, 7, 8), className = 'dt-center'),
                                                         list(targets = c(9), className = 'dt-right'))
                        ),
                        extensions = 'Buttons',
                        selection = 'single', ## enable selection of a single row
                        filter = 'bottom',              ## include column filters at the bottom
                        rownames = FALSE                ## don't show row numbers/names
    )
  }

  # Dynamically render the tab panels based on user input
  output$ssn_tabs <- renderUI({
    if (!is.null(input$season)) {
      # Get selected seasons
      selected_seasons <- sort(input$season, decreasing = TRUE)

      # Create a tab panel for each selected season
      ssn_tabs <- lapply(selected_seasons, function(season) {
        tabPanel(title = season,
                 fluidRow(
                   output_ssn_results(season)
                 )
        )
      })

      # Return the tabsetPanel containing season results
      do.call(tabsetPanel, ssn_tabs)
    } else {
      p("Please select one or more seasons from the dropdown menu.")
    }
  })

  output$results_table <- DT::renderDataTable(
    filter_results(input$season),
    rownames= FALSE,
    options = list()
  )

  output$scorers_plot <- renderPlotly(
    plot_ssn_scorers(input$season)
  )

}

# Run the application
shinyApp(ui = ui, server = server)

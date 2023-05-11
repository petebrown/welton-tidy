library(shiny)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(


  # Application title
  titlePanel("Season Records"),

  # Sidebar panel for inputs
  sidebarPanel(
    # First drop-down where a season is selected
    selectInput(
      "season", "Select seasons:", get_season_list(),
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
    plotlyOutput("seasonsPlot")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application
shinyApp(ui = ui, server = server)

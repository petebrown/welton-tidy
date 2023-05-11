get_results <- function() {
  results <- read_csv("./results.csv")
  return(results)
}

get_season_list <- function() {
  results <- get_results()
  seasons <- sort(unique(results$season))
  return(seasons)
}

get_scorers <- function() {
  scorers <- read_csv("./scorers.csv")
  return(scorers)
}

get_chart_options <- function() {
  charts <- c(
    "Point Accumulation" = "pts",
    "Points-per-game" = "ppg"
  )
}

get_results_raw <- function() {
  results <- read_csv("./results.csv", show_col_types = FALSE) %>%
    return(results)
}

get_results <- function() {
  results <- get_results_raw() %>%
    arrange(desc(date)) %>%
    select(
      season,
      game_no,
      date,
      opponent,
      venue,
      score,
      scorers,
      competition,
      cup_round,
      attendance,
      manager,
      referee,
      manager
    )
  return(results)
}

filter_results <- function(seasons) {
  results <- get_results() %>%
    filter(season %in% seasons)
  return(results)
}

get_season_list <- function() {
  results <- get_results()
  seasons <- sort(unique(results$season), decreasing = TRUE)
  return(seasons)
}

get_scorers <- function() {
  scorers <- read_csv("./scorers.csv", show_col_types = FALSE)
  return(scorers)
}

get_chart_options <- function() {
  charts <- c(
    "Point Accumulation" = "pts",
    "Points-per-game" = "ppg"
  )
}

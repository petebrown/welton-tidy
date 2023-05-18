get_chart_options <- function() {
  charts <- c(
    "Point Accumulation" = "pts",
    "Points-per-game" = "ppg"
  )
  return(charts)
}

get_results_raw <- function() {
  results <- read_csv("./results.csv", show_col_types = FALSE)
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

filter_scorers <- function(seasons) {
  scorers <- get_scorers()
  results <- filter_results(seasons)
  df <- scorers %>%
    inner_join(results, by = "date")
  return(df)
}

get_ssn_scorers <- function(seasons) {
  df <- filter_scorers(seasons) %>%
    group_by(season, player_name) %>%
    summarize(
      total_goals = sum(goals_scored),
      .groups = "keep"
    ) %>%
    ungroup() %>%
    group_by(season) %>%
    slice_max(
      n = 3,
      order_by = total_goals
    ) %>%
    ungroup() %>%
    arrange(
      season,
      desc(total_goals),
      desc(player_name)
    )

  return(df)
}

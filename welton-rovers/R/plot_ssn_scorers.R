plot_ssn_scorers <- function(seasons) {
  scorers_by_season <- get_ssn_scorers(seasons)
  player_order <- scorers_by_season$player_name

  p <- ggplot(
    scorers_by_season,
    aes(
      x = factor(player_name, levels = player_order),
      y = total_goals
    )
  ) +
    geom_col() +
    coord_flip() +
    theme_bw() +
    labs(
      x = NULL,
      y = NULL
    )

  ggplotly(p)
}

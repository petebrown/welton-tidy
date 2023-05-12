get_ppg_plot <- function(seasons) {
  df <- get_results_raw() %>%
    filter(
      game_type == "league",
      season %in% seasons
    )

  p <- ggplot(df, aes(x = comp_game_no, y = ppg)) +
    geom_line(aes(color = season)) +
    geom_point(aes(color = season)) +
    theme_bw() +
    scale_y_continuous(
      limits = c(0, 3),
      breaks = c(0, 1, 2, 3)
    )

  ggplotly(p)
}

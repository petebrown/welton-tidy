plot_ssn_pts <- function(seasons) {
  df <- get_results_raw() %>%
    filter(
      game_type == "league",
      season %in% seasons
    )

  p <- ggplot(df, aes(x = comp_game_no, y = ssn_pts)) +
    geom_line(aes(color = season)) +
    geom_point(aes(color = season)) +
    theme_bw()

  ggplotly(p)
}

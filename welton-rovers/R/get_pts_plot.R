get_pts_plot <- function(seasons) {
  df <- get_results_raw() %>%
    # df[df$season %in% seasons, df$game_type == "league"]
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

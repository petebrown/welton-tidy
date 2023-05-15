plot_ssn_scorers <- function(seasons) {
  df <- get_ssn_scorers(seasons) %>%
    mutate(
      ordered = paste0(season, total_goals, player_name) %>%
        forcats::fct_inorder()
    )

  player_order <- df$player_name

  p <- ggplot(
    df,
    aes(
      x = ordered,
      y = total_goals
    )
  ) +
    geom_col() +
    theme_bw() +
    labs(
      x = NULL,
      y = NULL
    ) +
    facet_wrap(
      ~season,
      scales = "free_x"
    ) +
    scale_x_discrete(
      labels = setNames(df$player_name, df$ordered)
    )

  ggplotly(p)
}


get_ssn_scorers(c("2017/18", "2021/22"))

plot_ssn_scorers(c("2017/18", "2021/22"))

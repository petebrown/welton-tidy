plot_ssn_ppg <- function(seasons) {
  df <- get_results_raw() %>%
    filter(
      game_type == "league",
      season %in% seasons
    ) %>%
    mutate(
      date_str = format(date, format = "%e %B %Y"),
      weekday = wday(date, label = TRUE, abbr = FALSE)
    )

  p <- ggplot(df,
              aes(
                x = comp_game_no,
                y = ppg,
                group = 1,
                text = sprintf("Season: %s\nGame No: %.0f\nWeekday: %s\nDate: %s\nOpponent: %s\nVenue: %s\nScore: %s\nScorers: %s\nDivision: %s\nAttendance: %s\nManager: %s\nReferee: %s\nSeason Points: %s\nPPG: %.2f",
                               season,
                               comp_game_no,
                               weekday,
                               date_str,
                               opponent,
                               venue,
                               score,
                               scorers,
                               competition,
                               attendance,
                               manager,
                               referee,
                               ssn_pts,
                               ppg)
                )) +
    geom_line(aes(color = season)) +
    geom_point(aes(color = season)) +
    theme_classic() +
    scale_y_continuous(
      limits = c(0, 3),
      breaks = c(0, 1, 2, 3)
    )

  ggplotly(p, tooltip = "text")
}

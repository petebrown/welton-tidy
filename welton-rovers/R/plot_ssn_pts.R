plot_ssn_pts <- function(seasons) {
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
                y = ssn_pts,
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
    theme_classic()

  ggplotly(p, tooltip = "text")
}

plot_ssn_pts("2022/23")

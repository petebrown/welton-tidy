library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(janitor)
library(readr)
library(readxl)
library(stringr)
library(tidyr)

df <- read_excel("./data/welton.xlsx", sheet = 2) %>%
  clean_names()

edits_df <- read_excel("./data/gazza_check.xlsx", sheet = 1) %>%
  clean_names() %>%
  select(
    date,
    surname,
    full_name_here
  ) %>%
  filter(
    !is.na(full_name_here)
  )

results <- df %>%
  # Rename columns
  rename(
    cup_round = r_6,
    competition = comp,
    venue = h_a,
    goals_for = f,
    goals_against = a,
    scorers = goalscorers,
    attendance = att,
  ) %>%
  mutate(
    goals_against = case_when(
      date == as.Date("2014-08-19") ~ 0,
      TRUE ~ TRUE
    ),
    outcome = case_when(
      goals_for > goals_against ~ "W",
      goals_for == goals_against ~ "D",
      goals_for < goals_against ~ "L"
    ),
    score = paste0(goals_for, "-", goals_against),
    scorers = case_when(
      scorers %in% c("x", "X") ~ NA,
      TRUE ~ scorers
    ),
    referee = case_when(
      referee %in% c("x", "X") ~ NA,
      TRUE ~ referee
    ),
    scorers = case_when(
      date == as.Date("1980-04-09") ~ "Graham Withey 3",
      date == as.Date("1980-04-05") ~ "Simon Neil, Graham Withey",
      date == as.Date("1961-02-25") ~ "Keith Simmons",
      TRUE ~ scorers
    ),
    cup_round = case_when(
      date == as.Date("1951-09-22") ~ "P",
      date == as.Date("1952-09-13") ~ "P",
      date == as.Date("1952-09-20") ~ "P",
      TRUE ~ cup_round
    ),
    scorers = str_replace(scorers, "Matty Morris Cam Allen 2", "Matty Morris, Cam Allen 2"),
    scorers = str_replace_all(scorers, "\\?+", "Unknown"),
    scorers = str_replace_all(scorers, "\\s+,", ","),
    scorers = str_remove(scorers, ",\\s*$"),
    scorers = str_remove(scorers, "\\s\\(10th of season\\)"),
    scorers = str_remove(scorers, "\\s\\(shared the Welton goals\\)")
  )

results <- results %>%
  group_by(season) %>%
  arrange(date) %>%
  mutate(
    game_no = row_number()
  ) %>%
  ungroup()

results <- results %>%
  group_by(season, competition) %>%
  arrange(date) %>%
  mutate(
    comp_game_no = row_number()
  ) %>%
  ungroup()

results <- results %>%
  select(
    season,
    date,
    opponent,
    venue,
    score,
    outcome,
    scorers,
    competition,
    cup_round,
    game_no,
    comp_game_no,
    goals_for,
    goals_against,
    attendance,
    manager,
    referee
  ) %>%
  mutate(
    game_type = if_else(
      competition %in% c("WLP", "WL1"), "league", "cup"),
    points = case_when(
      game_type == "league" & outcome == "W" ~ 3,
      game_type == "league" & outcome == "D" ~ 1,
      game_type == "league" & outcome == "L" ~ 0,
      TRUE ~ NA
    )
  )

ssn_pts <- results %>%
  filter(game_type == "league") %>%
  select(
    season,
    date,
    game_type,
    points
  ) %>%
  arrange(date) %>%
  group_by(season) %>%
  mutate(
    ssn_pts = cumsum(points)
  ) %>%
  select(-points)

results <- results %>%
  left_join(ssn_pts, by = c("season", "date", "game_type")) %>%
  mutate(
    ppg = ssn_pts / comp_game_no
  )

write_csv(results, "./welton-rovers/results.csv")

scorers_list <- separate_wider_delim(
  results,
  scorers,
  delim = ",",
  names_sep = "",
  too_few = "align_start",
) %>%
  select(
    date,
    starts_with("scorers")
  ) %>%
  mutate(
    across(starts_with("scorers"), str_squish)
  ) %>%
  pivot_longer(
    !date,
    values_to = "scorer"
  ) %>%
  filter(
    !is.na(scorer)
  ) %>%
  select(
    date,
    scorer
  ) %>%
  mutate(
    scorer_clean = str_replace_all(
      scorer, "\\s?[Pp]enalty\\)", "p\\)"
    ),
    scorer_clean = str_replace_all(
      scorer_clean, "\\([Pp]\\)", "\\(1p\\)"
    ),
    player_name = case_when(
      str_detect(scorer_clean, "\\s\\(?\\d+\\)?$") ~ str_replace_all(scorer_clean, "\\s\\(?\\d+\\)?$", ""),
      str_detect(scorer_clean, "(?:\\s\\d+)?\\s\\(\\d+p\\)$") ~ str_replace_all(scorer_clean, "(?:\\s\\d+)?\\s\\(\\d+p\\)$", ""),
      TRUE ~ scorer_clean
    ),
    goals_scored = case_when(
      str_detect(scorer_clean, "[:alpha:]\\s\\(?\\d+\\)?$") ~ as.numeric(str_match(scorer_clean, "[:alpha:]\\s\\(?(\\d+)\\)?$")[,2]),
      str_detect(scorer_clean, "[:alpha:]\\s\\(?\\d+\\)?\\s\\(\\d+p\\)$") ~ as.numeric(str_match(scorer_clean, "[:alpha:]\\s\\(?(\\d+)\\)?\\s\\(\\d+p\\)$")[,2]),
      TRUE ~ 1
    ),
    penalty = case_when(
      str_detect(
        scorer_clean, "\\(\\d?\\s?[Pp]\\)") ~ as.numeric(str_match(scorer_clean, "\\((\\d+)p\\)")[,2]),
      TRUE ~ 0
    ),
    own_goal = case_when(
      str_detect(scorer_clean, "OG") ~ 1,
      TRUE ~ 0
    ),
    player_name = case_when(
      player_name == "Bobby Comer" ~ "Bob Comer",
      player_name == "Haydn Watts" ~ "Hayden Watts",
      player_name == "Jeff Elliott" ~ "Geoff Elliott",
      str_detect(player_name, "OG") ~ "OG",
      TRUE ~ player_name
    )
  ) %>%
  select(
    date,
    player_name,
    goals_scored,
    penalty,
    own_goal
  )

scorers_list <- scorers_list %>%
  left_join(edits_df, by = c("date" = "date", "player_name" = "surname")) %>%
  mutate(
    player_name = case_when(
      !is.na(full_name_here) ~ full_name_here,
      TRUE ~ player_name
    )
  ) %>%
  select(-full_name_here)

write_csv(scorers_list, "./welton-rovers/scorers.csv")

scorers_list %>%
  left_join(
    results %>% filter(goals_for > 0), by = "date"
  ) %>%
  group_by(season, player_name) %>%
  summarize(
    goals = n()
  ) %>%
  slice_max(
    goals,
    n = 1
  ) %>%
  arrange(
    desc(season)
  ) %>% print(n = 20)

results %>%
  filter(
    season == "2021/22",
    game_type == "league"
  ) %>%
  ggplot(aes(x = comp_game_no, y = ppg)) +
  geom_line() +
  theme_bw() +
  expand_limits(y = c(0, 3))

results %>%
  filter(
    season == "2021/22",
    game_type == "league"
  ) %>%
  ggplot(aes(x = comp_game_no, y = ssn_pts)) +
  geom_line() +
  theme_bw()

results %>%
  # filter(season %in% input$season) %>%
  # filter(season == "1964/65") %>%
  arrange(season, game_no) %>%
  group_by(season) %>%
  mutate(
    wins = ifelse(outcome == "W", 1, 0),
    unbeaten = ifelse(outcome != "L", 1, 0),
    losses = ifelse(outcome == "L", 1, 0),
    winless = ifelse(outcome != "W", 1, 0),
    draws = ifelse(outcome == "D", 1, 0),
    cs = ifelse(goals_against == 0, 1, 0),
    goalless = ifelse(goals_for == 0, 1, 0),
    wins_cs = ifelse(outcome == "W" & goals_against == 0, 1, 0),
    w_streak = ifelse(wins == 0, 0, sequence(rle(as.character(wins))$lengths)),
    unbeaten_streak = ifelse(unbeaten == 0, 0, sequence(rle(as.character(unbeaten))$lengths)),
    losing_streak = ifelse(losses == 0, 0, sequence(rle(as.character(losses))$lengths)),
    winless_streak = ifelse(winless == 0, 0, sequence(rle(as.character(winless))$lengths)),
    d_streak = ifelse(draws == 0, 0, sequence(rle(as.character(draws))$lengths)),
    clean_sheets = ifelse(cs == 0, 0, sequence(rle(as.character(cs))$lengths)),
    goalless_streak = ifelse(goalless == 0, 0, sequence(rle(as.character(goalless))$lengths)),
  ) %>%
  rename(Season = season) %>%
  summarize(
    "Wins" = max(w_streak),
    "Unbeaten" = max(unbeaten_streak),
    "Losses" = max(losing_streak),
    "Winless" = max(winless_streak),
    "Draws" = max(d_streak),
    "Clean sheets" = max(clean_sheets),
    "Firing blanks" = max(goalless_streak)
  ) %>%
  arrange(desc(`Firing blanks`))

results %>%
  filter(season == "2015/16") %>%
  mutate(
    gd = goals_for - goals_against
  ) %>%
  arrange(gd)

results %>%
  group_by(cup_round) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  print(n=100)

results %>%
  mutate(replay = case_when(
    str_sub(cup_round, -1) == "R" ~ 1,
    TRUE ~ 0
  )) %>%
  filter(game_type == "cup") %>%
  arrange(competition, date) %>%
  mutate(
    is_replay = case_when(
      replay == 1 & opponent == lag(opponent) & competition == lag(competition) ~ 1,
      TRUE ~ 0
    )
  )

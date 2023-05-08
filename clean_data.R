library(readxl)
library(dplyr)
library(readr)
library(stringr)
library(janitor)
library(tidyr)

df <- read_excel("./data/welton.xlsx", sheet = 2) %>%
  clean_names()

results <- df %>%
  rename(
    cup_round = r_6,
    competition = comp,
    venue = h_a,
    gf = f,
    ga = a,
    scorers = goalscorers,
    attendance = att,
  ) %>%
  mutate(
    outcome = case_when(
      gf > ga ~ "W",
      gf == ga ~ "D",
      gf < ga ~ "L"
    ),
    score = paste0(gf, "-", ga),
    scorers = na_if(scorers, "x")
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
    scorers,
    competition,
    cup_round,
    game_no,
    comp_game_no,
    gf,
    ga,
    attendance,
    manager,
    referee
  ) %>%
  arrange(desc(date))

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
    no_of_goals = str_extract(
      scorer, "\\d+"
    ),
    scorer2 = str_split(scorer, "[:space:]\\d+", n = 2)
  )

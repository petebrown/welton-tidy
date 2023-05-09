library(readxl)
library(dplyr)
library(readr)
library(stringr)
library(janitor)
library(tidyr)
library(purrr)

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
    scorers = case_when(
      scorers %in% c("x", "X") ~ NA,
      str_detect(scorers, regex(",\\s?$")) ~ str_match(scorers, "(.+),$")[,2],
      scorers == "Matty Morris Cam Allen 2" ~ "Matty Morris, Cam Allen 2",
      TRUE ~ scorers
    ),
    scorers = str_replace_all(scorers, "\\?+", "Unknown"),
    scorers = str_replace_all(scorers, "\\s+,", ",")
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
    scorer_clean = str_replace_all(
      scorer, "\\s?[Pp]enalty\\)", "p\\)"
    ),
    scorer_clean = str_replace_all(
      scorer_clean, "\\([Pp]\\)", "\\(1p\\)"
    ),
    # no_of_goals = str_extract(
    #   scorer_clean, "\\s\\d+"
    #   ),
    scorer_name = str_replace_all(
      scorer_clean, "[:space:]\\d+$", ""
      ),
    goals_scored = case_when(
      str_detect(scorer_clean, "\\s\\(?\\d+\\)?") ~ as.integer(str_extract(scorer, "[:space:]\\(?\\d+\\)?")),
      TRUE ~ 1
    ),
    penalty = case_when(
      str_detect(scorer_clean, "\\(\\d?\\s?[Pp]\\)") ~ as.numeric(str_match(scorer_clean, "\\((\\d+)p\\)")[,2]),
      TRUE ~ 0
    ),
    own_goal = case_when(
      str_detect(scorer_clean, "OG") ~ 1,
      TRUE ~ 0
    ),
    scorer_name = case_when(
      scorer_name == "Bobby Comer" ~ "Bob Comer",
      scorer_name == "Haydn Watts" ~ "Hayden Watts",
      scorer_name == "Jeff Elliott" ~ "Geoff Elliott",
      # scorer_name == "P Abrahams" ~ "Paul Abrahams",
      TRUE ~ scorer_name
    )
  )

surnames_only <- scorers_list %>%
  filter(
    !str_detect(scorer_name, "\\s"),
    scorer_name != "Unknown",
    scorer_name != "OG",
    scorer_name != "???"
  )

surnames_only_counts <- surnames_only %>%
  group_by(scorer_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

possible_matches <- scorers_list %>%
  select(scorer_name) %>%
  mutate(surname = str_split(scorer_name, "\\s", n = 2, simplify = TRUE)[, -1]) %>%
  unique() %>%
  filter(surname %in% surnames_only$scorer) %>%
  arrange(surname, scorer_name) %>%
  select(surname, scorer_name)

possible_matches_lists <- possible_matches %>%
  group_by(surname) %>%
  summarise(
    poss_matches = list(str_extract_all(scorer_name, paste0(scorer_name, collapse = "|"))),
    ) %>%
  rowwise() %>%
  mutate(
    poss_matches = paste(poss_matches, collapse=', ')
  ) %>%
  ungroup()

gazza_check <- surnames_only %>%
  select(
    date,
    scorer_name
    ) %>%
  rename(surname = scorer_name) %>%
  left_join(results %>% select(
    season,
    date,
    opponent,
    venue,
    score,
    scorers,
    competition
    ), by = "date") %>%
  left_join(possible_matches_lists, by = "surname") %>%
  relocate("surname", .before = "poss_matches")

# gazza_check %>%
#   arrange(surname, date) %>%
#   mutate(score = paste0("'", score)) %>% write_excel_csv("~/Desktop/gazza_check.csv")

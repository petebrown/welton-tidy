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
#   write_excel_csv("~/Desktop/gazza_check.csv")


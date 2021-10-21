voting<-tar_read("cleaned_data")

voting %>%
  filter(senator_for == "Denis O'Donovan") %>%
  filter(voting_day==TRUE) %>%
  mutate(contributions = as.numeric(as.character(contributions))) %>%
  mutate(actual_attended = case_when(voted == TRUE ~ TRUE,
                                     as.numeric(contributions) > 0 ~ TRUE,
                                     T ~ FALSE)) %>%

  group_by(actual_attended, attended) %>%
  count()

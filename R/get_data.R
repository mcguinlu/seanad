source(here::here("R/helpers.R"))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Get voting data, and save to CSV
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

get_raw_data(26, "data/senate_votes_26.csv")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Analysis
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-


# Create bar chart of % of votes attended ----
(
  voting %>%
    group_by(member_rev_comma) %>%
    count() %>%
    mutate(percentage = n / 131 * 100) %>%
    arrange(desc(percentage)) %>%
    mutate(half = ifelse(percentage > 50, TRUE, FALSE)) %>%
    arrange(desc(percentage)) %>%
    ggplot(aes(
      y = reorder(member_rev_comma, percentage),
      x = percentage,
      fill = half
    )) +
    geom_vline(xintercept = c(25, 50, 75), linetype = "dashed") +
    geom_bar(
      stat = "identity",
      alpha = 0.5,
      show.legend = F
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(0, 102),
      labels = c("0%", "25%", "50%", "75%", "100%")
    ) +
    ggthemes::scale_fill_economist() +
    labs(
      y = "Senator",
      x = "Percentage of votes attended",
      title = "Percentage of votes attended member of the 26th Seanad",
      subtitle = "Many Senators do not regularly vote on Seanad motions\n",
      caption = stringr::str_wrap(
        "Total number of Senators shown is 56. The Cathaoirleach (who does not vote) was excluded, as were Senators who are Ministers (n=1), who resigned (n=2), or who replaced those who resigned (n=2)",
        70
      )
    ) +
    geom_text(
      x = 37.5,
      y = "O'Donovan, Denis",
      label = "12 Senators voted\nin <50% of votes",
      size = 4
    ) +
    # theme_minimal() +
    ggthemes::theme_economist_white() +
    theme(
      axis.title = element_text(margin = margin(t = 10, r = 10, b = 10, l = 0)),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title.position = "plot"
    )
) %>%
  ggsave(filename = "figures/seanad_percentage.png",
         height = 14,
         width = 9)




# days_2021 <- processed_2021 %>%
#   get_attendence() %>%
#   rename(member_rev_comma = name)
#
# days_2020 <- processed_2020 %>%
#   get_attendence() %>%
#   rename(member_rev_comma = name)
#
# days_attended <-
#   left_join(days_2021,
#             days_2020,
#             by = c("member_rev_comma" = "member_rev_comma")) %>%
#   mutate(
#     sitting_days = sitting_days.x + sitting_days.y,
#     non_sitting_days = non_sitting_days.x + non_sitting_days.y
#   ) %>%
#   select(-contains("sitting_days.")) %>%
#   mutate(total_days = sitting_days + non_sitting_days) %>%
#   glimpse()

days_voting <- voting %>%
  group_by(member_rev_comma, member_for) %>%
  summarise(n = n_distinct(debate_date)) %>%
  glimpse()


(
  left_join(days_attended, days_voting) %>%
    ggplot(aes(y = n, x = sitting_days)) +
    geom_point(aes(
      color = ifelse(n < 10 | sitting_days < 35, "red", "black")
    )) +
    geom_text(
      aes(label = ifelse(n < 10 |
                           sitting_days < 35, member_for, "")),
      nudge_x = .5,
      nudge_y = -0.7,
      hjust = 0
    ) +
    scale_y_continuous(limits = c(0, 55), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 70), expand = c(0, 0)) +
    scale_color_identity() +
    ggthemes::theme_economist_white() +
    labs(y = "Number of sitting days member voted",
         x = "Number of sitting days attended")
) %>%
  ggsave(filename = "figures/seanad_attendance_vs_voting_days.png",
         height = 9,
         width = 8)

# Get mean number of votes per debate
voting_by_member <- voting %>%
  group_by(member_rev_comma) %>%
  count()

(
  left_join(tmp2, voting_by_member) %>%
    mutate(
      half = ifelse(n / 131 > .5, TRUE, FALSE),
      percentage = n / 131 * 100
    ) %>%

    ggplot(
      aes(
        x = days,
        y = percentage,
        colour = half,
        label = member_rev_comma
      )
    ) +
    geom_point(
      position = position_jitter(width = 0.4, seed = 42),
      show.legend = FALSE
    ) +
    geom_label(show.legend = FALSE) +
    scale_x_continuous(breaks = seq(25, 64, by = 2)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
    labs(
      y = "Percentage of votes attended",
      x = "Number of sitting days attended",
      title = ,
      caption = stringr::str_wrap(
        "Both Lynn Boylan and Fintan Warfield were not recorded as attending any sitting days in 2021, which is likely a adminstrative effor given the number of votes they have attended.",
        70
      ),
      subtitle = stringr::str_wrap(
        "Despite being presented for the majority of sitting days, some Senators do not vote on those days.",
        70
      )
    ) +
    ggthemes::theme_economist_white()
) %>%
  ggsave(filename = "figures/seanad_percentage_vs_attendance.png",
         height = 8,
         width = 8)


t3 <- t %>%
  filter(sitting_day == TRUE) %>%
  distinct(date) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  summarise(length = n(),
            min = min(date),
            max = max(date))

t2 <-
  read_html(
    "https://www.oireachtas.ie/en/debates/find/?datePeriod=term&debateType=seanad&term=%2Fie%2Foireachtas%2Fhouse%2Fseanad%2F26&resultsPerPage=100"
  ) %>%
  html_nodes(".js-debate-accordion") %>%
  html_text() %>%
  stringr::str_remove_all("\n") %>%
  stringr::str_remove_all(".+,") %>%
  stringr::str_squish() %>%
  as.Date(format = "%d %b %Y")

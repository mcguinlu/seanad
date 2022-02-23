library(pdftools)
library(dplyr)
library(rvest)
library(ggplot2)
library(tidyr)
library(magrittr)

# Create a basic dataframe with senators and all sitting days in tidy format

create_base_data <- function(before = ""){

  n_pages <-   read_html(
    glue::glue(
      "https://www.oireachtas.ie/en/debates/find/?datePeriod=term&debateType=se"
      ,"anad&term=%2Fie%2Foireachtas%2Fhouse%2Fseanad%2F26&resultsPerPage=20")
  ) %>%
    html_node(".c-page-num__ref") %>%
    html_text() %>%
    stringr::str_remove(" 1 of ") %>%
    as.numeric()

  get_sitting_days <- function(page_n){
    read_html(
      glue::glue(
        "https://www.oireachtas.ie/en/debates/find/?page={page_n}&datePeriod=term&debateType=se"
        ,"anad&term=%2Fie%2Foireachtas%2Fhouse%2Fseanad%2F26&resultsPerPage=20")
    ) %>%
      html_nodes(".c-debates-expanding-list__accordion") %>%
      html_text() %>%
      stringr::str_remove(".*,") %>%
      stringr::str_remove_all("\\n") %>%
      stringr::str_squish() %>%
      as.Date("%d %B %Y") %>%
      data.frame(sitting_date = .) %>%
      return()
  }

  sitting_days <- purrr::map_df(1:n_pages, get_sitting_days)
  senators_for <- get_senator_names("for")

  data.frame(
    sitting_day = rep(sitting_days$sitting_date, times = length(senators_for)),
    senator_for = rep(senators_for, each = length(sitting_days$sitting_date))
  )

}

add_voting <- function(dat, raw_voting_data){

  raw_voting_data <- raw_voting_data %>%
    mutate(sitting_day = stringr::str_remove(debate_link,"https://www.oireachtas.ie/en/debates/vote/seanad/26/")) %>%
    mutate(sitting_day = stringr::str_remove(sitting_day,"/$")) %>%
    separate(sitting_day, into = c("sitting_day", "vote_number"),sep = "/") %>%
    mutate(sitting_day = as.Date(sitting_day))

  # Add maximum votes per day
  max_votes_per_day <- raw_voting_data %>%
    group_by(sitting_day) %>%
    summarise(max_votes_per_day = n_distinct(debate_link))

  dat<-left_join(dat, max_votes_per_day) %>%
    mutate(max_votes_per_day = ifelse(is.na(max_votes_per_day),0,max_votes_per_day)) %>%
    mutate(voting_day = ifelse(max_votes_per_day>0,TRUE,FALSE))

  # Add number of votes attended

  votes_attended <- raw_voting_data %>%
    rename("member_rev_comma" = member) %>%
    left_join(get_senator_names(all=T, type = "df")) %>%
    rename("senator_for" = member_for) %>%
    select(-starts_with("member_")) %>%
    group_by(sitting_day, senator_for) %>%
    summarise(votes_attended = n())

  dat <- left_join(dat, votes_attended) %>%
    mutate(votes_attended = ifelse(is.na(votes_attended),0,votes_attended)) %>%
    mutate(voted = ifelse(votes_attended>0,TRUE,FALSE))

  return(dat)

}

add_attendance <- function(dat){

  pdf_file_2021 <-
    "https://data.oireachtas.ie/ie/oireachtas/members/recordAttendanceForTaa/2022/2022-01-13_senators-verification-of-attendance-for-the-payment-of-taa-01-jan-2021-to-30-november-2021_en.pdf"


  pdf_file_2020 <-
    "https://data.oireachtas.ie/ie/oireachtas/members/recordAttendanceForTaa/2021/2021-06-22_senators-verification-of-attendance-for-the-payment-of-taa-30-mar-2020-to-31-dec-2020_en.pdf"

  processed_2021 <- process_pdf(pdf_file_2021)
  processed_2020 <- process_pdf(pdf_file_2020)



  attendance_df <- rbind(
    purrr::map2_df(
      processed_2021$breaks$page_start,
      processed_2021$breaks$page_end,
      ~ get_attendence_days_df(processed_2021$txt, .x, .y)
    ),
    purrr::map2_df(
      processed_2020$breaks$page_start,
      processed_2020$breaks$page_end,
      ~ get_attendence_days_df(processed_2020$txt, .x, .y)
    )
  ) %>%
    filter(sitting_day == TRUE) %>%
    select(-sitting_day) %>%
    left_join(get_senator_names(all=T, type = "df")) %>%
    rename("senator_for" = member_for) %>%
    select(-starts_with("member_")) %>%
    mutate(attended = TRUE) %>%
    mutate(sitting_day = as.Date(strptime(date, "%d/%m/%Y"),"%Y-%m-%d")) %>%
    select(-date)

  dat <- left_join(dat, attendance_df) %>%
    mutate(attended = ifelse(is.na(attended),FALSE,attended))

  return(dat)

}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

count_contributions <- function(debate, senators){

  count <- purrr::map_chr(senators,
                          ~sum(stringr::str_count(
    unlist(debate$text),
    paste0("Senator ", .x, ":")
  )))

  return(data.frame(senator_for = senators,
                    sitting_day = rep(debate$date,
                                      length(senators)),
                    contributions = count))
}


add_contributions <- function(dat, debates) {
  contributions <-
    purrr::map_df(debates,
                  ~ count_contributions(debate = .x,
                                        senators = unique(dat$senator_for)))

  left_join(dat, contributions)

}


#' Count how many contributions were made by a given Senator on a given sitting
#' day
#'
#' @description If there is no debate that day, the connection will give a 500
#'   error.
#'
#' @param date Date of debate, in format {%d/%m/%Y}
#' @param senator Name of senator in form {first last}

get_debate_text <- function(date){

  url <- make_url(date = date, senator = senator)

  contributions <- read_html(url) %>%
    html_node(".u-reader-heading strong:nth-child(1)") %>%
    html_text() %>%
    as.numeric()

}

get_contributions <- function(date, senator = "David Norris", pb) {
  pb$tick()



  if (runif(1)>.75) {
    Sys.sleep(1.5)
  }else {
    Sys.sleep(4)
  }

  data.frame(
    sitting_day = date,
    senator_for = senator,
    contributions = contributions,
    stringsAsFactors = F
  ) %>%
  mutate(contributions = ifelse(is.na(contributions),0,contributions)) %>%
  mutate(contributed = ifelse(contributions > 0, TRUE, FALSE)) %>%
  return()
}

#' Create the URL needed to search contributions
#'
#' @param date Date of debate, in format {%d/%m/%Y}
#' @param senator Name of senator in form {first last}

make_url <- function(date, senator) {

  senator_clean <- stringr::str_replace_all(senator, " ", "%20") %>%
    stringr::str_replace_all("'", "%27") %>%
    paste0("Senator%20", .)

  glue::glue(
    "https://www.oireachtas.ie/en/search/?q={senator_clean}&sort=relevance&debate=seanad%2F{date}&searchType=debate"
  ) %>%
    as.character()
}

#' Returns a number indicating how many pages of votes there are
#'
#' @param senate
#'

get_number_of_vote_pages <- function(senate) {
  read_html(
    glue::glue(
      "https://www.oireachtas.ie/en/debates/votes/?page=1&datePeriod=term",
      "&voteResultType=all&debateType=seanad&term=%2Fie%2Foireachtas%2Fhouse",
      "%2Fseanad%2F{senate}&resultsPerPage=100"
    )
  ) %>%
    html_node(".c-page-num__ref") %>%
    html_text() %>%
    stringr::str_remove("1 of ") %>%
    as.numeric()

}

#' Convert list of relative links to full links, and return as a dataframe
#'
#' @param v

clean_links <- function(v) {
  data.frame(v = paste0("https://www.oireachtas.ie", v),
             stringsAsFactors = FALSE)
}

#' Title
#'
#' @param page

get_vote_links <- function(page_n, senate) {
  read_html(
    glue::glue(
      "https://www.oireachtas.ie/en/debates/votes/?page={page_n}&datePeriod=term",
      "&voteResultType=all&debateType=seanad&term=%2Fie%2Foireachtas%2Fhouse",
      "%2Fseanad%2F{senate}&resultsPerPage=100"
    )
  ) %>%
    html_nodes(".c-votes-list__link") %>%
    html_attr("href")
}

#' Title
#'
#' @param page
#' @param name
#' @param link

get_voting_record <- function(page, name, link) {
  page %>%
    html_nodes(".c-vote-detail-voters-list__group") %>%
    html_text() %>%
    stringr::str_trim() %>%
    stringr::str_squish() %>%
    stringr::str_split("\\. ") %>%
    unlist() %>%
    data.frame(member = .) %>%
    dplyr::mutate(debate_name = name, debate_link = link)
}


#' Title
#'
#' @param page

get_vote_name <- function(page) {
  page %>%
    html_node(".c-vote-detail-info__description") %>%
    html_text() %>%
    stringr::str_trim() %>%
    stringr::str_squish() %>%
    unlist()
}


#' Combines simple functions above to return info on each vote
#'
#' @param link

get_voting_df <- function(link, pb) {
  pb$tick()

  Sys.sleep(2)

  page <- read_html(link)

  name <- get_vote_name(page)

  return(get_voting_record(page, name, link))

}



#' Get raw data on the voting history of any given Seanad
#'
#' @param senate Which senate session data should be extracted for. For example,
#'   setting senate = 26 will extract data for the 26th senate

get_raw_voting_data <- function(senate = 26, file) {
  usethis::ui_info(paste("Extracting data for the", senate, "Seanad"))

  v <-
    purrr::map(1:get_number_of_vote_pages(senate),
               ~ get_vote_links(.x, senate = senate)) %>%
    unlist() %>%
    clean_links()

  usethis::ui_info(paste("Number of votes:", nrow(v)))
  usethis::ui_info("Beginning extraction of member voting histories now...")

  # Estimate length of progress bar
  pb <-
    progress::progress_bar$new(total = nrow(v),
                               format = "[:bar] :current/:total ETA: :eta",
                               force = TRUE)
  DF <- purrr::map_df(v$v, ~ get_voting_df(.x, pb))
  row.names(DF) <- NULL

  DF %>%
    mutate(member = stringr::str_remove(member, "\\."))
}


apply_filters <- function(dat){
  dat %>%

  filter(
    # remove senators who won't have full complement of data
    !(senator_for %in%  c(
      "Mark Daly", # Cath
      "Maria Byrne", # Resigned/replaced
      "Michael D'Arcy", # Resigned/replaced
      "Elisha McCallion", # Resigned/replaced
      "Gerry Horkan",  # Resigned/replaced,
      "Pippa Hackett" # Minister
    )
  ),
  # remove dates before we have attendance data for
  sitting_day < as.Date("2021-08-01"))  %>%
  return()

  }


#' Clean sitting days extracted manually from PDF
#'
#' @param x Character string of sitting days

clean_sitting_days <- function(x) {
  stringr::str_split(x, " ", simplify = T) %>%
    as.data.frame() %>%
    pull(1) %>%
    as.character()
}

is_name_page <- function(txt, page) {
  stringr::str_split(txt[page], "\n") %>%
    as.character() %>%
    stringr::str_replace_all(",", "") %>%
    stringr::str_detect("Member Sitting Days Report") %>%
    as.data.frame()
}

# Extract name of senators from PDF
get_name <- function(txt, start, end, type = "df") {
  t <- paste0(txt[start:end], collapse = " ") %>%
    stringr::str_split("\n") %>%
    data.frame()


  name <- t %>%
    pull(1) %>%
    nth(3) %>%
    as.character() %>%
    stringr::str_remove_all("\\r")

  if (type == "char") {
    return(name)
  }

  return(data.frame(name = name))
}

# Extract number of sitting days attended
get_sitting_days <- function(txt, start, end) {
  t <- paste0(txt[start:end], collapse = " ") %>%
    stringr::str_split("\n") %>%
    data.frame()

  days <- t %>%
    janitor::clean_names() %>%
    rename("clean" = 1) %>%
    filter(grepl("Sub-total", clean)) %>%
    pull(1) %>%
    stringr::str_remove_all("\\r") %>%
    stringr::str_remove_all("Sub-total: ") %>%
    stringr::word() %>%
    as.numeric()

  return(data.frame(sitting_days = days))
}

get_non_sitting_days <- function(txt, start, end) {
  t <- paste0(txt[start:end], collapse = " ") %>%
    stringr::str_split("\n") %>%
    data.frame()

  days <- t %>%
    janitor::clean_names() %>%
    rename("clean" = 1) %>%
    filter(grepl("Sub-total", clean)) %>%
    pull(1) %>%
    stringr::str_remove_all("\\r") %>%
    stringr::str_remove_all("Sub-total: ") %>%
    stringr::str_squish() %>%
    stringr::word(start = 2) %>%
    as.numeric()

  return(data.frame(non_sitting_days = days))
}

get_attendence_days_df <- function(txt, start, end) {
  name <- get_name(txt, start, end, "char")

  t <- paste0(txt[start:end], collapse = " ") %>%
    stringr::str_split("\n") %>%
    data.frame(stringsAsFactors = FALSE)

  days <-
    t %>%
    janitor::clean_names() %>%
    rename("clean" = 1) %>%
    mutate(clean = gsub("\\s+", " ", clean)) %>%
    filter(grepl("\\s?[0-9]{2}/", stringr::str_sub(clean, 1, 4))) %>%
    pull(1) %>%
    stringr::str_remove_all("\\r") %>%
    stringr::str_split(" +")

  if (length(days) != 0) {
    sitting_days_vec <- sapply(days, `[`, 1)

    sitting_days <-
      data.frame(
        member_rev_no_comma = rep(name, length(sitting_days_vec)),
        sitting_day = rep(TRUE, length(sitting_days_vec)),
        date = sitting_days_vec,
        stringsAsFactors = FALSE
      )


    non_sitting_days_vec <- sapply(days, `[`, 2)

    non_sitting_days <-
      data.frame(
        member_rev_no_comma = rep(name, length(non_sitting_days_vec)),
        sitting_day = rep(FALSE, length(non_sitting_days_vec)),
        date = non_sitting_days_vec,
        stringsAsFactors = FALSE
      )

    rbind(sitting_days, non_sitting_days) %>%
      dplyr::filter(date != "") %>%
      return()
  }
}



get_senator_names <- function(format = "for", type = "character", all = FALSE) {
  # read.csv("data/senate_votes_26.csv", stringsAsFactors = F) %>%
  #   mutate(member = stringr::str_remove(member, "\\.")) %>%
  #   tidyr::separate(member, c("member_last","member_first"),remove = FALSE, sep = ", ") %>%
  #   rename(member_rev_comma = member) %>%
  #   mutate(member_rev_no_comma = stringr::str_remove(member_rev_comma, ","),
  #          member_for = paste(member_first,member_last)) %>%
  #   group_by(member_for) %>%
  #   summarise(member_rev_comma,member_rev_no_comma,member_first,member_last) %>%
  #   distinct()%>%
  #
  #   write.csv("data/senator_names.csv", row.names = F)


  ret <-
    read.csv(here::here("data/senator_names.csv"), stringsAsFactors = F)

  if (all != TRUE) {
  ret <- ret[paste0("member_", format)]
  }

  if (type == "character") {
    return(ret %>% pull())
  }

  return(ret)

}



process_pdf <- function(file) {
  txt <- pdf_text(file)

  ind <-
    purrr::map_df(1:length(txt), ~ is_name_page(txt = txt, page = .x)) %>%
    mutate(seq = 1:n()) %>%
    mutate(seq = ifelse(. == "FALSE", NA, seq)) %>%
    tidyr::fill(seq) %>%
    mutate(pages = seq(1:n())) %>%
    group_by(seq) %>%
    summarise(page_start = min(pages),
              page_end = max(pages))


  return(list(txt = txt,
              breaks = ind))

}



clean_senator_names <- function(df) {
  df %>%
    mutate(name =  stringr::str_replace(name, " ", ", ")) %>%
    mutate(
      name = stringr::str_replace_all(name, "Ã¡", "á"),
      name = stringr::str_replace_all(name, "Ã³", "ó")
    ) %>%
    mutate(
      name = case_when(
        agrepl("Kyne, Seán", name) ~ "Kyne, Seán",
        agrepl("Ó Donnghaile, Niall", name) ~ "Ó Donnghaile, Niall",
        agrepl("Mullen, Rónán", name) ~ "Mullen, Rónán",
        agrepl("Carrigy, Micheál", name) ~ "Carrigy, Micheál",
        agrepl("Craughwell, Gerard P", name) ~ "Craughwell, Gerard P",
        agrepl("Martin, Vincent P", name) ~ "Martin, Vincent P",
        agrepl("Seery Kearney, Mary", name) ~ "Seery Kearney, Mary",
        T ~ name
      )
    ) %>%
    return()
}



get_attendence <- function(ind) {
  purrr::map2_df(
    ind$breaks$page_start,
    ind$breaks$page_end,
    ~ get_name(
      txt = ind$txt,
      start = .x,
      end = .y
    )
  ) %>%

    cbind(purrr::map2_df(
      ind$breaks$page_start,
      ind$breaks$page_end,
      ~ get_sitting_days(
        txt = ind$txt,
        start = .x,
        end = .y
      )
    )) %>%
    cbind(
      purrr::map2_df(
        ind$breaks$page_start,
        ind$breaks$page_end,
        ~ get_non_sitting_days(
          txt = ind$txt,
          start = .x,
          end = .y
        )
      )
    ) %>%
    clean_senator_names() %>%

    filter(!(
      name %in%  c(
        "Daly, Mark",
        # Cath
        "Byrne, Maria",
        # Resigned/replaced
        "D'Arcy, Michael",
        # Resigned/replaced
        "McCallion, Elisha",
        # Resigned/replaced
        "Horkan, Gerry",
        # Resigned/replaced
        "Hackett, Pippa" # Minister
      )
    )) %>%
    return()
}


get_debate_text <- function(base_data){

  debate_text <- function(sitting_date){
  pdf_text <- pdf_text(glue::glue("https://data.oireachtas.ie/ie/oireachtas/debateRecord/seanad/{sitting_date}/debate/mul@/main.pdf"))

  return(list(date = sitting_date,
              text = pdf_text))
  }

  purrr::map(unique(base_data$sitting_day), debate_text)
}

create_plots <- function(cleaned_data){

(cleaned_data %>%
  filter(voted==TRUE) %>%
  group_by(senator_for) %>%
  count() %>%
  mutate(percentage = n / 48 * 100) %>%
  arrange(desc(percentage)) %>%
  mutate(half = ifelse(percentage > 50, TRUE, FALSE)) %>%
  arrange(desc(percentage)) %>%
  ggplot(aes(
    y = reorder(senator_for, percentage),
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
    x = "Percentage of voting days",
    title = "Percentage of voting days on which Senators voted at least once",
    subtitle = "Based on 48 voting days between June 2020 and July 2021",
    caption = stringr::str_wrap(
      "Total number of Senators shown is 56. The Cathaoirleach (who does not vote) was excluded, as were Senators who are Ministers (n=1), who resigned (n=2), or who replaced those who resigned (n=2)",
      100
    )
  ) +
  geom_text(
    x = 37.5,
    y = "Denis O'Donovan",
    label = "7 Senators voted\non less than 50% \nof voting days",
    size = 3
  ) +
  # theme_minimal() +
  ggthemes::theme_economist_white() +
  theme(
    axis.title = element_text(margin = margin(t = 10, r = 10, b = 10, l = 0)),
    title = element_text(margin = margin(t = 0, r = 0, b = 5, l = 0)),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title.position = "plot"
  )
) %>%
  ggsave(filename = file.path("figures/seanad_percentage.png"),
         height = 14,
         width = 9)


  (cleaned_data %>%
      filter(voting_day==TRUE) %>%
      group_by(senator_for) %>%
      summarise(days_voted = sum(voted),
                days_recorded = sum(attended)) %>%
      ggplot(aes(x=days_recorded,y=days_voted, label = senator_for)) +
      geom_point(
        aes(color = case_when(days_voted < 5 ~ "red",
                              days_recorded == 0 ~ "white",
                              T~"black"))
      ) +
      geom_text(
        aes(label = ifelse(senator_for == "Denis O'Donovan", senator_for, "")),
        nudge_x = 0,
        nudge_y = -0.9,
        hjust = .5,
        size = 4
      ) +
      geom_text(
        aes(label = ifelse(senator_for == "David Norris", senator_for, "")),
        nudge_x = 0,
        nudge_y = 0.9,
        hjust = .5,
        size = 4
      ) +
      scale_color_identity() +
      geom_abline(slope = 1, intercept = 0) +
      scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
      scale_x_continuous(limits = c(0, 50), expand = c(0, 0)) +
      ggthemes::theme_economist_white() +
      labs(y = "Number of voting days on which a Senator voted",
           x = "Number of voting days on which a Senator claimed attendence")+
    theme(
        axis.title = element_text(margin = margin(t = 10, r = 10, b = 10, l = 0)),
        title = element_text(margin = margin(t = 0, r = 0, b = 5, l = 0)),
      )) %>%
    ggsave(filename = "figures/seanad_attendance_vs_voting_days.png",
           height = 8,
           width = 8)


  (cleaned_data %>%
      filter(voting_day==TRUE) %>%
      mutate(contributions = as.numeric(as.character(contributions))) %>%
      mutate(actual_attended = case_when(voted == TRUE ~ TRUE,
                                         as.numeric(contributions) > 0 ~ TRUE,
                                         T ~ FALSE)) %>%
      group_by(senator_for) %>%
      summarise(days_voted = sum(actual_attended),
                days_recorded = sum(attended)) %>%
      ggplot(aes(x=days_recorded,y=days_voted, label = senator_for)) +
      geom_point(
        aes(color = case_when(days_voted <15 ~ "red",
                              days_recorded == 0 ~ "white",
                              T~"black"))
        ) +
      geom_text(
        aes(label = ifelse(senator_for == "Denis O'Donovan", senator_for, "")),
        nudge_x = 0,
        nudge_y = -0.9,
        hjust = .5,
        size = 4
      ) +
      geom_text(
        aes(label = ifelse(senator_for == "David Norris", senator_for, "")),
        nudge_x = 0,
        nudge_y = 0.9,
        hjust = .5,
        size = 4
      ) +
      scale_color_identity() +
      geom_abline(slope = 1, intercept = 0) +
      scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
      scale_x_continuous(limits = c(0, 50), expand = c(0, 0)) +
      ggthemes::theme_economist_white() +
      theme(
        axis.title = element_text(margin = margin(t = 10, r = 10, b = 10, l = 0)),
        title = element_text(margin = margin(t = 0, r = 0, b = 5, l = 0))
      ) +
      labs(y = "Number of voting days on which there was evidence of a Senator's attendance",
           x = "Number of voting days on which a Senator claimed attendence"))  %>%
    ggsave(filename = "figures/seanad_claimed_vs_actual_attendence.png",
           height = 8,
           width = 8)

}





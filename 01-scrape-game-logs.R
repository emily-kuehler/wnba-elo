library(tidyverse)
library(rvest)
library(lubridate)

get_wnba_bball_ref_team_urls <- function() {
  
  base_teams_url <- "https://www.basketball-reference.com/wnba/teams/"
  
  webpage <- read_html(base_teams_url)
  
  active_teams <- webpage %>%
    html_nodes("table#active") %>%
    html_table() %>%
    pluck(1)
  
  defunct_teams <- webpage %>%
    html_nodes("table#defunct") %>%
    html_table() %>%
    pluck(1)
  
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  
  team_url_df <- tibble(link = link_, url = url_) %>% 
    filter(str_detect(url,"/wnba/teams/")) %>% 
    filter(link != "Teams") %>% 
    rename(Team = link) %>% 
    mutate(Team = str_trim(Team)) %>% 
    arrange(Team) %>% 
    distinct()
  
  active_team_df <- active_teams %>%
    mutate(Team = str_trim(Team)) %>% 
    inner_join(team_url_df)
  
  defunct_team_df <- defunct_teams %>% 
    mutate(Team = str_trim(Team)) %>% 
    inner_join(team_url_df)
  
  team_df <- active_team_df %>% 
    bind_rows(defunct_team_df) %>% 
    arrange(url)
  
  duplicate_urls <- team_df %>% 
    group_by(url) %>% 
    filter(n() > 1) %>% 
    filter(Yrs == min(Yrs))
  
  team_df_unique_url <- team_df %>% 
    anti_join(duplicate_urls)
  
  return (team_df_unique_url)
  
}

























curr_season <- 2020

test_team_game_log_url <- paste0("https://www.basketball-reference.com/", test_team_url, "/", curr_season, "_games.html")






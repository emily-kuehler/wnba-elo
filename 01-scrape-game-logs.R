library(tidyverse)
library(rvest)
library(lubridate)


# scrape team links -------------------------------------------------------

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


# scrape team game logs ---------------------------------------------------

team_urls <- get_wnba_bball_ref_team_urls()

#function will take a url and a season, then scrape game info


try_catch_read_html <- function(x) {
  tryCatch({
    read_html(x)
  }, error = function(e) {
    NA
  })
}


get_season_game_logs <- function(team_url, season, team_url_df) {
  
  team_game_log_url <- paste0("https://www.basketball-reference.com/", team_url, season, "_games.html")
  
  webpage <- try_catch_read_html(team_game_log_url)
  
  curr_team <- team_url_df %>% 
    filter(url == team_url) %>% 
    pull(Team)
  
  if (!is.na(webpage)) {
    
    str_vars <- c("date","home_away","opponent","win_loss","streak")
    num_vars <- c("game_id","team_pts","opp_pts","w","l")
    
    game_logs_reg <- webpage %>%
      html_nodes("table#teams_games") %>% 
      html_table() %>% 
      pluck(1) %>% 
      rename(game_id = G,
             date = Date,
             home_away = 3,
             opponent = Opponent,
             win_loss = 5,
             team_pts = Tm,
             opp_pts = Opp,
             w = W,
             l = L,
             streak = Streak) %>% 
      filter(game_id != 'G') %>% 
      mutate_at(str_vars, as.character) %>% 
      mutate_at(num_vars, as.numeric) %>% 
      mutate(season_type = "reg")
    
    game_logs_post <- webpage %>% 
      html_nodes("table#teams_games_playoffs") %>% 
      html_table() %>% 
      pluck(1)
    
    if (!is.null(game_logs_post)) {
      
      game_logs_post <- game_logs_post %>% 
        rename(game_id = G,
               date = Date,
               home_away = 3,
               opponent = Opponent,
               win_loss = 5,
               team_pts = Tm,
               opp_pts = Opp,
               w = W,
               l = L,
               streak = Streak) %>% 
        filter(game_id != 'G') %>% 
        mutate_at(str_vars, as.character) %>% 
        mutate_at(num_vars, as.numeric) %>% 
        mutate(season_type = "post")
      
    }
      
    
    game_logs <- game_logs_reg %>% 
      bind_rows(game_logs_post)
    
    game_logs <- game_logs %>% 
      mutate(team = curr_team) %>% 
      select(-game_id) %>% 
      mutate(game_num = row_number())
    
    return (game_logs)
    
  } else {
    
    return (NULL)
    
  }
  
}

game_logs_df <- get_season_game_logs(team_urls$url[1], 2014, team_urls)







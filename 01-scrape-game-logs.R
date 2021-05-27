library(tidyverse)
library(rvest)
library(lubridate)


# scrape team links -------------------------------------------------------

get_wnba_bball_ref_team_urls <- function() {
  
  base_teams_url <- "https://www.basketball-reference.com/wnba/teams/"
  base_teams_url <- try_catch_get_url(base_teams_url)
  
  #base_teams_url <- url(base_teams_url, "rb")
  
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
  
  #close.connection(base_teams_url)
  
  return (team_df_unique_url)
  
}


# scrape team game logs ---------------------------------------------------

#actually reads the html
try_catch_read_html <- function(url_con) {
  tryCatch({
    read_html(url_con)
  }, error = function(e) {
    NA
  })
}

#just returns a url object
try_catch_get_url <- function(x) {
  tryCatch({
    url_obj <- url(x)
  }, error = function(e) {
    NA
  })
}


get_season_game_logs <- function(team_url, season, team_url_df) {
  
  team_game_log_url_str <- paste0("https://www.basketball-reference.com/", team_url, season, "_games.html")
  
  team_game_log_url <- try_catch_get_url(team_game_log_url_str)
  
  curr_team <- team_url_df %>% 
    filter(url == team_url) %>% 
    pull(Team)
  
  webpage <- try_catch_read_html(team_game_log_url)
  
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
      mutate(team = curr_team,
             team_game_num = row_number(),
             home_team = ifelse(home_away == '',team,opponent),
             visit_team = ifelse(home_away == '@',team,opponent),
             season = season,
             club_code = team_url,
             win_loss = ifelse(win_loss == "W",1,0)) %>% 
      select(team_game_num,
             date,
             team,
             opponent,
             home_team,
             visit_team,
             team_pts,
             opp_pts,
             win_loss,
             season_type,
             season,
             club_code
             )
    
    #close.connection(team_game_log_url)
    
    print(paste0("got ", season, " data for ", curr_team))
    
    return (game_logs)
    
  } else {
    
    close.connection(team_game_log_url)
    print(showConnections())
    
    return (NULL)
    
  }
  
}


# scrape all historical gamelogs ------------------------------------------

scrape_historical_gamelogs <- function() {
  
  team_url_df <- get_wnba_bball_ref_team_urls()
  
  first_wnba_season <- min(team_url_df$From)
  last_completed_wnba_season <- max(team_url_df$To) - 1
  #last_completed_wnba_season <- 2002
  seasons <- seq(from = first_wnba_season, to = last_completed_wnba_season)
  
  unique_team_urls <- unique(team_url_df$url)
  
  url_season_list <- list(url = unique_team_urls, season = seasons)
  url_season_cross <- cross_df(url_season_list)
  
  historical_game_logs <- map2_df(.x = url_season_cross$url, .y = url_season_cross$season, .f = get_season_game_logs, team_url_df)
  
  return (historical_game_logs)
  
}

historical_game_logs <- scrape_historical_gamelogs()





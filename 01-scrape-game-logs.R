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
  
  #go to next page
  # team_active_season <- get_team_active_seasons(team_df_unique_url$url[2])
  
  team_active_seasons <- map(.x = team_df_unique_url$url, .f = get_team_active_seasons) %>%
    bind_rows()
  
  return (team_active_seasons)
  
}

get_team_active_seasons <- function(team_url) {
  
  team_code <- str_sub(team_url, start = -4, end = -2)
  table_node <- paste0("table#",team_code)
  
  full_team_url <- paste0("https://www.basketball-reference.com",team_url)
  
  wnba_full_team_url <- try_catch_get_url(full_team_url)
  
  wnba_team_page <- try_catch_read_html(wnba_full_team_url)
  
  url_ <- wnba_team_page %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  
  team_table <- wnba_team_page %>% 
    html_nodes(table_node) %>% 
    html_table() %>% 
    pluck(1) %>% 
    select(season = Year,
           team = Team)
  
  team_url_df <- tibble(url = url_) %>% 
    filter(str_detect(url,"/wnba/teams/[A-Za-z][A-Za-z][A-Za-z]/[0-9][0-9]")) %>% 
    mutate(season = as.numeric(str_sub(url, start = -9, end = -6)))
  
  team_table_df <- team_table %>% 
    inner_join(team_url_df) %>% 
    mutate(team = str_replace(team, "\\*", ''),
           team_code = team_code,
           games_url = str_sub(url,1,nchar(url)-5),
           games_url = paste0(games_url,"_games.html"))
  
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
  
  team_game_log_url_str <- paste0("https://www.basketball-reference.com/", team_url)
  
  team_game_log_url <- try_catch_get_url(team_game_log_url_str)
  
  curr_team <- team_url_df %>% 
    filter(games_url == team_url) %>% 
    pull(team)
  
  curr_team_code <- team_url_df %>% 
    filter(games_url == team_url) %>% 
    pull(team_code)
  
  print("8====D")
  print(curr_team_code)
  
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
             club_code = curr_team_code,
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
    
    print(team_game_log_url)
    close.connection(team_game_log_url)
    print(showConnections())
    
    return (NULL)
    
  }
  
}


# scrape all historical gamelogs ------------------------------------------

scrape_historical_gamelogs <- function() {
  
  team_url_df <- get_wnba_bball_ref_team_urls()
  
  curr_season <- max(team_url_df$season)
  
  historical_team_url_df <- team_url_df %>% 
    filter(season != curr_season)
  
  historical_game_logs <- map2_df(.x = historical_team_url_df$games_url, 
                                  .y = historical_team_url_df$season,
                                  .f = get_season_game_logs,
                                  historical_team_url_df)
  
  return (historical_game_logs)
  
}

scrape_current_season_gamelogs <- function() {
  
  team_url_df <- get_wnba_bball_ref_team_urls()
  
  curr_season <- max(team_url_df$season)
  
  curr_team_url_df <- team_url_df %>% 
    filter(season == curr_season)
  
  game_logs <- map2_df(.x = curr_team_url_df$games_url,
                       .y = curr_team_url_df$season,
                       .f = get_season_game_logs,
                       curr_team_url_df)
  
  return (game_logs)
  
}
  
historical_gamelogs <- scrape_historical_gamelogs()

current_season_gamelogs <- scrape_current_season_gamelogs()





  
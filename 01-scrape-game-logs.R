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
        mutate(home_away = ifelse(is.na(home_away) , "" , home_away)) %>%
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
             ) %>% 
      mutate(team = ifelse(str_detect(team, "San Antonio"), "San Antonio", team),
             opponent = ifelse(str_detect(opponent, "San Antonio"), "San Antonio", opponent))
    
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


# scrape league standings -------------------------------------------------

get_league_standings <- function(curr_season) {
  
  base_url <- "https://www.basketball-reference.com/wnba/years/"
  
  conference_standings_str <- paste0(base_url, curr_season, ".html")
  
  webpage <- rvest::read_html(conference_standings_str)
  
  eastern_conf_standings <- webpage %>%
    rvest::html_nodes("table#standings_e") %>%
    rvest::html_table() %>%
    pluck(1) %>% 
    mutate(conference = "eastern") %>% 
    rename(team = `Eastern Conference`)
  
  western_conf_standings <- webpage %>% 
    rvest::html_nodes("table#standings_w") %>% 
    rvest::html_table() %>% 
    pluck(1) %>% 
    mutate(conference = "western") %>% 
    rename(team = `Western Conference`)
  
  season_standings <- eastern_conf_standings %>% 
    bind_rows(western_conf_standings) %>% 
    mutate(season = curr_season)
  
  print(curr_season)
  
  return (season_standings)
  
}

get_playoff_table <- function(curr_season) {
  
  playoff_rounds <- c("First Round", "Second Round", 
                      "Eastern Conference Semifinals",
                      "Western Conference Semifinals",
                      "Semifinals",
                      "Finals")
  
  base_url <- "https://www.basketball-reference.com/wnba/years/"
  
  conference_standings_str <- paste0(base_url, curr_season, ".html")
  
  webpage <- rvest::read_html(conference_standings_str)
  
  #save and work with the body
  body <- rvest::html_node(webpage, "body") 
  xml2::write_xml(body, "data/temp.xml")
  
  #find and remove comments
  lines <- readLines("data/temp.xml")
  lines<-lines[-grep("<!--", lines)]
  lines<-lines[-grep("-->", lines)]
  writeLines(lines, "data/temp.xml")
  
  #read the file back in and process normally
  body <- rvest::read_html("data/temp.xml")
  
  playoff_results <- body %>%
    rvest::html_nodes("table#all_playoffs") %>%
    rvest::html_table() %>%
    pluck(1) %>% 
    filter(X1 %in% playoff_rounds) %>% 
    select(round = X1,
           result = X2) %>% 
    mutate(season = curr_season)
  
  #remove unneeded xml file
  if(file.exists("data/temp.xml")) {
    file.remove("data/temp.xml")
  }
  
  print(curr_season)
  
  return(playoff_results)
  
}

scrape_league_standings <- function() {
  
  league_standings <- map(.x = INIT_SEASON:FINAL_SEASON, .f = get_league_standings) %>% 
    bind_rows()
  
  return(league_standings)
}


scrape_playoff_results <- function() {
  
  playoff_results <- map(.x = INIT_SEASON:FINAL_SEASON, .f = get_playoff_table) %>% 
    bind_rows() %>% 
    separate(col = result, into = c("winner", "loser"), sep = "over") %>% 
    mutate(loser2 = str_sub(loser, 1,-8),
           record = str_sub(loser, -4,-2)) %>% 
    select(-loser) %>% 
    rename(loser = loser2)
  
  return(playoff_results)
  
}

test_playoffs <- scrape_playoff_results()




# clean gamelogs ----------------------------------------------------------

get_average_win_loss_scores <- function(game_log_df, curr_season = 2018) {
  
  avg_scores <- historical_gamelogs %>% 
    filter(season == curr_season) %>% 
    filter(!is.na(team_pts) & !is.na(opp_pts)) %>% 
    mutate(win_score = if_else(win_loss == 1, team_pts, opp_pts),
           losing_score = if_else(win_loss == 1, opp_pts, team_pts)) %>% 
    summarise(avg_win_score = round(mean(win_score)),
              avg_losing_score = round(mean(losing_score)))
  
  return (avg_scores)
  
}




#there was a forfeit in 2018, fill with average win / loss scores
clean_game_logs <- function(historical_gamelogs) {
  
  scores_2018 <- get_average_win_loss_scores(historical_gamelogs)
  winner_pts <- scores_2018$avg_win_score
  loser_pts <- scores_2018$avg_losing_score
  
  historical_gamelogs <- historical_gamelogs %>% 
    mutate(team_pts = if_else(team_game_num == 27 & club_code == 'SAS', loser_pts, team_pts),
           team_pts = if_else(team_game_num == 27 & club_code == 'WAS', winner_pts, team_pts),
           opp_pts = if_else(team_game_num == 27 & club_code == 'SAS', winner_pts, opp_pts),
           opp_pts = if_else(team_game_num == 27 & club_code == 'WAS', loser_pts, opp_pts))
  
  return (historical_gamelogs)
  
}
  
historical_gamelogs <- scrape_historical_gamelogs()

clean_game_logs_df <- clean_game_logs(historical_gamelogs)

current_season_gamelogs <- scrape_current_season_gamelogs()

league_standings <- scrape_league_standings()

playoff_results <- scrape_playoff_results()
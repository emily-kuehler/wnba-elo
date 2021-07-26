library(lubridate)

# source("00-load-params.R")
# source("01-scrape-game-logs.R")

calculate_all_elo_vals <- function(game_log_df) {
  
  elo_list <- list()
  
  elo_list[[1]] <- calculate_single_season(game_log_df = game_log_df, season = INIT_SEASON)
  
  for (i in (INIT_SEASON + 1):FINAL_SEASON) {
    
    last_element <- length(elo_list)
    
    curr_elo_df <- elo_list[[last_element]]
    
    elo_vals <- calculate_single_season(game_log_df, season = i, elo_df = curr_elo_df)
    
    elo_list[[i-INIT_SEASON + 1]] <- elo_vals
    
    print (i)
    
  }
  
  return (elo_list)
  
}


initialize_elo_values <- function(game_log_df, curr_season = 1997, elo_df = NULL) {
  
  if (curr_season == 1997) {
    
    game_logs <- game_log_df %>% 
      filter(season == curr_season) %>% 
      mutate(pregame_elo = ifelse(team_game_num == 1, INIT_ELO, NA_integer_))
    
  } else {
    
    #print ("8=====D")
    
    #initialize with values from end of previous season
    prev_season_df <- elo_df %>%
      filter(season == curr_season - 1) %>% 
      group_by(team) %>% 
      filter(team_game_num == max(team_game_num)) %>% 
      ungroup() %>% 
      select(team, pregame_elo_tm = post_game_elo_tm) %>% 
      mutate(team_game_num = 1)
    
    game_logs <- game_log_df %>% 
      filter(season == curr_season) %>% 
      left_join(prev_season_df, by = c("team", "team_game_num")) %>%
      mutate(pregame_elo = ifelse(is.na(pregame_elo_tm) & team_game_num == 1, INIT_ELO, pregame_elo_tm)) %>% 
      select(-pregame_elo_tm)
    
  }
  
  game_logs_test <- game_logs %>% 
    inner_join(game_logs, by = c("team"="opponent","date")) %>% 
    rename(pregame_elo_tm = pregame_elo.x,
           pregame_elo_opp = pregame_elo.y,
           team_game_num = team_game_num.x,
           opp_game_num = team_game_num.y,
           team_club_code = club_code.x,
           opp_club_code = club_code.y) %>% 
    select(-contains(".y")) %>% 
    rename_all(funs(stringr::str_replace_all(., ".x", ""))) %>% 
    mutate(game_date = lubridate::mdy(str_sub(date, start = 5, end = -1))) %>% 
    arrange(game_date) %>% 
    # calculate postgame elo for initialized elo values
    mutate(s_val = ifelse(win_loss == 1,1,0),
           home_team = ifelse(home_team == team,1,0),
           pregame_elo_tm = ifelse(pregame_elo_tm != INIT_ELO & team_game_num == 1, 0.75 * pregame_elo_tm + 1505 * 0.25, pregame_elo_tm),
           pregame_elo_opp = ifelse(pregame_elo_opp != INIT_ELO & opp_game_num == 1, 0.75 * pregame_elo_opp + 1505 * 0.25, pregame_elo_opp),
           pregame_elo_tm_adj = ifelse(home_team == 1, pregame_elo_tm + 100, pregame_elo_tm),
           pregame_elo_opp_adj = ifelse(home_team == 0, pregame_elo_opp + 100, pregame_elo_opp),
           winner_elo = ifelse(s_val == 1,pregame_elo_tm,pregame_elo_opp),
           loser_elo = ifelse(s_val == 0,pregame_elo_tm,pregame_elo_opp),
           wp_expon = (pregame_elo_opp - pregame_elo_tm) / 400,
           wp = 1 / (1 + 10 ** wp_expon),
           mov_winner = abs(team_pts - opp_pts),
           elo_diff_winner = winner_elo - loser_elo,
           k_val_num = (mov_winner + 3) ** 0.8,
           k_val_denom = 7.5 + 0.006 * (elo_diff_winner),
           k_val = 20 * k_val_num / k_val_denom,
           post_game_elo_tm = k_val * (s_val - wp) + pregame_elo_tm
           
    )
           
  
}


calculate_single_season <- function(game_log_df, season, elo_df) {
  
  initialized_df <- initialize_elo_values(game_log_df = game_log_df, curr_season = season, elo_df = elo_df)
  
  #return (initialized_df)
  
  #write function to get opp and team pregame elo
  for (i in 1:nrow(initialized_df)) {
    
    curr_row <- initialized_df[i,]
    team_elo <- curr_row$pregame_elo_tm
    opp_elo <- curr_row$pregame_elo_opp
    
    #need to write this function
    if (is.na(team_elo) | is.na(opp_elo)) {

      curr_row <- get_prev_elo_vals(initialized_df, i)
      
      
      team_elo <- curr_row$pregame_elo_tm
      opp_elo <- curr_row$pregame_elo_opp
      
      curr_row <- curr_row %>%
        mutate(s_val = ifelse(win_loss == 1,1,0),
               home_team = ifelse(home_team == team,1,0),
               #pregame_elo_tm = 0.75 * pregame_elo_tm + 1505 * 0.25,
               #pregame_elo_opp = 0.75 * pregame_elo_opp + 1505 * 0.25,
               pregame_elo_tm_adj = ifelse(home_team == 1, pregame_elo_tm + 100, pregame_elo_tm),
               pregame_elo_opp_adj = ifelse(home_team == 0, pregame_elo_opp + 100, pregame_elo_opp),
               winner_elo = ifelse(s_val == 1,pregame_elo_tm,pregame_elo_opp),
               loser_elo = ifelse(s_val == 0,pregame_elo_tm,pregame_elo_opp),
               wp_expon = (pregame_elo_opp - pregame_elo_tm) / 400,
               wp = 1 / (1 + 10 ** wp_expon),
               mov_winner = abs(team_pts - opp_pts),
               elo_diff_winner = winner_elo - loser_elo,
               k_val_num = (mov_winner + 3) ** 0.8,
               k_val_denom = 7.5 + 0.006 * (elo_diff_winner),
               k_val = 20 * k_val_num / k_val_denom,
               post_game_elo_tm = k_val * (s_val - wp) + pregame_elo_tm
               
        )
      
      initialized_df[i,] <- curr_row

    }
    
    
  }
  
  return(initialized_df)
  
}


#need to calculate postgame elo for initialized values
#logic: if pregame_elo_tm 
get_prev_elo_vals <- function(game_log_df, row_num) {
  
  #print (row_num)
  
  curr_row <- game_log_df[row_num,]
  
  curr_team <- curr_row$team
  curr_opp <- curr_row$opponent
  
  curr_game_num_team <- curr_row$team_game_num
  curr_game_num_opp <- curr_row$opp_game_num
  
  if (is.na(curr_row$pregame_elo_tm)) {
    
    prev_row_team <- game_log_df %>% filter(team == curr_team & team_game_num == (curr_game_num_team - 1))
    team_elo <- prev_row_team$post_game_elo_tm
    curr_row$pregame_elo_tm <- team_elo
    
  }
  
  if (is.na(curr_row$pregame_elo_opp)) {
    
    prev_row_opp <- game_log_df %>% filter(team == curr_opp & team_game_num == curr_game_num_opp - 1)
    opp_elo <- prev_row_opp$post_game_elo_tm
    curr_row$pregame_elo_opp <- opp_elo
    
  }
  
  return (curr_row)
  
}

elo_val_list <- calculate_all_elo_vals(clean_game_logs_df) %>% 
  bind_rows()


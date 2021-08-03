source("helper-functions.R")


my_con <- connect_to_aws_db()

# simulate completed seasons ----------------------------------------------

#simulates playoffs of current season
simulate_playoffs <- function(curr_season) {
  
  #collect data (universal)
  
  #league standings
  standings <- DBI::dbGetQuery(my_con, "SELECT * FROM wnba_league_standings") %>% 
    filter(season == curr_season) %>% 
    mutate(team = str_remove(team, "[*]"))
  
  print("collected standings...")
  
  #get initial playoff values
  init_playoff_elo_vals <- DBI::dbGetQuery(my_con, "SELECT * FROM wnba_elo_vals") %>% 
    filter(season == curr_season & season_type == 'post') %>% 
    group_by(team) %>% 
    filter(game_date == min(game_date)) %>% 
    select(team, opponent, pregame_elo_tm, pregame_elo_opp, wp)
  
  print("collected init elo vals")
  
  #initial playoff matchups
  matchups <- DBI::dbGetQuery(my_con, "SELECT * FROM wnba_playoff_results") %>% 
    filter(season == curr_season) %>% 
    mutate(winner = str_trim(winner),
           loser = str_trim(loser))
  
  print("collected matchups")
  
  #clean data for relevant season
  #simulate playoffs using rules for relevant season
  if (curr_season == 1997) {
    
    playoff_sim_results <- simulate_four_team_playoff(matchup_df = matchups,
                                                      standings_df = standings,
                                                      init_elos = init_playoff_elo_vals,
                                                      single_elimination = T)
    
    return (playoff_sim_results)
    
  }
  
  
  #should be same as 1997, but we do a best of three instead of single elimination
  if (curr_season == 1998) {
    
    playoff_sim_results <- simulate_four_team_playoff(matchup_df = matchups,
                                                      standings_df = standings,
                                                      init_elos = init_playoff_elo_vals,
                                                      single_elimination = F)
    
    return (playoff_sim_results)
    
  }
  
  if (curr_season == 1999) {
    
    playoff_sim_results <- simulate_six_team_playoff(matchup_df = matchups,
                                                     standings_df = standings,
                                                     init_elos = init_playoff_elo_vals)
    
    return (playoff_sim_results)
    
  }
  
}


# helper functions for small sims -----------------------------------------

#simulates elimination round N times
simulate_elimination_round <- function(win_prob_tm1) {
  
  results <- sample(x = c("wins_tm_1", "wins_tm_2"),
                    size = N_SIMS,
                    replace = T,
                    prob = c(win_prob_tm1,
                             1 - win_prob_tm1)) %>% 
    as_tibble() %>% 
    count(value) %>% 
    rename(team = value,
           wins = n) %>% 
    filter(team == "wins_tm_1") %>% 
    pull(wins)
  
  return(results)
  
}


#simulates best of three series N times
simulate_best_of_three_series <- function(win_prob_tm1) {
  
  sim_results <- replicate(N_SIMS, get_three_game_series_winner(win_prob_tm1)) %>% 
    as_tibble() %>%
    count(value) %>%
    rename(team = value,
           wins = n) %>% 
    filter(team == "wins_tm_1") %>%
    pull(wins)
  
  return (sim_results)

}


#returns series winner if best of three series given win probability
get_three_game_series_winner <- function(win_prob) {
  
  three_game_winner <- sample(x = c("wins_tm_1", "wins_tm_2"), 
                              size = 3, 
                              replace = T, 
                              prob = c(win_prob,
                                       1 - win_prob)) %>% 
    as_tibble() %>% 
    count(value) %>% 
    rename(team = value,
           wins = n) %>% 
    filter(wins >= 2) %>% 
    pull(team)
  
  return (three_game_winner)
  
}


#returns finals results for 1997, 1998
simulate_1997_1998_finals <- function(semifinals_results, wnba_standings, single_elim) {
  
  finals_sim <- semifinals_results %>%
    inner_join(wnba_standings, by = c("series_winner" = "team")) %>% 
    select(team = series_winner,
           pregame_elo_tm = pregame_elo_winner,
           reg_season_wins = W,
           reg_season_losses = L,
           win_pct = `W/L%`)
  
  finals_wide <- as_tibble(bind_cols(finals_sim[1,], finals_sim[2,]))
  final_round_cols <- c("team1","pregame_elo_tm1","reg_season_wins_tm1","reg_season_losses_tm1", "win_pct_tm1",
                        "team2","pregame_elo_tm2","reg_season_wins_tm2","reg_season_losses_tm2", "win_pct_tm2")
  names(finals_wide) <- final_round_cols
  
  finals_df <- finals_wide %>% 
    mutate(home_team = if_else(win_pct_tm1 >= win_pct_tm2, 1, 0),
           pregame_elo_tm_adj = ifelse(home_team == 1, pregame_elo_tm1 + 100, pregame_elo_tm1),
           pregame_elo_opp_adj = ifelse(home_team == 0, pregame_elo_tm2 + 100, pregame_elo_tm2),
           wp_expon = (pregame_elo_tm2 - pregame_elo_tm1) / 400,
           wp = 1 / (1 + 10 ** wp_expon)) %>% 
    select(team1, team2, pregame_elo_tm1, pregame_elo_tm2, wp_tm1 = wp) %>% 
    mutate(wp_tm2 = 1 - wp_tm1)
  
  #gives wins for team 1
  if (single_elim == T) {
    
    finals_results <- simulate_elimination_round(finals_df$wp_tm1)
    
  } else {
    
    finals_results <- simulate_best_of_three_series(finals_df$wp_tm1)
    
  }
  
  
  finals_df <- finals_df %>% 
    mutate(team_1_wins = finals_results,
           team_2_wins = N_SIMS - team_1_wins,
           series_winner = if_else(team_1_wins > team_2_wins, team1, team2),
           pregame_elo_winner = if_else(team_1_wins > team_2_wins, pregame_elo_tm1, pregame_elo_tm2),
           pregame_elo_loser = if_else(team_1_wins > team_2_wins, pregame_elo_tm2, pregame_elo_tm1)) %>% 
    select(team1, team2, team_1_wins, team_2_wins, series_winner, pregame_elo_winner, pregame_elo_loser)
  
  return (finals_df)
  
}



# four team playoffs ------------------------------------------------------

#function works for 1997, 1998 playoffs
#single elimination for 1997, best of 3 for 1998
simulate_four_team_playoff <- function(matchup_df, 
                                       standings_df,
                                       init_elos,
                                       single_elimination = T) {
  
  
  matchups <- matchup_df %>% 
    filter(round == "Semifinals")
  
  #join matchups to init values
  init_matchups <- matchups %>% 
    inner_join(init_elos, by = c("winner" = "team")) %>% 
    rename(team1 = winner,
           team2 = loser,
           pregame_elo_tm1 = pregame_elo_tm,
           pregame_elo_tm2 = pregame_elo_opp,
           wp_tm1 = wp) %>% 
    select(-opponent) %>% 
    mutate(wp_tm2 = 1 - wp_tm1)
  
  #simulate round 1 playoffs
  if (single_elimination == T) {
    
    round1_results <- map(.x = init_matchups$wp_tm1, .f = simulate_elimination_round) %>% 
      unlist()
    
  } else {
    
    # do best of three simulation
    round1_results <- map(.x = init_matchups$wp_tm1, .f = simulate_best_of_three_series) %>% 
      unlist()
    
    # return (round1_results)
    
  }
  
  results <- init_matchups %>% 
    mutate(team_1_wins = round1_results,
           team_2_wins = N_SIMS - team_1_wins,
           series_winner = if_else(team_1_wins > team_2_wins, team1, team2),
           pregame_elo_winner = if_else(team_1_wins > team_2_wins, pregame_elo_tm1, pregame_elo_tm2),
           pregame_elo_loser = if_else(team_1_wins > team_2_wins, pregame_elo_tm2, pregame_elo_tm1)) %>% 
    select(team1, team2, team_1_wins, team_2_wins, series_winner, pregame_elo_winner, pregame_elo_loser)
  
  #simulate finals
  if (single_elimination == T) {
    
    finals_results <- simulate_1997_1998_finals(semifinals_results = results,
                                           wnba_standings = standings_df,
                                           single_elim = T)
    return (list(results, finals_results))
    
    
    
  } else {
    
    finals_results <- simulate_1997_1998_finals(semifinals_results = results,
                                                wnba_standings = standings_df,
                                                single_elim = F)
    
    return (list(results, finals_results))
    
  }
  
  
}



# six team playoffs -------------------------------------------------------

simulate_six_team_playoff <- function(matchup_df, 
                                      standings_df,
                                      init_elos) {
  
  matchups <- matchup_df %>% 
    filter(str_detect(round, "First Round"))
  
  #join matchups to init values
  init_matchups <- matchups %>% 
    inner_join(init_elos, by = c("winner" = "team")) %>% 
    rename(team1 = winner,
           team2 = loser,
           pregame_elo_tm1 = pregame_elo_tm,
           pregame_elo_tm2 = pregame_elo_opp,
           wp_tm1 = wp) %>% 
    select(-opponent) %>% 
    mutate(wp_tm2 = 1 - wp_tm1)
  
  #get results from play in rounds
  play_in_results <- map(.x = init_matchups$wp_tm1, .f = simulate_elimination_round) %>% 
    unlist()
  
  play_in_results_df <- init_matchups %>% 
    mutate(team_1_wins = play_in_results,
           team_2_wins = N_SIMS - team_1_wins,
           series_winner = if_else(team_1_wins > team_2_wins, team1, team2),
           pregame_elo_winner = if_else(team_1_wins > team_2_wins, pregame_elo_tm1, pregame_elo_tm2),
           pregame_elo_loser = if_else(team_1_wins > team_2_wins, pregame_elo_tm2, pregame_elo_tm1)) %>% 
    select(team1, team2, team_1_wins, team_2_wins, series_winner, pregame_elo_winner, pregame_elo_loser)
  
  #get semifinals matchups
  semi_final_matchups <- standings_df %>%
    group_by(conference) %>%
    mutate(seed = row_number()) %>%
    filter(seed == 1 | team %in% play_in_results_df$series_winner) %>%
    ungroup() %>% 
    inner_join(init_elos) %>% 
    select(team,
           pregame_elo_tm,
           conference,
           seed) %>% 
    group_by(conference) %>% 
    arrange(seed)
  
  return (list(init_matchups, semi_final_matchups))
  
  
}


test_sim <- simulate_playoffs(curr_season = 1999)








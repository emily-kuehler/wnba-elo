source("helper-functions.R")


my_con <- connect_to_aws_db()

# simulate completed seasons ----------------------------------------------

test_sim <- simulate_playoffs(curr_season = 1997)

simulate_playoffs <- function(curr_season) {
  
  #league standings
  standings <- DBI::dbGetQuery(my_con, "SELECT * FROM wnba_league_standings") %>% 
    filter(season == 1997) %>% 
    mutate(team = str_remove(team, "[*]"))
  
  print("collected standings...")
  
  #get initial playoff values
  init_playoff_elo_vals <- DBI::dbGetQuery(my_con, "SELECT * FROM wnba_elo_vals") %>% 
    filter(season == 1997 & season_type == 'post') %>% 
    group_by(team) %>% 
    filter(game_date == min(game_date)) %>% 
    select(team, opponent, pregame_elo_tm, pregame_elo_opp, wp)
  
  print("collected init elo vals")
  
  #initial playoff matchups
  matchups <- DBI::dbGetQuery(my_con, "SELECT * FROM wnba_playoff_results") %>% 
    filter(season == 1997 & round == "Semifinals") %>% 
    mutate(winner = str_trim(winner),
           loser = str_trim(loser))
  
  print("collected matchups")
  
  #join matchups to init values
  init_matchups <- matchups %>% 
    inner_join(init_playoff_elo_vals, by = c("winner" = "team")) %>% 
    rename(team1 = winner,
           team2 = loser,
           pregame_elo_tm1 = pregame_elo_tm,
           pregame_elo_tm2 = pregame_elo_opp,
           wp_tm1 = wp) %>% 
    select(-opponent) %>% 
    mutate(wp_tm2 = 1 - wp_tm1)
  
  #simulate round 1 playoffs
  round1_results <- map(.x = init_matchups$wp_tm1, .f = simulate_elimination_round) %>% 
    unlist()
  
  results <- init_matchups %>% 
    mutate(team_1_wins = round1_results,
           team_2_wins = N_SIMS - team_1_wins,
           series_winner = if_else(team_1_wins > team_2_wins, team1, team2),
           pregame_elo_winner = if_else(team_1_wins > team_2_wins, pregame_elo_tm1, pregame_elo_tm2),
           pregame_elo_loser = if_else(team_1_wins > team_2_wins, pregame_elo_tm2, pregame_elo_tm1)) %>% 
    select(team1, team2, team_1_wins, team_2_wins, series_winner, pregame_elo_winner, pregame_elo_loser)
  
  #simulate finals
  finals_results <- simulate_1997_finals(semifinals_results = results,
                                         wnba_standings = standings)
    
  
  return (list(results, finals_results))
  
}

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


simulate_1997_finals <- function(semifinals_results, wnba_standings) {
  
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
  finals_results <- simulate_elimination_round(finals_df$wp_tm1)
  
  finals_df <- finals_df %>% 
    mutate(team_1_wins = finals_results,
           team_2_wins = N_SIMS - team_1_wins,
           series_winner = if_else(team_1_wins > team_2_wins, team1, team2),
           pregame_elo_winner = if_else(team_1_wins > team_2_wins, pregame_elo_tm1, pregame_elo_tm2),
           pregame_elo_loser = if_else(team_1_wins > team_2_wins, pregame_elo_tm2, pregame_elo_tm1)) %>% 
    select(team1, team2, team_1_wins, team_2_wins, series_winner, pregame_elo_winner, pregame_elo_loser)
  
  return (finals_df)
  
}








# finals_test <- test_sim %>%
#   inner_join(standings, by = c("series_winner" = "team")) %>% 
#   select(team = series_winner,
#          pregame_elo_tm = pregame_elo_winner,
#          reg_season_wins = W,
#          reg_season_losses = L,
#          win_pct = `W/L%`)
# 
# finals_sim <- as_tibble(bind_cols(finals_test[1,], finals_test[2,]))
# final_round_cols <- c("team1","pregame_elo_team1","reg_season_wins_team1","reg_season_losses_team1", "win_pct_team1",
#                       "team2","pregame_elo_team2","reg_season_wins_team2","reg_season_losses_team2", "win_pct_team2")
# names(finals_sim) <- final_round_cols
# 
# #need to calculate win probability
# #calculate win probability for team1
# finals_df <- finals_sim %>% 
#   mutate(home_team = if_else(win_pct_team1 >= win_pct_team2, 1, 0),
#          pregame_elo_tm_adj = ifelse(home_team == 1, pregame_elo_team1 + 100, pregame_elo_team1),
#          pregame_elo_opp_adj = ifelse(home_team == 0, pregame_elo_team2 + 100, pregame_elo_team2),
#          wp_expon = (pregame_elo_team2 - pregame_elo_team1) / 400,
#          wp = 1 / (1 + 10 ** wp_expon))








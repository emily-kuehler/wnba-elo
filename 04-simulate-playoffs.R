source("00-load-params.R")
source("helper-functions.R")

#need to make sure we are getting distribution of results

my_con <- connect_to_aws_db()


# simulate season ---------------------------------------------------------

simulate_playoff_season <- function(curr_season) {
  
  #league standings
  standings <- DBI::dbGetQuery(my_con, "SELECT * FROM wnba_league_standings") %>% 
    filter(season == curr_season) %>% 
    mutate(team = str_remove(team, "[*]"),
           team = ifelse(str_detect(team, "San Antonio"), "San Antonio", team))
  
  # standings <- DBI::dbGetQuery(my_con, "SELECT * FROM wnba_league_standings") %>% 
  #   filter(season == curr_season) %>% 
  #   mutate(team = str_remove(team, "[*]"))
  
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
           loser = str_trim(loser),
           winner = ifelse(str_detect(winner, "San Antonio"), "San Antonio", winner),
           loser = ifelse(str_detect(loser, "San Antonio"), "San Antonio", loser))
  # matchups <- DBI::dbGetQuery(my_con, "SELECT * FROM wnba_playoff_results") %>% 
  #   filter(season == curr_season) %>% 
  #   mutate(winner = str_trim(winner),
  #          loser = str_trim(loser))
  
  print("collected matchups")
  
  if (curr_season == 1997) {
    
    playoff_sim_results <- map(.x = rep(x = 4, times = N_SIMS), .f = simulate_four_team_playoff, matchups, standings, init_playoff_elo_vals, T) %>% 
      bind_rows() %>% 
      mutate(season = curr_season)
    
    print(paste0("finished season: ", curr_season))
    
    return (playoff_sim_results)
    
  } else if (curr_season == 1998) {
    
    playoff_sim_results <- map(.x = rep(x = 4, times = N_SIMS), .f = simulate_four_team_playoff, matchups, standings, init_playoff_elo_vals, F) %>%
      bind_rows() %>% 
      mutate(season = curr_season)
    
    print(paste0("finished season: ", curr_season))
    
    return (playoff_sim_results)
    
  } else if (curr_season == 1999) {
    
    playoff_sim_results <- map(.x = rep(x = 6, times = N_SIMS), .f = simulate_six_team_playoff, matchups, standings, init_playoff_elo_vals, T) %>% 
      bind_rows() %>% 
      mutate(season = curr_season)
    
    print(paste0("finished season: ", curr_season))
    
    return (playoff_sim_results)
    
  } else if (curr_season >= 2000 & curr_season <= 2004) {
    
    #these years have a best of three finals round
    playoff_sim_results <- map(.x = rep(x = 8, times = N_SIMS), .f = simulate_eight_team_playoff, matchups, standings, init_playoff_elo_vals, T) %>% 
      bind_rows() %>% 
      mutate(season = curr_season)
    
    print(paste0("finished season: ", curr_season))
    
    return (playoff_sim_results)
    
  } else if (curr_season > 2004 & curr_season <= 2015) {
    
    #these years have a best of five finals round
    playoff_sim_results <- map(.x = rep(x = 8, times = N_SIMS), .f = simulate_eight_team_playoff, matchups, standings, init_playoff_elo_vals, F) %>% 
      bind_rows() %>% 
      mutate(season = curr_season)
    
    print(paste0("finished season: ", curr_season))
    
    return (playoff_sim_results)
    
  } else if (curr_season >= 2016) {
    
    playoff_sim_results <- map(.x = rep(x = 8, times = N_SIMS), .f = simulate_four_round_playoff, matchups, standings, init_playoff_elo_vals) %>% 
      bind_rows() %>% 
      mutate(season = curr_season)
    
    print(paste0("finished season: ", curr_season))
    
    return (playoff_sim_results)
    
  }
  
}




# four team playoff -------------------------------------------------------


simulate_four_team_playoff <- function(n_teams = 4,
                                       matchup_df,
                                       standings_df,
                                       init_elos,
                                       single_elimination = T) {
  
  total_teams <- n_teams
  
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
  
  if (single_elimination == T) {
    
    semis_list <- list(wp_tm1 = init_matchups$wp_tm1,
                       team1 = init_matchups$team1,
                       team2 = init_matchups$team2)
    semifinal_winners <- pmap(.l = semis_list, .f = get_single_elimination_results) %>%
      unlist()
    
  } else {
    
    semis_list <- list(wp_tm1 = init_matchups$wp_tm1,
                       team1 = init_matchups$team1,
                       team2 = init_matchups$team2)
    semifinal_winners <- pmap(.l = semis_list, .f = get_best_of_three_results) %>%
      unlist()
    
  }
  
  finals_matchups <- init_elos %>%
    filter(team %in% semifinal_winners) %>%
    select(team, pregame_elo_tm, wp) %>%
    inner_join(standings_df, by = c("team")) %>%
    select(team,
           pregame_elo_tm,
           reg_season_wins = W,
           reg_season_losses = L,
           win_pct = `W/L%`)
  
  finals_df_cols <- c("team1", "pregame_elo_tm1", "reg_season_wins_tm1", "reg_season_losses_tm1", "win_pct_tm1",
                      "team2", "pregame_elo_tm2", "reg_season_wins_tm2", "reg_season_losses_tm2", "win_pct_tm2")
  finals_wide <- data.frame(finals_matchups[1,], finals_matchups[2,])
  names(finals_wide) <- finals_df_cols
  
  #calculate new win probability
  finals_df <- finals_wide %>%
    mutate(home_team = if_else(win_pct_tm1 >= win_pct_tm2, 1, 0),
           pregame_elo_tm_adj = ifelse(home_team == 1, pregame_elo_tm1 + 100, pregame_elo_tm1),
           pregame_elo_opp_adj = ifelse(home_team == 0, pregame_elo_tm2 + 100, pregame_elo_tm2),
           wp_expon = (pregame_elo_tm2 - pregame_elo_tm1) / 400,
           wp = 1 / (1 + 10 ** wp_expon)) %>%
    select(team1, team2, pregame_elo_tm1, pregame_elo_tm2, wp_tm1 = wp) %>%
    mutate(wp_tm2 = 1 - wp_tm1)
  
  if (single_elimination == T) {
    
    finals_winner <- get_single_elimination_results(finals_df$wp_tm1, finals_df$team1, finals_df$team2)
    
  } else {
    
    finals_winner <- get_best_of_three_results(finals_df$wp_tm1, finals_df$team1, finals_df$team2)
    
  }
  
  semifinal_winner_df <- data.frame(round = "Semifinals",
                                    winner = semifinal_winners)
  
  finals_winner_df <- data.frame(round = "Finals",
                                 winner = finals_winner)
  
  playoff_results <- semifinal_winner_df %>%
    bind_rows(finals_winner_df)
  
  return (playoff_results)
  
}


# six team playoff --------------------------------------------------------

simulate_six_team_playoff <- function(n_teams = 6,
                                      matchup_df,
                                      standings_df,
                                      init_elos,
                                      single_elimination = T) {
  
  
  total_teams <- n_teams
  
  #first round matchups
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
  play_in_list <- list(wp_tm1 = init_matchups$wp_tm1,
                       team1 = init_matchups$team1,
                       team2 = init_matchups$team2)
  play_in_winners <- pmap(.l = play_in_list, .f = get_single_elimination_results) %>%
    unlist()
  
  #create dataframe for conference finals and simulate best of three
  top_seeds <- standings_df %>% 
    group_by(conference) %>% 
    slice(1) %>% 
    ungroup()
  
  play_in_winner_df <- init_elos %>% 
    filter(team %in% play_in_winners) %>% 
    inner_join(standings_df, by = c("team")) %>% 
    select(team1 = team,
           pregame_elo_tm1 = pregame_elo_tm,
           reg_season_wins_tm1 = W,
           reg_season_losses_tm1 = L,
           win_pct_tm1 = `W/L%`,
           conference)
  
  top_seed_df <- init_elos %>% 
    filter(team %in% top_seeds$team) %>% 
    inner_join(standings_df, by = c("team")) %>% 
    select(team2 = team,
           pregame_elo_tm2 = pregame_elo_tm,
           reg_season_wins_tm2 = W,
           reg_season_losses_tm2 = L,
           win_pct_tm2 = `W/L%`,
           conference)
  
  #get matchups and calculate new win probabilities
  conf_finals_matchups <- play_in_winner_df %>% 
    inner_join(top_seed_df, by = c("conference")) %>% 
    mutate(home_team = if_else(win_pct_tm1 >= win_pct_tm2, 1, 0),
           pregame_elo_tm_adj = ifelse(home_team == 1, pregame_elo_tm1 + 100, pregame_elo_tm1),
           pregame_elo_opp_adj = ifelse(home_team == 0, pregame_elo_tm2 + 100, pregame_elo_tm2),
           wp_expon = (pregame_elo_tm2 - pregame_elo_tm1) / 400,
           wp = 1 / (1 + 10 ** wp_expon)) %>%
    select(team1, team2, pregame_elo_tm1, pregame_elo_tm2, wp_tm1 = wp) %>%
    mutate(wp_tm2 = 1 - wp_tm1)
  
  #get results from conference finals
  conf_finals_list <- list(wp_tm1 = conf_finals_matchups$wp_tm1,
                           team1 = conf_finals_matchups$team1,
                           team2 = conf_finals_matchups$team2)
  conf_finals_winners <- pmap(.l = conf_finals_list, .f = get_best_of_three_results) %>%
    unlist()
  
  #get finals winner
  finals_matchups <- init_elos %>%
    filter(team %in% conf_finals_winners) %>%
    select(team, pregame_elo_tm, wp) %>%
    inner_join(standings_df, by = c("team")) %>%
    select(team,
           pregame_elo_tm,
           reg_season_wins = W,
           reg_season_losses = L,
           win_pct = `W/L%`)
  
  finals_df_cols <- c("team1", "pregame_elo_tm1", "reg_season_wins_tm1", "reg_season_losses_tm1", "win_pct_tm1",
                      "team2", "pregame_elo_tm2", "reg_season_wins_tm2", "reg_season_losses_tm2", "win_pct_tm2")
  finals_wide <- data.frame(finals_matchups[1,], finals_matchups[2,])
  names(finals_wide) <- finals_df_cols
  
  finals_df <- finals_wide %>%
    mutate(home_team = if_else(win_pct_tm1 >= win_pct_tm2, 1, 0),
           pregame_elo_tm_adj = ifelse(home_team == 1, pregame_elo_tm1 + 100, pregame_elo_tm1),
           pregame_elo_opp_adj = ifelse(home_team == 0, pregame_elo_tm2 + 100, pregame_elo_tm2),
           wp_expon = (pregame_elo_tm2 - pregame_elo_tm1) / 400,
           wp = 1 / (1 + 10 ** wp_expon)) %>%
    select(team1, team2, pregame_elo_tm1, pregame_elo_tm2, wp_tm1 = wp) %>%
    mutate(wp_tm2 = 1 - wp_tm1)
  
  finals_winner <- get_best_of_three_results(finals_df$wp_tm1, finals_df$team1, finals_df$team2)
  
  play_in_winner_results <- data.frame(round = "Play-In",
                                       winner = play_in_winners)
  
  conf_finals_results <- data.frame(round = "Conference Finals",
                                    winner = conf_finals_winners)
  
  finals_results <- data.frame(round = "Finals",
                               winner = finals_winner)
  
  playoff_results <- play_in_winner_results %>%
    bind_rows(conf_finals_results) %>% 
    bind_rows(finals_results)
  
  return (playoff_results)
  
}



# eight team playoff ------------------------------------------------------

simulate_eight_team_playoff <- function(n_teams = 8,
                                        matchup_df,
                                        standings_df,
                                        init_elos,
                                        best_of_three_finals = T) {
  
  #first round matchups
  matchups <- matchup_df %>% 
    filter(str_detect(round, "Semifinals"))
  
  #join matchups to init values
  semis_matchups <- matchups %>% 
    inner_join(init_elos, by = c("winner" = "team")) %>% 
    rename(team1 = winner,
           team2 = loser,
           pregame_elo_tm1 = pregame_elo_tm,
           pregame_elo_tm2 = pregame_elo_opp,
           wp_tm1 = wp) %>% 
    select(-opponent) %>% 
    mutate(wp_tm2 = 1 - wp_tm1)
  
  #get results from semi final rounds
  semis_list <- list(wp_tm1 = semis_matchups$wp_tm1,
                     team1 = semis_matchups$team1,
                     team2 =  semis_matchups$team2)
  semis_winners <- pmap(.l = semis_list, .f = get_best_of_three_results) %>%
    unlist()
  
  #two teams from each conference will play each other
  semi_winners_df <- standings_df %>%
    filter(team %in% semis_winners) %>% 
    inner_join(init_elos, by = c("team")) %>% 
    select(team, 
           pregame_elo_tm,
           reg_season_wins = W,
           reg_season_losses = L,
           win_pct = `W/L%`, 
           conference) %>% 
    group_by(conference) %>% 
    arrange(desc(win_pct)) %>%
    ungroup() %>% 
    select(-conference)
  
  semi_finals_df_cols <- c("team1", "pregame_elo_tm1", "reg_season_wins_tm1", "reg_season_losses_tm1", "win_pct_tm1",
                           "team2", "pregame_elo_tm2", "reg_season_wins_tm2", "reg_season_losses_tm2", "win_pct_tm2")
  
  conf1_winners <- semi_winners_df[1,] %>% 
    bind_cols(semi_winners_df[2,])
  
  conf2_winners <- semi_winners_df[3,] %>% 
    bind_cols(semi_winners_df[4,])
  
  names(conf1_winners) <- semi_finals_df_cols
  names(conf2_winners) <- semi_finals_df_cols
  
  conf_finals_matchups <- conf1_winners %>% 
    bind_rows(conf2_winners) %>% 
    mutate(home_team = if_else(win_pct_tm1 >= win_pct_tm2, 1, 0),
           pregame_elo_tm_adj = ifelse(home_team == 1, pregame_elo_tm1 + 100, pregame_elo_tm1),
           pregame_elo_opp_adj = ifelse(home_team == 0, pregame_elo_tm2 + 100, pregame_elo_tm2),
           wp_expon = (pregame_elo_tm2 - pregame_elo_tm1) / 400,
           wp = 1 / (1 + 10 ** wp_expon)) %>%
    select(team1, team2, pregame_elo_tm1, pregame_elo_tm2, wp_tm1 = wp) %>%
    mutate(wp_tm2 = 1 - wp_tm1)
  
  
  conf_finals_list <- list(wp_tm1 = conf_finals_matchups$wp_tm1,
                           team1 = conf_finals_matchups$team1,
                           team2 = conf_finals_matchups$team2)
  conf_finals_winners <- pmap(.l = conf_finals_list, .f = get_best_of_three_results) %>%
    unlist()
  
  #get overall winner
  finals_matchups <- init_elos %>%
    filter(team %in% conf_finals_winners) %>%
    select(team, pregame_elo_tm, wp) %>%
    inner_join(standings_df, by = c("team")) %>%
    select(team,
           pregame_elo_tm,
           reg_season_wins = W,
           reg_season_losses = L,
           win_pct = `W/L%`)
  
  finals_df_cols <- c("team1", "pregame_elo_tm1", "reg_season_wins_tm1", "reg_season_losses_tm1", "win_pct_tm1",
                      "team2", "pregame_elo_tm2", "reg_season_wins_tm2", "reg_season_losses_tm2", "win_pct_tm2")
  finals_wide <- data.frame(finals_matchups[1,], finals_matchups[2,])
  names(finals_wide) <- finals_df_cols
  
  finals_df <- finals_wide %>%
    mutate(home_team = if_else(win_pct_tm1 >= win_pct_tm2, 1, 0),
           pregame_elo_tm_adj = ifelse(home_team == 1, pregame_elo_tm1 + 100, pregame_elo_tm1),
           pregame_elo_opp_adj = ifelse(home_team == 0, pregame_elo_tm2 + 100, pregame_elo_tm2),
           wp_expon = (pregame_elo_tm2 - pregame_elo_tm1) / 400,
           wp = 1 / (1 + 10 ** wp_expon)) %>%
    select(team1, team2, pregame_elo_tm1, pregame_elo_tm2, wp_tm1 = wp) %>%
    mutate(wp_tm2 = 1 - wp_tm1)
  
  
  if (best_of_three_finals == T) {
    
    finals_winner <- get_best_of_three_results(finals_df$wp_tm1, finals_df$team1, finals_df$team2)
    
  } else {
    
    finals_winner <- get_best_of_five_results(finals_df$wp_tm1, finals_df$team1, finals_df$team2)
    
  }
  
  conference_semis_results <- data.frame(round = "Conference Semifinals",
                                         winner = semis_winners)
  
  conf_finals_results <- data.frame(round = "Conference Finals",
                                    winner = conf_finals_winners)
  
  finals_results <- data.frame(round = "Finals",
                               winner = finals_winner)
  
  playoff_results <- conference_semis_results %>% 
    bind_rows(conf_finals_results) %>% 
    bind_rows(finals_results)
  
  return (playoff_results)
  
}


simulate_four_round_playoff <- function(n_teams = 8,
                                        matchup_df,
                                        standings_df,
                                        init_elos) {
  
  #first round matchups
  matchups <- matchup_df %>% 
    filter(str_detect(round, "First Round"))
  
  #join matchups to init values
  first_round_matchups <- matchups %>% 
    inner_join(init_elos, by = c("winner" = "team")) %>% 
    rename(team1 = winner,
           team2 = loser,
           pregame_elo_tm1 = pregame_elo_tm,
           pregame_elo_tm2 = pregame_elo_opp,
           wp_tm1 = wp) %>% 
    select(-opponent) %>% 
    mutate(wp_tm2 = 1 - wp_tm1)
  
  first_round_list <- list(wp_tm1 = first_round_matchups$wp_tm1,
                           team1 = first_round_matchups$team1,
                           team2 =  first_round_matchups$team2)
  first_round_winners <- pmap(.l = first_round_list, .f = get_single_elimination_results) %>%
    unlist()
  
  #reseed and get second round matchups
  #gets 3rd and 4th seeds
  third_fourth_seeds <- standings_df %>% 
    rename(win_pct = `W/L%`) %>% 
    arrange(desc(win_pct)) %>% 
    slice(3:4) %>%
    inner_join(init_elos, by = c("team")) %>% 
    select(team1 = team,
           pregame_elo_tm1 = pregame_elo_tm,
           reg_season_wins_tm1 = W,
           reg_season_losses = L,
           win_pct_tm1 = win_pct)
  
  
  #first round winners
  round1_lower_seeds <- standings_df %>%
    filter(team %in% first_round_winners) %>% 
    rename(win_pct = `W/L%`) %>% 
    arrange(win_pct) %>%
    inner_join(init_elos, by = c("team")) %>% 
    select(team2 = team,
           pregame_elo_tm2 = pregame_elo_tm,
           reg_season_wins_tm2 = W,
           reg_season_losses_tm2 = L,
           win_pct_tm2 = win_pct)
  
  second_round_matchups <- third_fourth_seeds %>% 
    bind_cols(round1_lower_seeds) %>% 
    mutate(home_team = if_else(win_pct_tm1 >= win_pct_tm2, 1, 0),
           pregame_elo_tm_adj = ifelse(home_team == 1, pregame_elo_tm1 + 100, pregame_elo_tm1),
           pregame_elo_opp_adj = ifelse(home_team == 0, pregame_elo_tm2 + 100, pregame_elo_tm2),
           wp_expon = (pregame_elo_tm2 - pregame_elo_tm1) / 400,
           wp = 1 / (1 + 10 ** wp_expon)) %>%
    select(team1, team2, pregame_elo_tm1, pregame_elo_tm2, wp_tm1 = wp) %>%
    mutate(wp_tm2 = 1 - wp_tm1)
  
  second_round_list <- list(wp_tm1 = second_round_matchups$wp_tm1,
                            team1 = second_round_matchups$team1,
                            team2 =  second_round_matchups$team2)
  second_round_winners <- pmap(.l = second_round_list, .f = get_single_elimination_results) %>%
    unlist()
  
  
  
  #reseed for semifinals
  top_two_seeds <- standings_df %>% 
    rename(win_pct = `W/L%`) %>% 
    arrange(desc(win_pct)) %>% 
    slice(1:2) %>%
    inner_join(init_elos, by = c("team")) %>% 
    select(team1 = team,
           pregame_elo_tm1 = pregame_elo_tm,
           reg_season_wins_tm1 = W,
           reg_season_losses = L,
           win_pct_tm1 = win_pct)
  
  round2_lower_seeds <- standings_df %>%
    filter(team %in% second_round_winners) %>% 
    rename(win_pct = `W/L%`) %>% 
    arrange(win_pct) %>%
    inner_join(init_elos, by = c("team")) %>% 
    select(team2 = team,
           pregame_elo_tm2 = pregame_elo_tm,
           reg_season_wins_tm2 = W,
           reg_season_losses_tm2 = L,
           win_pct_tm2 = win_pct)
  
  semifinal_matchups <- top_two_seeds %>% 
    bind_cols(round2_lower_seeds) %>% 
    mutate(home_team = if_else(win_pct_tm1 >= win_pct_tm2, 1, 0),
           pregame_elo_tm_adj = ifelse(home_team == 1, pregame_elo_tm1 + 100, pregame_elo_tm1),
           pregame_elo_opp_adj = ifelse(home_team == 0, pregame_elo_tm2 + 100, pregame_elo_tm2),
           wp_expon = (pregame_elo_tm2 - pregame_elo_tm1) / 400,
           wp = 1 / (1 + 10 ** wp_expon)) %>%
    select(team1, team2, pregame_elo_tm1, pregame_elo_tm2, wp_tm1 = wp) %>%
    mutate(wp_tm2 = 1 - wp_tm1)
  
  semifinals_list <- list(wp_tm1 = semifinal_matchups$wp_tm1,
                          team1 = semifinal_matchups$team1,
                          team2 = semifinal_matchups$team2)
  semifinals_winners <- pmap(.l = semifinals_list, .f = get_best_of_five_results) %>%
    unlist()
  
  #get finals results
  finals_matchups <- init_elos %>%
    filter(team %in% semifinals_winners) %>%
    select(team, pregame_elo_tm, wp) %>%
    inner_join(standings_df, by = c("team")) %>%
    select(team,
           pregame_elo_tm,
           reg_season_wins = W,
           reg_season_losses = L,
           win_pct = `W/L%`)
  
  finals_df_cols <- c("team1", "pregame_elo_tm1", "reg_season_wins_tm1", "reg_season_losses_tm1", "win_pct_tm1",
                      "team2", "pregame_elo_tm2", "reg_season_wins_tm2", "reg_season_losses_tm2", "win_pct_tm2")
  finals_wide <- data.frame(finals_matchups[1,], finals_matchups[2,])
  names(finals_wide) <- finals_df_cols
  
  finals_df <- finals_wide %>%
    mutate(home_team = if_else(win_pct_tm1 >= win_pct_tm2, 1, 0),
           pregame_elo_tm_adj = ifelse(home_team == 1, pregame_elo_tm1 + 100, pregame_elo_tm1),
           pregame_elo_opp_adj = ifelse(home_team == 0, pregame_elo_tm2 + 100, pregame_elo_tm2),
           wp_expon = (pregame_elo_tm2 - pregame_elo_tm1) / 400,
           wp = 1 / (1 + 10 ** wp_expon)) %>%
    select(team1, team2, pregame_elo_tm1, pregame_elo_tm2, wp_tm1 = wp) %>%
    mutate(wp_tm2 = 1 - wp_tm1)
  
  finals_winner <- get_best_of_five_results(finals_df$wp_tm1, finals_df$team1, finals_df$team2)
  
  first_round_df <- data.frame(round = "First", 
                               winner = first_round_winners)
  
  second_round_df <- data.frame(round = "Second",
                                winner = second_round_winners)
  
  semifinal_df <- data.frame(round = "Semifinals",
                             winner = semifinals_winners)
  
  final_result_df <- data.frame(round = "Finals",
                                winner = finals_winner)
  
  results_df <- first_round_df %>% 
    bind_rows(second_round_df) %>% 
    bind_rows(semifinal_df) %>% 
    bind_rows(final_result_df)
  
  return(results_df)
  
}





# helpers -----------------------------------------------------------------



get_single_elimination_results <- function(wp_tm1, team1, team2) {
  
  result <- sample(x = c(team1,team2),
                   size = 1,
                   replace = T,
                   prob = c(wp_tm1,
                            1 - wp_tm1))
  
  return (result)
  
}

get_best_of_three_results <- function(wp_tm1, team1, team2) {
  
  result <- sample(x = c(team1,team2),
                   size = 3,
                   replace = T,
                   prob = c(wp_tm1,
                            1 - wp_tm1)) %>% 
    as_tibble() %>% 
    count(value) %>% 
    rename(team = value,
           wins = n) %>% 
    filter(wins >= 2) %>% 
    pull(team)
  
  return (result)
  
}

get_best_of_five_results <- function(wp_tm1, team1, team2) {
  
  result <- sample(x = c(team1,team2),
                   size = 5,
                   replace = T,
                   prob = c(wp_tm1,
                            1 - wp_tm1)) %>% 
    as_tibble() %>% 
    count(value) %>% 
    rename(team = value,
           wins = n) %>% 
    filter(wins >= 3) %>% 
    pull(team)
  
  return (result)
  
}


set.seed(94110)
full_simulation_results <- map(.x = INIT_SEASON:FINAL_SEASON, .f = simulate_playoff_season) %>%
  bind_rows()
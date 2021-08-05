source("helper-functions.R")

#need to make sure we are getting distribution of results

my_con <- connect_to_aws_db()


# simulate season ---------------------------------------------------------

simulate_playoff_season <- function(curr_season) {
  
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
  
  if (curr_season == 1997) {
    
    playoff_sim_results <- map(.x = rep(x = 4, times = N_SIMS), .f = simulate_four_team_playoff, matchups, standings, init_playoff_elo_vals, T) %>% 
      bind_rows()
    
    return (playoff_sim_results)
    
  } else if (curr_season == 1998) {
    
    playoff_sim_results <- map(.x = rep(x = 4, times = N_SIMS), .f = simulate_four_team_playoff, matchups, standings, init_playoff_elo_vals, F) %>% 
      bind_rows()
    
    return (playoff_sim_results)
    
  }
  
}



# helpers -----------------------------------------------------------------


simulate_four_team_playoff <- function(n_teams = 4,
                                       matchup_df,
                                       standings_df,
                                       init_elos,
                                       single_elimination = T) {
  
  n_teams <- 4
  
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
      inner_join(standings_df) %>%
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


test_sim <- simulate_playoff_season(curr_season = 1998)















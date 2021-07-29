source("helper-functions.R")


my_con <- connect_to_aws_db()

# simulate completed seasons ----------------------------------------------

simulate_playoffs <- function(curr_season, sims = 1e4) {
  
  #get initial playoff values
  init_playoff_elo_vals <- DBI::dbGetQuery(my_con, "SELECT * FROM wnba_elo_vals") %>% 
    filter(season == 1997 & season_type == 'post') %>% 
    group_by(team) %>% 
    filter(game_date == min(game_date)) %>% 
    select(team, opponent, pregame_elo_tm, pregame_elo_opp, wp)
  
  #initial playoff matchups
  matchups <- DBI::dbGetQuery(my_con, "SELECT * FROM wnba_playoff_results") %>% 
    filter(season == 1997 & round == "Semifinals") %>% 
    mutate(winner = str_trim(winner),
           loser = str_trim(loser))
  
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
  round1_results <- map(.x = init_matchups$wp_tm1, .f = simulate_elimination_round, n_sims = sims) %>% 
    unlist()
  
  results <- init_matchups %>% 
    mutate(team_1_wins = round1_results,
           team_2_wins = sims - team_1_wins,
           series_winner = if_else(team_1_wins > team_2_wins, team1, team2),
           pregame_elo_winner = if_else(team_1_wins > team_2_wins, pregame_elo_tm1, pregame_elo_tm2),
           pregame_elo_loser = if_else(team_1_wins > team_2_wins, pregame_elo_tm2, pregame_elo_tm1)) %>% 
    select(team1, team2, team_1_wins, team_2_wins, series_winner, pregame_elo_winner, pregame_elo_loser)
  
  #simulate finals
  
  return (results)
  
}

simulate_elimination_round <- function(win_prob_tm1, n_sims) {
  
  results <- sample(x = c("wins_tm_1", "wins_tm_2"),
                    size = n_sims,
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

test_sim <- simulate_playoffs(curr_season = 1997, sims = 1e3)










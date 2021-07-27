source("helper-functions.R")


# simulate completed seasons ----------------------------------------------

simulate_playoffs <- function(curr_season, sims = 1e4) {
  
  elo_vals <- DBI::dbGetQuery(my_con, "SELECT * FROM wnba_elo_vals") %>% 
    filter(season == 1997 & season_type == 'post') %>% 
    group_by(team) %>% 
    filter(game_date == min(game_date))
  
}

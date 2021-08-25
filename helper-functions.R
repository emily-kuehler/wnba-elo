
# database functions ------------------------------------------------------
library(tidyverse)
database_df <- read_csv("secrets.txt")

connect_to_aws_db <- function(pw = database_df$pw) {
  
  dbhost <- database_df$host
  
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = dbhost,
    port = 5432,
    dbname = "sportsDB",
    user = "emily",
    password = pw
  )
  
  return (con)
  
}


# simulation helpers ------------------------------------------------------


summarise_sim_results <- function(curr_season, sim_results_df) {
  
  curr_season_results <- sim_results_df %>% 
    filter(season == curr_season)
  
  unique_rounds <- curr_season_results %>% 
    distinct(round) %>% 
    pull()
  
  curr_season_summary <- curr_season_results %>% 
    group_by(winner, round) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(prop = n / N_SIMS) %>% 
    select(team = winner,
           round,
           prop)
  
  if (curr_season >= 1997 &  curr_season <= 1998) {
    
    curr_season_summary <- curr_season_summary %>% 
      mutate(round = factor(round, levels = c("Semifinals", "Finals"))) %>% 
      arrange(round, desc(prop))
    
  } else if (curr_season == 1999) {
    
    curr_season_summary <- curr_season_summary %>% 
      mutate(round = factor(round, levels = c("Play-In", "Conference Finals", "Finals"))) %>% 
      arrange(round, desc(prop))
    
  } else if (curr_season >= 2000 & curr_season <= 2015) {
    
    curr_season_summary <- curr_season_summary %>% 
      mutate(round = factor(round, levels = c("Conference Semifinals", "Conference Finals", "Finals"))) %>% 
      arrange(round, desc(prop))
    
  } else if (curr_season >= 2016) {
    
    curr_season_summary <- curr_season_summary %>% 
      mutate(round = factor(round, levels = c("First", "Second", "Semifinals", "Finals")),
             prop = ifelse(round != "Finals", prop / 2, prop)) %>% 
      arrange(round, desc(prop))
    
  }
  
  curr_season_summary <- curr_season_summary %>% 
    mutate(pct = scales::percent(prop),
           season = curr_season)
  
  return (curr_season_summary)
  
}
source("helper-functions.R")

my_con <- connect_to_aws_db()

sim_results <- DBI::dbGetQuery(my_con, "SELECT * FROM simulation_results")

summary_sim_results <- map(.x = INIT_SEASON:FINAL_SEASON, .f = summarise_sim_results, sim_results) %>% 
  bind_rows()

#DBI::dbWriteTable(my_con, "sim_results_summary", summary_sim_results)

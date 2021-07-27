source("helper-functions.R")

# database connections ----------------------------------------------------

my_con <- connect_to_aws_db()

DBI::dbWriteTable(my_con, "wnba_game_logs", clean_game_logs_df)

DBI::dbWriteTable(my_con, "wnba_elo_vals", elo_val_list)

DBI::dbWriteTable(my_con, "wnba_league_standings", league_standings)

DBI::dbWriteTable(my_con, "wnba_playoff_results", playoff_results)



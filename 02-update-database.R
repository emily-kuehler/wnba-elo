source("01-scrape-game-logs.R")

# database connections ----------------------------------------------------

connect_to_aws_db <- function(pw = 'miggy479') {
  
  dbhost <- 'db-sports.csmeuaw95l7j.us-west-2.rds.amazonaws.com'
  
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

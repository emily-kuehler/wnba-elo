
# database functions ------------------------------------------------------

database_pw <- read_csv("secrets.txt")

connect_to_aws_db <- function(pw = database_pw$pw) {
  
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


# simulation helpers ------------------------------------------------------



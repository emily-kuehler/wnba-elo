
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



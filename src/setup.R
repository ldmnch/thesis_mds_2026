library(DBI)
library(RPostgres)

load_dot_env(".env")

get_postgres_connection <- function(host = "localhost"){
  
  ECOSYSTEMS_DB_USER <- Sys.getenv("ECOSYSTEMS_DB_USER")
  ECOSYSTEMS_DB_PASSWORD <- Sys.getenv("POSTGRESQL_ECOSYSTEMS_DB")
  ECOSYSTEMS_DB_NAME <- Sys.getenv("ECOSYSTEMS_DB_NAME")

  
  # now start creating connection
  dbConnect(Postgres(),
                   dbname = ECOSYSTEMS_DB_NAME,
                   host = "localhost",
                   port = 5432,
                   user = ECOSYSTEMS_DB_USER,
                   password = ECOSYSTEMS_DB_PASSWORD)
  
  
}

# now start creating connection
con <- get_postgres_connection()

# Datetime formatting
## set system locale in spanish
Sys.setlocale("LC_TIME", "es_ES.UTF-8")


#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("dependencies.R")
source("preperation.R")

#add sourcing of rest controller here

is_local <- file.exists("local_dev/.env_local") || dir.exists("local_dev")

PORT<-Sys.getenv("PORT")

current_url <- if (is_local) {
  "http://127.0.0.1:8080"
} else {
  Sys.getenv("EXTERNAL_URL", "https://127.0.0.1:8080")
}

# --- VERBINDUNGEN AUFBAUEN ---

message("Verbinde mit MongoDB ...")

if(is_local){
  
  load_dot_env(file = "local_dev/.env_local")
  
}else{
  PORT<-as.numeric(Sys.getenv('API_PORT'))
}

LOG_FILE_PATH <- Sys.getenv("LOG_FILE", "logs/api.log") # Read log path from entrypoint script

MONGO_USER <- Sys.getenv("MONGO_USER")
MONGO_PASSWORD <- Sys.getenv("MONGO_PASSWORD")
MONGO_IP <- Sys.getenv("MONGO_IP")
MONGO_PORT <- Sys.getenv("MONGO_PORT")


url <- sprintf("mongodb://%s:%s@%s:%s", MONGO_USER, MONGO_PASSWORD, MONGO_IP, MONGO_PORT)


pg_host <- Sys.getenv("PG_HOST")
pg_port <- Sys.getenv("PG_PORT")
pg_pass <- Sys.getenv("PG_PASS")
pg_user <- Sys.getenv("PG_USER")
pg_db <- Sys.getenv("PG_DB")


message("Verbinde mit PostgreSQL...")

# PostgreSQL Verbindung
p_con <- dbConnect(
  RPostgres::Postgres(),
  host = pg_host,
  port = pg_port,
  user = pg_user,
  password = pg_pass,
  dbname = pg_db
)



initial_params<-get_notfall_times_current(return_internal=T)
initial_plot_params_notfall<-initial_params[[1]]
initial_point_params_notfall<-initial_params[[2]]


options(plumber.apiURL=current_url)



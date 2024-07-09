# Load the RPostgres package
library(RPostgres)
library(dplyr)
# Set up the database connection
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "pelagic",
                 host = "sp2.cs.vt.edu",
                 port = 5432,
                 user = "spr",
                 password = "spr_pass")

# Fetch all data from 'inat_effort' table
inat_effort <- dbReadTable(con, "inat_effort") # effort
# Fetch all valid scientific names from 'taxonomy3' where 'superorder' equals 'Selachimorpha'
valid_names_query <- "SELECT valid FROM taxonomy3 WHERE superorder = 'Selachimorpha'"
valid_names <- dbGetQuery(con, valid_names_query)
# Prepare the SQL query to select all rows from 'inat' where 'scientific_name' exists in the above valid names
query <- sprintf("SELECT * FROM inat WHERE scientific_name IN (%s)",
                 paste0("'", valid_names$valid, "'", collapse = ", "))
# Fetch the data based on the condition
inat_data <- dbGetQuery(con, query)
# Close the database connection
dbDisconnect(con)
# Filter out rows without latitude, longitude, or 'time_observed_at' from 'inat_effort'
inat_effort <- inat_effort %>% 
  filter(!is.na(latitude) & !is.na(longitude) & !is.na(time_observed_at))
# Filter out rows without latitude, longitude, or 'time_observed_at' from 'inat_data'
inat_data <- inat_data %>%
  filter(!is.na(latitude) & !is.na(longitude) & !is.na(time_observed_at))

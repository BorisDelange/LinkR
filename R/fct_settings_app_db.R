#' @noRd
db_create_table <- function(db, table, dbms, empty_tibble, primary_key_col, text_cols, unique_cols = character(), not_null_cols = character()){
  
  if (table %not_in% DBI::dbListTables(db)){
    
    sql <- ""
    
    for (i in 1:ncol(empty_tibble)){
      col_class <- empty_tibble[, i] %>% dplyr::pull() %>% class()
      col_name <- colnames(empty_tibble[, i])
      
      if (dbms == "postgres"){
        if (col_name %in% text_cols) col_type <- "TEXT"
        else col_type <- switch(col_class, "integer" = "INT", "character" = "VARCHAR(255)", "logical" = "BOOLEAN", "numeric" = "REAL")
      }
      else if (dbms == "sqlite"){
        col_type <- switch(col_class, "integer" = "INTEGER", "character" = "TEXT", "logical" = "INTEGER", "numeric" = "DOUBLE PRECISION")
      }
      
      if (col_name %in% primary_key_col) primary_key_constraint <- " PRIMARY KEY" else primary_key_constraint <-  ""
      if (col_name %in% unique_cols) unique_constraint <- " UNIQUE" else unique_constraint <- ""
      if (col_name %in% not_null_cols) not_null_constraint <- " NOT NULL" else not_null_constraint <- ""
      
      if (i == 1) sql <- paste0(sql, col_name, " ", col_type, primary_key_constraint, unique_constraint, not_null_constraint)
      else sql <- paste0(sql, ", ", col_name, " ", col_type, primary_key_constraint, unique_constraint, not_null_constraint)
    }
    
    sql <- glue::glue_sql("CREATE TABLE {`table`} (", sql, ")", .con = db)
    query <- DBI::dbSendStatement(db, sql)
    DBI::dbClearResult(query)
  }
}

#' @noRd
db_create_tables <- function(db, type, dbms, db_col_types){
  
  # Create tables if does not exist
  
  # Type = main for main database
  # Type = public for plugins / tabs database
  
  for (type in c("main", "public")){
    
    tables <- db_col_types %>% dplyr::filter(db == type) %>% dplyr::pull(table)
    
    for (table in tables){
      
      row <- db_col_types %>% dplyr::filter(db == type, table == !!table)
      
      types <- strsplit(row$col_types, "")[[1]]
      cols <- lapply(seq_along(types), function(i) {
        switch(types[[i]],
               "i" = integer(),
               "c" = character(),
               "n" = numeric(),
               "l" = logical())
      })
      names(cols) <- row$col_names %>% unlist()
      empty_tibble <- tibble::as_tibble(cols)
      
      primary_key_col <- "id"
      
      text_cols <- character()
      if (table %in% c("datasets", "studies", "plugins", "tabs_groups", "tabs", "subsets")) text_cols = "description"
      else if (table == "code") text_cols <- "code"
      else if (table %in% c("options", "persons_options", "widgets_options")) text_cols <- "value"
      else if (table == "messages") text_cols <- c("message", "filepath")
      else if (table %in% c("conversations", "user_deleted_conversations")) text_cols <- "name"
      else if (table == "log") text_cols <- "value"
      else if (table == "git_repos") text_cols <- c("description", "link")
      else if (table == "concept_user") text_cols <- c("name", "display_name")
      
      db_create_table(db = db, table = table, dbms = dbms, empty_tibble = empty_tibble, primary_key_col = primary_key_col, text_cols = text_cols)
    }
  }
}

#' @noRd
get_db <- function(r, m, app_db_folder, db_col_types){
  
  # Get local database connection
  
  r$db_connection <- "local"
  
  db <- list()
  db$local_main <- DBI::dbConnect(RSQLite::SQLite(), paste0(app_db_folder, "/linkr_main"))
  r$db <- db$local_main
  db$local_public <- DBI::dbConnect(RSQLite::SQLite(), paste0(app_db_folder, "/linkr_public"))
  m$db <- db$local_public
  
  # Create tables for local databases
  
  db_create_tables(db = db$local_main, type = "main", dbms = "sqlite", db_col_types = db_col_types)
  db_create_tables(db = db$local_public, type = "public", dbms = "sqlite", db_col_types = db_col_types)
  
  # Add remote db rows if they do not already exist
  
  if (DBI::dbGetQuery(db$local_main, "SELECT COUNT(id) FROM options WHERE category = 'remote_db'") != 8){
    
    DBI::dbSendStatement(db$local_main, "DELETE FROM options WHERE category = 'remote_db'")
    
    last_row <- DBI::dbGetQuery(db$local_main, "SELECT COALESCE(MAX(id), 0) FROM options")
    
    sql <- paste0("INSERT INTO options(id, category, name, value, deleted) ",
      "SELECT ", last_row + 1, ", 'remote_db', 'connection_type', 'local', FALSE ",
      "UNION SELECT ", last_row + 2, ", 'remote_db', 'sql_lib', 'postgres', FALSE ",
      "UNION SELECT ", last_row + 3, ", 'remote_db', 'main_db_name', '', FALSE ",
      "UNION SELECT ", last_row + 4, ", 'remote_db', 'public_db_name', '', FALSE ",
      "UNION SELECT ", last_row + 5, ", 'remote_db', 'host', '', FALSE ",
      "UNION SELECT ", last_row + 6, ", 'remote_db', 'port', '', FALSE ",
      "UNION SELECT ", last_row + 7, ", 'remote_db', 'user', '', FALSE ",
      "UNION SELECT ", last_row + 8, ", 'remote_db', 'password', '', FALSE")
    query <- DBI::dbSendStatement(db$local_main, sql)
    DBI::dbClearResult(query)
  }
  
  DBI::dbGetQuery(db$local_main, "SELECT value FROM options WHERE category = 'remote_db' AND name = 'connection_type'") %>% dplyr::pull() -> choice_remote_db
  
  if (choice_remote_db == "remote"){
    
    # Get remote DB parameters
    
    db_info <- DBI::dbGetQuery(db$local_main, "SELECT * FROM options WHERE category = 'remote_db'") %>% tibble::as_tibble()
    db_info <- db_info %>% dplyr::pull(value, name) %>% as.list()
    
    # Try remote connection
    
    result <- "failure"
    
    tryCatch({
      
      if (db_info$main_db_name != "" & db_info$public_db_name != ""){
        
        # Postgres
        if (db_info$sql_lib == "postgres"){
          db$remote_main <- DBI::dbConnect(RPostgres::Postgres(), dbname = db_info$main_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
          db$remote_public <- DBI::dbConnect(RPostgres::Postgres(), dbname = db_info$public_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
        } 
        
        # SQLite
        if (db_info$sql_lib == "sqlite"){
          db$remote_main <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_info$main_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
          db$remote_public <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_info$public_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
        }
        
        r$db_connection <- "remote"
        
        r$db <- db$remote_main
        m$db <- db$remote_public
        
        db_create_tables(db = db$remote_main, type = "main", dbms = db_info$sql_lib, db_col_types = db_col_types)
        db_create_tables(db = db$remote_public, type = "public", dbms = db_info$sql_lib, db_col_types = db_col_types)
        
        result <- "success"
      }
      
    }, error = function(e) "", warning = function(w) "")
    
    # If didn't succeed to connect to remote DB, update database and set connection to local
    if (result != "success"){
      sql <- glue::glue_sql("UPDATE options SET value = 'local' WHERE category = 'remote_db' AND name = 'connection_type'", .con = db$local_main)
      query <- DBI::dbSendStatement(db$local_main, sql)
      DBI::dbClearResult(query)
    }
  }
  
  return(db$local_main)
}

#' Get last row
#'
#' @description Get the last row ID of a table
#' @param con DBI connection object to the database
#' @param table Name of the table (character)
#' @examples
#' \dontrun{
#' get_last_row(con = r$db, table = "plugins")
#' }
get_last_row <- function(con, table){
  glue::glue_sql("SELECT COALESCE(MAX(id), 0) FROM {`table`}", .con = con) -> sql
  DBI::dbGetQuery(con, sql) %>% dplyr::pull() %>% as.integer()
}

#' @noRd
get_remote_db <- function(r, m, output, i18n, ns){
  
  result <- "failure"
  
  db_info <- DBI::dbGetQuery(r$local_db, "SELECT * FROM options WHERE category = 'remote_db'") %>% tibble::as_tibble()
  db_info <- db_info %>% dplyr::pull(value, name) %>% as.list()
  db <- list()
  
  # Try the connection
  tryCatch({
    
    if (db_info$main_db_name != "" & db_info$public_db_name != ""){
      
      # Postgres
      if (db_info$sql_lib == "postgres"){
        db$main <- DBI::dbConnect(RPostgres::Postgres(), dbname = db_info$main_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
        db$plugins <- DBI::dbConnect(RPostgres::Postgres(), dbname = db_info$public_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
      } 
      
      # SQLite
      if (db_info$sql_lib == "sqlite"){
        db$main <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_info$main_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
        db$plugins <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_info$public_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
      }
      
      r$remote_db <- db$main
      m$remote_db <- db$plugins
      
      result <- "success"
    }
    
  }, error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_connect_remote_db", 
    error_name = "get_remote_db", category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
  
  result
}

#' @noRd
load_database <- function(r, m, i18n){
  
  # Database tables to load
  r_tables <- c("users", "users_accesses", "users_statuses", "datasets",
    "plugins", "scripts", "code", "options", "git_repos")
  # r_tables <- c("options", "code")
  
  m_tables <- c("vocabulary")
  
  # sapply(r_tables, function(table){
  #   r[[table]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
  #   r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
  # })
  
  sapply(m_tables, function(table){
    # Easier to load vocabulary in r var
    r[[table]] <- DBI::dbGetQuery(m$db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
    r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
  })
  
  # Add a tab_types variable, for settings/plugins dropdown
  r$tab_types <- tibble::tribble(~id, ~name, 1, i18n$t("patient_level_data"), 2, i18n$t("aggregated_data"))
}

sql_send_statement <- function(con, sql){
  query <- DBI::dbSendStatement(con, sql)
  DBI::dbClearResult(query)
}
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
get_app_db_col_types <- function(){
  
  return(tibble::tribble(
    ~table, ~col_types, ~db, ~col_names,
    
    # Main tables
    
    "users", "icccciicl", "main", c("id", "username", "firstname", "lastname", "password", "user_access_id", "user_status_id", "datetime", "deleted"),
    "users_accesses", "icccl", "main", c("id", "name", "description", "datetime", "deleted"),
    "users_statuses", "icccl", "main", c("id", "name", "description", "datetime", "deleted"),
    "datasets", "iciiccl", "main", c("id", "name", "data_source_id", "creator_id", "creation_datetime", "update_datetime", "deleted"),
    "studies", "iciiiiccl", "main", c("id", "name", "dataset_id", "patient_lvl_tab_group_id", "aggregated_tab_group_id", "creator_id", "creation_datetime", "update_datetime", "deleted"),
    "plugins", "iciccl", "main", c("id", "name", "tab_type_id", "creation_datetime", "update_datetime", "deleted"),
    "scripts", "icccl", "main", c("id", "name", "creation_datetime", "update_datetime", "deleted"),
    "tabs_groups", "icccicl", "main", c("id", "category", "name", "description", "creator_id", "datetime", "deleted"),
    "tabs", "iccciiiicl", "main", c("id", "category", "name", "description", "tab_group_id", "parent_tab_id", "display_order", "creator_id", "datetime", "deleted"),
    "widgets", "icciiiicl", "main", c("id", "category", "name", "tab_id", "plugin_id", "display_order", "creator_id", "datetime", "deleted"),
    "code", "icicicl", "main", c("id", "category", "link_id", "code", "creator_id", "datetime", "deleted"),
    "options", "iciccnicl", "main", c("id", "category", "link_id", "name", "value", "value_num", "creator_id", "datetime", "deleted"),
    "messages", "iiicccicl", "main", c("id", "conversation_id", "study_id", "category", "message", "filepath", "creator_id", "datetime", "deleted"),
    "conversations", "iccl", "main", c("id", "name", "datetime", "deleted"),
    "user_deleted_conversations", "iiic", "main", c("id", "conversation_id", "user_id", "datetime"),
    "inbox_messages", "iiilcl", "main", c("id", "message_id", "receiver_id", "read", "datetime", "deleted"),
    "log", "icccic", "main", c("id", "category", "name", "value", "creator_id", "datetime"),
    "git_repos", "icccccicl", "main", c("id", "unique_id", "name", "api_key", "repo_url_address", "raw_files_url_address", "creator_id", "datetime", "deleted"),
    
    # Public tables
    
    "persons_options", "iiiiiiciccnicl", "public", c("id", "dataset_id", "study_id", "subset_id", "person_id", "visit_detail_id", "category", "link_id", "name", "value", "value_num", "creator_id", "datetime", "deleted"),
    "widgets_options", "iiiicccnicl", "public", c("id", "widget_id", "person_id", "link_id", "category", "name", "value", "value_num", "creator_id", "datetime", "deleted"),
    "subsets", "icciicl", "public", c("id", "name", "description", "study_id", "creator_id", "datetime", "deleted"),
    "options", "iciccnicl", "public", c("id", "category", "link_id", "name", "value", "value_num", "creator_id", "datetime", "deleted"),
    "code", "icicicl", "public", c("id", "category", "link_id", "code", "creator_id", "datetime", "deleted"),
    "subset_persons", "iiiiiicl", "public", c("id", "subset_id", "person_id", "visit_occurrence_id", "visit_detail_id", "creator_id", "datetime", "deleted"),
    "concept", "iiccccccccc", "public", c("id", "concept_id", "concept_name", "domain_id", "vocabulary_id", "concept_class_id", "standard_concept", "concept_code", "valid_start_date", "valid_end_date", "invalid_reason"),
    "concept_dataset", "iiciiii", "public", c("id", "concept_id", "vocabulary_id", "dataset_id", "count_persons_rows", "count_concepts_rows", "count_secondary_concepts_rows"),
    "concept_user", "iiiccc", "public", c("id", "user_id", "concept_id", "concept_name", "concept_display_name", "vocabulary_id"),
    "vocabulary", "icccciciiccl", "public", c("id", "vocabulary_id", "vocabulary_name", "vocabulary_reference", "vocabulary_version", "vocabulary_concept_id", "data_source_id", "display_order", "creator_id", "creation_datetime", "update_datetime", "deleted"),
    "domain", "icci", "public", c("id", "domain_id", "domain_name", "domain_concept_id"),
    "concept_class", "icci", "public", c("id", "concept_class_id", "concept_class_name", "concept_class_concept_id"),
    "concept_relationship", "iiicccc", "public", c("id", "concept_id_1", "concept_id_2", "relationship_id", "valid_start_date", "valid_end_date", "invalid_reason"),
    "concept_relationship_user", "iicic", "public", c("id", "concept_relationship_id", "comment", "creator_id", "datetime"),
    "concept_relationship_evals", "iiicc", "public", c("id", "concept_relationship_id", "creator_id", "evaluation_id", "datetime"),
    "relationship", "iccccci", "public", c("id", "relationship_id", "relationship_name", "is_hierarchical", "defines_ancestry", "reverse_relationship_id", "relationship_concept_id"),
    "concept_synonym", "iici", "public", c("id", "concept_id", "concept_synonym_name", "language_concept_id"),
    "concept_ancestor", "iiiii", "public", c("id", "ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation", "max_levels_of_separation"),
    "drug_strength", "iiinininiiccc", "public", c("id", "drug_concept_id", "ingredient_concept_id", "amount_value", "amount_unit_concept_id", "numerator_value", "numerator_unit_concept_id", "denominator_value", "denominator_unit_concept_id", "box_size", "valid_start_date", "valid_end_date", "invalid_reason"),
    "widgets_concepts", "iiicccilicl", "public", c("id", "widget_id", "concept_id", "concept_name", "concept_display_name", "domain_id", "mapped_to_concept_id", "merge_mapped_concepts", "creator_id", "datetime", "deleted")
  ))
}

#' @noRd
db_create_tables <- function(db, type, dbms){
  
  # Create tables if does not exist
  
  # Type = main for main database
  # Type = public for plugins / tabs database
  
  db_col_types <- get_app_db_col_types()
  
  for (type in c("main", "public")){
    
    tables <- db_col_types %>% dplyr::filter(db == type) %>% dplyr::pull(table)
    
    for (table in tables){
      
      row <- db_col_types %>% dplyr::filter(db == type, table == !!table)
      
      types <- strsplit(row$col_types, "")[[1]]
      cols <- lapply(seq_along(types), function(i) {
        switch(
          types[[i]],
          "i" = integer(),
          "c" = character(),
          "n" = numeric(),
          "l" = logical()
        )
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
get_db <- function(){
  
  # Get variables from other environments
  app_db_folder <- get("app_db_folder", envir = parent.frame())
  
  # Get local database connection
  
  db <- list()
  
  local_main_sqlite_file <- paste0(app_db_folder, "/linkr_main")
  db$local_main <- DBI::dbConnect(RSQLite::SQLite(), local_main_sqlite_file)
  Sys.chmod(local_main_sqlite_file, mode = "0664")
  
  local_public_sqlite_file <- paste0(app_db_folder, "/linkr_public")
  db$local_public <- DBI::dbConnect(RSQLite::SQLite(), local_public_sqlite_file)
  Sys.chmod(local_public_sqlite_file, mode = "0664")
  
  # Create tables for local databases
  
  db_create_tables(db = db$local_main, type = "main", dbms = "sqlite")
  db_create_tables(db = db$local_public, type = "public", dbms = "sqlite")
  
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
        
        db_create_tables(db = db$remote_main, type = "main", dbms = db_info$sql_lib)
        db_create_tables(db = db$remote_public, type = "public", dbms = db_info$sql_lib)
        
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
  
  return(db)
}

#' @noRd
get_last_row <- function(con, table){
  glue::glue_sql("SELECT COALESCE(MAX(id), 0) FROM {`table`}", .con = con) -> sql
  DBI::dbGetQuery(con, sql) %>% dplyr::pull() %>% as.integer()
}

#' @noRd
# get_remote_db <- function(){
#   
#   # Get variables from other environments
#   for (obj_name in c("r", "m", "output", "ns")) assign(obj_name, get(obj_name, envir = parent.frame()))
#   i18n <- r$i18n
#   
#   result <- "failure"
#   
#   db_info <- DBI::dbGetQuery(r$local_db, "SELECT * FROM options WHERE category = 'remote_db'") %>% tibble::as_tibble()
#   db_info <- db_info %>% dplyr::pull(value, name) %>% as.list()
#   db <- list()
#   
#   # Try the connection
#   tryCatch({
#     
#     if (db_info$main_db_name != "" & db_info$public_db_name != ""){
#       
#       # Postgres
#       if (db_info$sql_lib == "postgres"){
#         db$main <- DBI::dbConnect(RPostgres::Postgres(), dbname = db_info$main_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
#         db$plugins <- DBI::dbConnect(RPostgres::Postgres(), dbname = db_info$public_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
#       } 
#       
#       # SQLite
#       if (db_info$sql_lib == "sqlite"){
#         db$main <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_info$main_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
#         db$plugins <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_info$public_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
#       }
#       
#       r$remote_db <- db$main
#       m$remote_db <- db$plugins
#       
#       result <- "success"
#     }
#     
#   }, error = function(e) if (nchar(e[1]) > 0) cat(paste0("\n", now(), " - get_remote_db - error = ", toString(e))))
#   
#   result
# }

#' @noRd
# load_database <- function(){
#   
#   # Get variables from other environments
#   for (obj_name in c("r", "m")) assign(obj_name, get(obj_name, envir = parent.frame()))
#   i18n <- r$i18n
#   
#   # Database tables to load
#   r_tables <- c("users", "users_accesses", "users_statuses", "datasets", "plugins", "scripts", "code", "options", "git_repos")
#   
#   m_tables <- c("vocabulary")
#   
#   sapply(m_tables, function(table){
#     # Easier to load vocabulary in r var
#     r[[table]] <- DBI::dbGetQuery(m$db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
#     r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
#   })
#   
#   # Add a tab_types variable, for settings/plugins dropdown
#   r$tab_types <- tibble::tribble(~id, ~name, 1, i18n$t("patient_level_data"), 2, i18n$t("aggregated_data"))
# }

#' @noRd
sql_send_statement <- function(con, sql){
  query <- DBI::dbSendStatement(con, sql)
  DBI::dbClearResult(query)
}
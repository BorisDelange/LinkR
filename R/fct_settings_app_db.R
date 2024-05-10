#' Check user authentification
#' 
#' @description Check user authentification using shinymanager library
#' @param db A DBI database connection object
# check_authentification <- function(db){
#   function(user, password) {
#     
#     password <- rlang::hash(password)
#     
#     res <- DBI::dbGetQuery(db, paste0("SELECT * FROM users WHERE username = '", user, "' AND password = '", password, "' AND deleted IS FALSE"))
#     
#     if (nrow(res) > 0) list(result = TRUE, user_info = list(user = user, id = res$id))
#     else list(result = FALSE)
#   }
# }

#' Create a new table
#' 
#' @description Create a new table in the database
#' @details The distinct constraints can be combined (a column can have a constraint UNIQUE and NOT NULL)
#' 
#' @param db A DBI database connection object
#' @param table_name Name of the new table in the database (character)
#' @param dataframe Dataframe with table informations (columns names & type of value) (dataframe / tibble)
#' @param dbms Database management system used ("postgres" and "sqlite" supported) (character)
#' @param primary_key_col Character vector containing the primary key columns, to create a PRIMARY KEY constraint (character)
#' @param unique_cols Character vector containing the columns with a UNIQUE contraint (character)
#' @param not_null_cols Character vector containing the columns with a NOT NULL constraint (character)
#' @param text_cols Character vector containing the text columns, to create a column with TEXT type in postgres DBMS (character)
#' @examples 
#' \dontrun{
#' db_create_table(db = db, "my_new_table", tibble::tibble(id = integer(), name = character()), dbms = "postgres",
#'   primary_key_col = "id")
#' }
db_create_table <- function(db, table_name = character(), dataframe = tibble::tibble(), dbms = character(), 
  primary_key_col = character(), unique_cols = character(), not_null_cols = character(), text_cols = character()){
  if (table_name %not_in% DBI::dbListTables(db)){
    
    sql <- ""
    
    for (i in 1:ncol(dataframe)){
      col_class <- dataframe[, i] %>% dplyr::pull() %>% class()
      col_name <- colnames(dataframe[, i])
      
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
    
    sql <- glue::glue_sql("CREATE TABLE {`table_name`} (", sql, ")", .con = db)
    query <- DBI::dbSendStatement(db, sql)
    DBI::dbClearResult(query)
  }
}

#' Create tables
#' 
#' @description Create all the tables used in the application.
#' @param db A DBI database connection object
#' @param dbms Database management system used ("postgres" and "sqlite" supported) (character)
#' @examples 
#' \dontrun{
#' db_create_tables(db = db, dbms = "postgres")
#' }
db_create_tables <- function(db, type = character(), dbms = character()){
  
  # Create tables if does not exist
  
  # Type = main for main database
  # Type = public for plugins / tabs database
  
  # Don't forget to add table characteristics in mod_settings_app_database when you add a new table
  # (for import database)
  
  if (type == "main"){
    # In table users, create an admin user
    db_create_table(db, "users", primary_key_col = "id", dbms = dbms,
      tibble::tibble(id = integer(), username = character(), firstname = character(), lastname = character(), password = character(),
        user_access_id = integer(), user_status_id = integer(), datetime = character(), deleted = logical()))
    
    db_create_table(db, "users_accesses", primary_key_col = "id", dbms = dbms,
      tibble::tibble(id = integer(), name = character(), description = character(),
        datetime = character(), deleted = logical()))
    
    db_create_table(db, "users_statuses", primary_key_col = "id", dbms = dbms,
      tibble::tibble(id = integer(), name = character(), description = character(),
        datetime = character(), deleted = logical()))
    
    db_create_table(db, "datasets", primary_key_col = "id", dbms = dbms, text_cols = "description",
      tibble::tibble(id = integer(), name = character(), data_source_id = integer(), creator_id = integer(),
        creation_datetime = character(), update_datetime = character(), deleted = logical()))
    
    db_create_table(db, "studies", primary_key_col = "id", dbms = dbms, text_cols = "description",
      tibble::tibble(id = integer(), name = character(), dataset_id = integer(),
        patient_lvl_tab_group_id = integer(), aggregated_tab_group_id = integer(), creator_id = integer(),
        creation_datetime = character(), update_datetime = character(), deleted = logical()))
    
    db_create_table(db, "plugins", primary_key_col = "id", dbms = dbms, text_cols = "description",
      tibble::tibble(id = integer(), name = character(), tab_type_id = integer(), 
        creation_datetime = character(), update_datetime = character(), deleted = logical()))
    
    db_create_table(db, "scripts", primary_key_col = "id", dbms = dbms,
      tibble::tibble(id = integer(), name = character(),
        creation_datetime = character(), update_datetime = character(), deleted = logical()))
    
    db_create_table(db, "tabs_groups", primary_key_col = "id", dbms = dbms, text_cols = "description",
      tibble::tibble(id = integer(), category = character(), name = character(), description = character(), 
        creator_id = integer(), datetime = character(), deleted = logical()))
    
    db_create_table(db, "tabs", primary_key_col = "id", dbms = dbms, text_cols = "description",
      tibble::tibble(id = integer(), category = character(), name = character(), description = character(), 
        tab_group_id = integer(), parent_tab_id = integer(), display_order = integer(), creator_id = integer(), datetime = character(), deleted = logical()))
    
    db_create_table(db, "widgets", primary_key_col = "id", dbms = dbms,
      tibble::tibble(id = integer(), category = character(), name = character(), tab_id = integer(), plugin_id = integer(), 
        display_order = integer(), creator_id = integer(), datetime = character(), deleted = logical()))
    
    db_create_table(db, "code", primary_key_col = "id", dbms = dbms, text_cols = "code",
      tibble::tibble(id = integer(), category = character(), link_id = integer(), code = character(), creator_id = integer(),
        datetime = character(), deleted = logical()))
    
    db_create_table(db, "options", primary_key_col = "id", dbms = dbms, text_cols = "value",
      tibble::tibble(id = integer(), category = character(), link_id = integer(), name = character(), value = character(),
        value_num = numeric(), creator_id = integer(), datetime = character(), deleted = logical()))
    
    db_create_table(db, "messages", primary_key_col = "id", dbms = dbms, text_cols = c("message", "filepath"),
      tibble::tibble(id = integer(), conversation_id = integer(), study_id = integer(), category = character(), 
        message = character(), filepath = character(), creator_id = integer(), datetime = character(), deleted = logical()))
    
    db_create_table(db, "conversations", primary_key_col = "id", dbms = dbms, text_cols = "name",
      tibble::tibble(id = integer(), name = character(), datetime = character(), deleted = logical()))
    
    db_create_table(db, "user_deleted_conversations", primary_key_col = "id", dbms = dbms, text_cols = "name",
      tibble::tibble(id = integer(), conversation_id = integer(), user_id = integer(), datetime = character()))
    
    db_create_table(db, "inbox_messages", primary_key_col = "id", dbms = dbms,
      tibble::tibble(id = integer(), message_id = integer(), receiver_id = integer(), read = logical(), 
        datetime = character(), deleted = logical()))
    
    db_create_table(db, "log", primary_key_col = "id", dbms = dbms, text_cols = "value",
      tibble::tibble(id = integer(), category = character(), name = character(), value = character(), creator_id = integer(), datetime = character()))
    
    db_create_table(db, "git_repos", primary_key_col = "id", dbms = dbms, text_cols = c("description", "link"),
      tibble::tibble(id = integer(), unique_id = character(), name = character(), api_key = character(),
        repo_url_address = character(), raw_files_url_address = character(), creator_id = integer(), datetime = character(), deleted = logical()))
  }
  
  if (type == "public"){
    
    db_create_table(db, "persons_options", primary_key_col = "id", dbms = dbms, text_cols = "value",
      tibble::tibble(id = integer(), dataset_id = integer(), study_id = integer(), subset_id = integer(), person_id = integer(), visit_detail_id = integer(),
        category = character(), link_id = integer(), name = character(), value = character(), value_num = numeric(), 
        creator_id = integer(), datetime = character(), deleted = logical()))
    
    db_create_table(db, "widgets_options", primary_key_col = "id", dbms = dbms, text_cols = "value",
      tibble::tibble(id = integer(), widget_id = integer(), person_id = integer(), link_id = integer(),
        category = character(), name = character(), value = character(), value_num = numeric(),
        creator_id = integer(), datetime = character(), deleted = logical()))
    
    db_create_table(db, "subsets", primary_key_col = "id", dbms = dbms, text_cols = "description",
      tibble::tibble(id = integer(), name = character(), description = character(), study_id = integer(), creator_id = integer(),
        datetime = character(), deleted = logical()))
    
    db_create_table(db, "subset_persons", primary_key_col = "id", dbms = dbms,
      tibble::tibble(id = integer(), subset_id = integer(), person_id = integer(), creator_id = integer(), datetime = character(), deleted = logical()))
    
    db_create_table(db, "concept", primary_key_col = "id", dbms = dbms, 
      tibble::tibble(id = integer(), concept_id = integer(), concept_name = character(), domain_id = character(), vocabulary_id = character(),
        concept_class_id = character(), standard_concept = character(), concept_code = character(), valid_start_date = character(),
        valid_end_date = character(), invalid_reason = character()))
    
    db_create_table(db, "concept_dataset", primary_key_col = "id", dbms = dbms,
      tibble::tibble(id = integer(), concept_id = integer(), vocabulary_id = character(), dataset_id = integer(), 
        count_persons_rows = integer(), count_concepts_rows = integer(), count_secondary_concepts_rows = integer()))
    
    db_create_table(db, "concept_user", primary_key_col = "id", dbms = dbms, text_cols = c("name", "display_name"),
      tibble::tibble(id = integer(), user_id = integer(), concept_id = integer(), concept_name = character(), concept_display_name = character(),
        vocabulary_id = character()))
    
    db_create_table(db, "vocabulary", primary_key_col = "id", dbms = dbms,
      tibble::tibble(id = integer(), vocabulary_id = character(), vocabulary_name = character(), 
        vocabulary_reference = character(), vocabulary_version = character(), vocabulary_concept_id = integer(), data_source_id = character(), 
        display_order = integer(), creator_id = integer(), creation_datetime = character(), update_datetime = character(), deleted = logical()))
    
    db_create_table(db, "domain", primary_key_col = "id", dbms = dbms,
      tibble::tibble(id = integer(), domain_id = character(), domain_name = character(), domain_concept_id = integer()))
    
    db_create_table(db, "concept_class", primary_key_col = "id", dbms = dbms,
      tibble::tibble(id = integer(), concept_class_id = character(), concept_class_name = character(), concept_class_concept_id = integer()))
    
    db_create_table(db, "concept_relationship", primary_key_col = "id", dbms = dbms,
      tibble::tibble(id = integer(), concept_id_1 = integer(), concept_id_2 = integer(), relationship_id = character(),
        valid_start_date = character(), valid_end_date = character(), invalid_reason = character()))
    
    db_create_table(db, "concept_relationship_user", primary_key_col = "id", dbms = dbms,
      tibble::tibble(id = integer(), concept_relationship_id = integer(), comment = character(), creator_id = integer(), datetime = character()))
    
    db_create_table(db, "concept_relationship_evals", primary_key_col = "id", dbms = dbms,
      tibble::tibble(id = integer(), concept_relationship_id = integer(), creator_id = integer(), evaluation_id = integer(),
        datetime = character()))
    
    db_create_table(db, "relationship", primary_key_col = "id", dbms = dbms,
      tibble::tibble(id = integer(), relationship_id = character(), relationship_name = character(), is_hierarchical = character(),
        defines_ancestry = character(), reverse_relationship_id = character(), relationship_concept_id = integer()))
    
    db_create_table(db, "concept_synonym", primary_key_col = "id", dbms = dbms,
      tibble::tibble(id = integer(), concept_id = integer(), concept_synonym_name = character(), language_concept_id = integer()))
    
    db_create_table(db, "concept_ancestor", primary_key_col = "id", dbms = dbms,
      tibble::tibble(id = integer(), ancestor_concept_id = integer(), descendant_concept_id = integer(),
        min_levels_of_separation = integer(), max_levels_of_separation = integer()))
    
    db_create_table(db, "drug_strength", primary_key_col = "id", dbms = dbms,
      tibble::tibble(id = integer(), drug_concept_id = integer(), ingredient_concept_id = integer(), amount_value = numeric(),
        amount_unit_concept_id = integer(), numerator_value = numeric(), numerator_unit_concept_id = integer(),
        denominator_value = numeric(), denominator_unit_concept_id = integer(), box_size = integer(),
        valid_start_date = character(), valid_end_date = character(), invalid_reason = character()))
    
    db_create_table(db, "widgets_concepts", primary_key_col = "id", dbms = dbms,
      tibble::tibble(id = integer(), widget_id = integer(), concept_id = integer(),
        concept_name = character(), concept_display_name = character(), domain_id = character(), 
        mapped_to_concept_id = integer(), merge_mapped_concepts = logical(),
        creator_id = integer(), datetime = character(), deleted = logical()))
  }
}

#' Get authorized data for a user
#'
#' @details Some data have a restricted access. This function filter the data the user has access to.
#' @param r Shiny reactive value, used to communicate between modules
#' @param table Name of the table in the database (character)
#' @param data If data is not r[[table]] (tibble / dataframe)
#' @examples
#' \dontrun{
#' get_authorized_data(r = r, table = "plugins")
#' }
get_authorized_data <- function(r, table, data = tibble::tibble()){
  
  if (nrow(data) == 0) data <- r[[table]]
  
  # Merge with options
  options <- data %>% dplyr::inner_join(r$options %>% dplyr::filter(category == get_singular(table)) %>% 
      dplyr::select(option_id = id, link_id, option_name = name, value, value_num), by = c("id" = "link_id"))
  
  # Vector of authorized data
  data_allowed <- integer()
  
  # For each data row, select those the user has access
  sapply(unique(options$id), function(data_id){
    
    # Loop over each data ID
    
    users_allowed_read_group <- options %>% dplyr::filter(id == data_id, option_name == "users_allowed_read_group")
    users_allowed_read <- options %>% dplyr::filter(id == data_id, option_name == "user_allowed_read")
    
    if (users_allowed_read_group %>% dplyr::pull(value) == "everybody") data_allowed <<- c(data_allowed, data_id)
    else if (nrow(users_allowed_read %>% dplyr::filter(value_num == r$user_id)) > 0) data_allowed <<- c(data_allowed, data_id)
    
  })
  
  # Select authorized data
  data %>% dplyr::filter(id %in% data_allowed)
}

#' Get database DBI object
#'
#' @description Get final database connection DBI object (local or remote).
#' @param r Shiny reactive value, used to communicate between modules
#' @param m Shiny reactive value, used to communicate between modules
#' @param app_db_folder Folder where the application database is saved (character)
#' @examples
#' \dontrun{
#' get_db(r = r, m = m, app_db_folder = paste0(r$app_folder, "/app_database"))
#' }
get_db <- function(r = shiny::reactiveValues(), m = shiny::reactiveValues(), app_db_folder = character()){
  
  # Get local database connection
  
  r$db_connection <- "local"
  
  db <- list()
  db$local_main <- DBI::dbConnect(RSQLite::SQLite(), paste0(app_db_folder, "/linkr_main"))
  r$db <- db$local_main
  db$local_public <- DBI::dbConnect(RSQLite::SQLite(), paste0(app_db_folder, "/linkr_public"))
  m$db <- db$local_public
  
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
        
        r$db_connection <- "remote"
        
        r$db <- db$remote_main
        m$db <- db$remote_public
        
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

#' Connection to remote database
#' 
#' @description Get a connection to a remote database. If the remote connection fails, returns local DBI connection object.
#' @param r Shiny reactive value, used to communicate between modules
#' @param m Shiny reactive value, used to communicate between modules
#' @param output Shiny output variable
#' @param i18n Translator object from shiny.i18n library
#' @param ns Shiny namespace
#' @examples
#' \dontrun{
#' get_remote_db(r = r, m = m, output = output, i18n = i18n, ns = ns)
#' }
get_remote_db <- function(r = shiny::reactiveValues(), m = shiny::reactiveValues(), output, i18n = character(), ns = character()){
  
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

#' Load database
#' 
#' @description Load application database with only used tables
#' @param r Shiny reactive value, used to communicate between modules
#' @param m Shiny reactive value, used to communicate between modules
#' @param i18n Translator object from shiny.i18n library
#' @examples
#' \dontrun{
#' load_database(r = r, m = m, i18n = i18n)
#' }
load_database <- function(r = shiny::reactiveValues(), m = shiny::reactiveValues(), i18n = character()){
  # Database tables to load
  r_tables <- c("users", "users_accesses", "users_statuses", "data_sources", "datasets",
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
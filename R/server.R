#' @noRd
app_server <- function(
    pages, language, languages, i18n, app_folder, username, debug, log_file, local, 
    users_accesses_toggles_options, db_col_types, dropdowns, auto_complete_list, loading_options
  ){
  
  function(input, output, session) {
    
    if (debug) cat(paste0("\n", now(), " - server - init"))
    
    language <- tolower(language)
    
    if (debug) cat(paste0("\n", now(), " - server - reactive values"))
    
    # Create observers ----
    
    # Create r reactive value, for the application processings
    r <- reactiveValues()
    
    # Create d reactive value, for projects data
    d <- reactiveValues()
    main_tables <- c(
      "condition_occurrence",
      "drug_exposure",
      "procedure_occurrence",
      "device_exposure",
      "measurement",
      "observation",
      "death",
      "note",
      "note_nlp",
      "specimen",
      "fact_relationship",
      "payer_plan_period",
      "cost", 
      "drug_era",
      "dose_era",
      "condition_era", 
      "person",
      "observation_period",
      "visit_occurrence",
      "visit_detail",
      "location",
      "care_site",
      "provider"
    )
    sapply(main_tables, function(table) d[[table]] <- tibble::tibble())
    r$main_tables <- main_tables
    
    # Create m reactive value, for plugins & widgets data
    m <- reactiveValues()
    
    # Create o reactive values, for observers inactivation
    o <- reactiveValues()
    
    # App version ----
    r$app_version <- "0.3.1.9003"
    
    # Databse col types ----
    
    # Test internet connection ----
    # If local is TRUE, don't use internet connection
    if (debug) cat(paste0("\n", now(), " - server - has_internet"))
    if (local) has_internet <- FALSE
    else has_internet <- curl::has_internet()
    r$has_internet <- has_internet
    
    # App folder ----
    if (debug) cat(paste0("\n", now(), " - server - app_folder"))
    r$app_folder <- app_folder
    m$app_folder <- app_folder
    
    # App db folder ----
    app_db_folder <- paste0(app_folder, "/app_database")
    
    # Translations ----
    
    if (debug) cat(paste0("\n", now(), " - server - translations"))
    
    r$i18n <- i18n
    r$languages <- languages
    r$language <- language
    m$language <- language
    
    r$dropdowns <- dropdowns
    
    # Connection to database ----
    # If connection informations have been given in linkr() function, use these informations
    if (debug) cat(paste0("\n", now(), " - server - app_db"))
    local_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(app_db_folder, "/linkr_main"))
    r$local_db <- local_db
    m$local_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(app_db_folder, "/linkr_public"))
    
    db_local_main <- get_db(r = r, m = m, app_db_folder = app_db_folder, db_col_types = db_col_types)
    
    # Db col types
    r$db_col_types <- db_col_types
    
    # Add default values in database if database is empty
    # Load all data from database
    # Don't load concept, load it only when a vocabulary is selected
    # Don't load cache table neither
    
    observeEvent(r$db, {
      
      if (debug) cat(paste0("\n", now(), " - server - observer r$db"))
      
      # Add default values in database, if it is empty
      insert_default_data(output = output, r = r, m = m, i18n = i18n, language = language, db_col_types = db_col_types, users_accesses_toggles_options = users_accesses_toggles_options)
      
      # Connection with username
      sql <- glue::glue_sql("SELECT id FROM users WHERE username = {username}", .con = local_db)
      user_id <- DBI::dbGetQuery(local_db, sql)
      if (nrow(user_id) > 0) r$user_id <- user_id %>% dplyr::pull()
      else stop("Username not found in app database")
      
      # Load datasets
      sql <- glue::glue_sql("SELECT * FROM datasets", .con = r$db)
      r$datasets_wide <- DBI::dbGetQuery(r$db, sql) %>% tibble::as_tibble()
      
      # Load plugins
      sql <- glue::glue_sql("SELECT * FROM plugins", .con = r$db)
      r$plugins_wide <- DBI::dbGetQuery(r$db, sql) %>% tibble::as_tibble()
      
      # Load vocabularies
      sql <- glue::glue_sql("SELECT * FROM vocabulary", .con = m$db)
      r$vocabularies_wide <- DBI::dbGetQuery(m$db, sql) %>% tibble::as_tibble()
      
      # Load users names
      sql <- glue::glue_sql(paste0(
        "SELECT u.id, u.username, u.firstname, u.lastname, (u.firstname || ' ' || u.lastname) AS name, (SUBSTR(u.firstname, 1, 1) || SUBSTR(u.lastname, 1, 1)) AS initials, ",
        "s.id AS user_status_id, s.name AS user_status, a.id AS user_access_id, a.name AS user_access ",
        "FROM users u ",
        "LEFT JOIN users_statuses s ON u.user_status_id = s.id ",
        "LEFT JOIN users_accesses a ON u.user_access_id = a.id"), .con = r$db)
      
      r$users <- DBI::dbGetQuery(r$db, sql) %>% tibble::as_tibble()
      
      # Load git repos
      sql <- glue::glue_sql("SELECT * FROM git_repos", .con = r$db)
      r$git_repos <- DBI::dbGetQuery(r$db, sql) %>% tibble::as_tibble()
      
      # Retro-compatibility: delete all insertions with DELETED IS TRUE
      sql <- glue::glue_sql("SELECT * FROM options WHERE name = 'unused_rows_deleted' AND value = 'true'", .con = r$db)
      if (nrow(DBI::dbGetQuery(r$db, sql)) == 0){
        
        for (i in 1:nrow(db_col_types)){
          row <- db_col_types[i, ]
          
          if (row$table %not_in% c("user_deleted_conversations", "log", "concept", "concept_dataset", "concept_user", "domain",
            "concept_class", "concept_relationship", "concept_relationship_user", "concept_relationship_evals", "relationship",
            "concept_synonym", "concept_ancestor", "drug_strength")){
            
            if (row$db == "main") db <- r$db
            else db <- m$db
            
            sql <- glue::glue_sql("DELETE FROM {row$table} WHERE deleted = TRUE", .con = db)
            query <- DBI::dbSendStatement(db, sql)
            DBI::dbClearResult(query)
          }
        }
        
        new_data <- tibble::tibble(
          id = get_last_row(r$db, "options") + 1, category = 'link_settings', link_id = NA_integer_, name = 'unused_rows_deleted', value = 'true',
          value_num = NA_integer_, creator_id = NA_integer_, datetime = now(), deleted = FALSE
        )
        DBI::dbAppendTable(r$db, "options", new_data)
      }
      
      # Retro-compatibility: add visit_occurrence_id and visit_detail_id cols to subset_persons table
      
      sql <- glue::glue_sql("SELECT * FROM subset_persons LIMIT 1", .con = m$db)
      subset_persons_cols <- DBI::dbGetQuery(m$db, sql)
      
      if ("visit_occurrence_id" %not_in% colnames(subset_persons_cols)){
        sql_send_statement(m$db, "ALTER TABLE subset_persons ADD COLUMN visit_occurrence_id INTEGER AFTER person_id;")
        sql_send_statement(m$db, "ALTER TABLE subset_persons ADD COLUMN visit_detail_id INTEGER AFTER visit_occurrence_id;")
      }
    })
    
    # User is logged in
    
    observeEvent(r$user_id, {
      if (debug) cat(paste0("\n", now(), " - server - observer r$user_id"))
      
      if (log_file){
        
        # Create a log folder for this user if doesn't exist
        local_log_file <- paste0(r$app_folder, "/log/", r$user_id, ".txt")
        if (!file.exists(local_log_file)) file.create(local_log_file)
        
        sink(local_log_file, append = TRUE)
      }
      
      onStop(function() {
        if (debug) cat(paste0("\n", now(), " - server - observer onStop"))
        
        # Close db connections
        # if (length(d$con) > 0) if (DBI::dbIsValid(d$con)) DBI::dbDisconnect(d$con)
        
        # Stop console redirection to log file
        while(sink.number() > 0) sink(NULL)
      })
      
      # Clear temp dir
      if (debug) cat(paste0("\n", now(), " - server - clear temp_files"))
      
      temp_files_folder <- paste0(app_folder, "/temp_files/", r$user_id)
      unlink(temp_files_folder, recursive = TRUE, force = TRUE)
      dir.create(temp_files_folder)
      
      for (folder in c("markdowns", "plugins", "data_cleaning", "datasets", "projects", "app_db", "git_repos")){
        sub_folder <- paste0(app_folder, "/temp_files/", r$user_id, "/", folder)
        unlink(sub_folder, recursive = TRUE, force = TRUE)
        if (!dir.exists(sub_folder)) dir.create(sub_folder)
      }
    }, once = TRUE)
    
    # Route pages ----
    if (debug) cat(paste0("\n", now(), " - server - shiny.router"))
    shiny.router::router_server()
    
    # Load pages ----
    
    r$loaded_pages <- list()
    
    observeEvent(shiny.router::get_page(), {
      
      if (debug) cat(paste0("\n", now(), " - server - observer shiny.router::get_page()"))
      
      current_page <- shiny.router::get_page()
      r$current_page <- current_page
      
      req(current_page %in% pages)
      
      if (current_page == "/") current_page <- "home"
      
      else if (current_page == "data"){
        if (length(shiny.router::get_query_param()$type) > 0) r$data_page <- shiny.router::get_query_param()$type
        else r$data_page <- "patient_lvl"
      }
      
      else if (current_page %in% c("datasets", "data_cleaning", "datasets", "plugins", "subsets", "projects")){
        if (length(shiny.router::get_query_param()$create_element) > 0) shinyjs::runjs(paste0("Shiny.setInputValue('", current_page, "-create_element', Math.random());"))
      }
      
      if (current_page %not_in% names(r$loaded_pages)) r$load_page <- current_page
      
      # Prevent a bug with scroll into ace editor
      shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
    })
    
    observeEvent(r$load_page, {
      if (debug) cat(paste0("\n", now(), " - server - observer r$load_page"))
      
      page <- r$load_page
      
      # Get user accesses
      user_access_id <- r$users %>% dplyr::filter(id == r$user_id) %>% dplyr::pull(user_access_id) 
      sql <- glue::glue_sql("SELECT * FROM options WHERE category = 'users_accesses' AND link_id = {user_access_id} AND value_num = 1", .con = r$db)
      user_accesses <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(name)
      
      # Get user settings
      sql <- glue::glue_sql("SELECT name, value, value_num FROM options WHERE category = 'user_settings' AND link_id = {r$user_id}", .con = r$db)
      user_settings_db <- DBI::dbGetQuery(r$db, sql)
      user_settings <- list()
      
      user_settings$ace_theme <- user_settings_db %>% dplyr::filter(name == "ace_theme") %>% dplyr::pull(value)
      if (length(user_settings$ace_theme) == 0) user_settings$ace_theme <- "textmate"
      
      user_settings$ace_font_size <- user_settings_db %>% dplyr::filter(name == "ace_font_size") %>% dplyr::pull(value_num)
      if (length(user_settings$ace_font_size) == 0) user_settings$ace_font_size <- 11
      
      # Data pages are loaded from mod_widgets (when a project is selected)
      if (page == "data"){

        mod_data_server("data", r, d, m, language, i18n, debug, user_accesses)
        mod_page_sidenav_server("data", r, d, m, language, i18n, debug)
        mod_page_header_server("data", r, d, m, language, i18n, debug)
        r$loaded_pages$data <- TRUE

        r$load_project_trigger <- now()
      }
      else {
        if (page == "users") args <- list(page, r, d, m, language, i18n, debug, users_accesses_toggles_options, user_accesses)
        else if (page == "app_db") args <- list(page, r, d, m, language, i18n, db_col_types, app_folder, debug, user_accesses, user_settings)
        else if (page %in% c("console", "data_cleaning", "datasets", "plugins", "projects", "project_files", "subsets", "user_settings", "vocabularies")) args <- list(page, r, d, m, language, i18n, debug, user_accesses, user_settings)
        else args <- list(page, r, d, m, language, i18n, debug, user_accesses)
        do.call(paste0("mod_", page, "_server"), args)

        mod_page_sidenav_server(page, r, d, m, language, i18n, debug)
        mod_page_header_server(page, r, d, m, language, i18n, debug)

        r$loaded_pages[[page]] <- TRUE
      }
    })
    
    # Loading options
    
    r$loading_options <- loading_options
    
    # Go to a specific page if noticed in loading_options
    
    if (length(loading_options$page) > 0){
      if (loading_options$page %in% pages) shinyjs::delay(10, shiny.router::change_page(loading_options$page))
      else cat(paste0("\n", now(), " - server - ", loading_options$page, " is not a valid page"))
    }
  }
}

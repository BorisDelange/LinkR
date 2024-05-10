#' @noRd
app_server <- function(pages, language, languages, i18n, app_folder, debug, local, users_accesses_toggles_options){
  function(input, output, session ) {
    
    if (debug) cat(paste0("\n", now(), " - server - init"))
    
    language <- tolower(language)
    
    if (debug) cat(paste0("\n", now(), " - server - reactive values"))
    
    # Create r reactive value, for the application processings
    r <- reactiveValues()
    
    # Create d reactive value, for projects data
    d <- reactiveValues()
    main_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
      "observation", "death", "note", "note_nlp", "specimen", "fact_relationship", "payer_plan_period", "cost", 
      "drug_era", "dose_era", "condition_era", 
      "person", "observation_period", "visit_occurrence", "visit_detail",
      "location", "care_site", "provider")
    sapply(main_tables, function(table) d[[table]] <- tibble::tibble())
    
    # Create m reactive value, for plugins & widgets data
    m <- reactiveValues()
    
    # Create o reactive values, for observers inactivation
    o <- reactiveValues()
    
    # App version
    r$app_version <- "0.2.0.9085"
    
    # Col types of database tables, to import and restore database
    db_col_types <- tibble::tribble(
      ~table, ~col_types, ~db,
      "users", "icccciicl", "main",
      "users_accesses", "icccl", "main",
      "users_statuses", "icccl", "main",
      "data_sources", "iccicl", "main",
      "datasets", "iciiccl", "main",
      "studies", "iciiiiccl", "main",
      "plugins", "iciccl", "main",
      "scripts", "icccl", "main",
      "tabs_groups", "icccicl", "main",
      "tabs", "iccciiiicl", "main",
      "widgets", "icciiiicl", "main",
      "code", "icicicl", "main",
      "options", "iciccnicl", "main",
      "messages", "iiicccicl", "main",
      "conversations", "iccl", "main",
      "user_deleted_conversations", "iiic", "main",
      "inbox_messages", "iiilcl", "main",
      "log", "icccic", "main",
      "git_repos", "icccccicl", "main",
      "persons_options", "iiiiiiciccnicl", "public",
      "widgets_options", "iiiicccnicl", "public",
      "subsets", "icciicl", "public",
      "options", "iciccnicl", "public",
      "subset_persons", "iiiicl", "public",
      "concept", "iiccccccccc", "public",
      "concept_dataset", "iiciiii", "public",
      "concept_user", "iiiccc", "public",
      "vocabulary", "icccciciiccl", "public",
      "domain", "icci", "public",
      "concept_class", "icci", "public",
      "concept_relationship", "iiicccc", "public",
      "concept_relationship_user", "iicic", "public",
      "concept_relationship_evals", "iiicc", "public",
      "relationship", "iccccci", "public",
      "concept_synonym", "iici", "public",
      "concept_ancestor", "iiiii", "public",
      "drug_strength", "iiinininiiccc", "public",
      "widgets_concepts", "iiicccilicl", "public"
    )
    
    # Test internet connection
    # If local is TRUE, don't use internet connection
    if (debug) cat(paste0("\n", now(), " - server - has_internet"))
    if (local) has_internet <- FALSE
    else has_internet <- curl::has_internet()
    r$has_internet <- has_internet
    
    # App folder
    if (debug) cat(paste0("\n", now(), " - server - app_folder"))
    r$app_folder <- app_folder
    m$app_folder <- app_folder
    
    # App db folder
    app_db_folder <- paste0(app_folder, "/app_database")
    
    # Get translations
    
    if (debug) cat(paste0("\n", now(), " - server - translations"))
    
    r$i18n <- i18n
    r$languages <- languages
    r$language <- language
    m$language <- language
    
    # Connection to database
    # If connection informations have been given in linkr() function, use these informations
    if (debug) cat(paste0("\n", now(), " - server - app_db"))
    r$local_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(app_db_folder, "/linkr_main"))
    m$local_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(app_db_folder, "/linkr_public"))
    
    db_local_main <- get_db(r = r, m = m, app_db_folder = app_db_folder)
    
    # Close DB connection on exit
    # And restore initial working directory
    trad <- list()
    trad$session <- switch(language, "fr" = "Session", "en" = "Session")
    trad$session_starts <- switch(language, "fr" = "Début de la session", "en" = "Session starts")
    trad$session_ends <- switch(language, "fr" = "Fin de la session", "en" = "Session ends")
    
    # Add default values in database if database is empty
    # Load all data from database
    # Don't load concept, load it only when a vocabulary is selected
    # Don't load cache table neither
    
    observeEvent(r$db, {
      
      if (debug) cat(paste0("\n", now(), " - server - observer r$db"))
      
      # Add default values in database, if it is empty
      insert_default_data(output = output, r = r, m = m, i18n = i18n, language = language, db_col_types = db_col_types, users_accesses_toggles_options = users_accesses_toggles_options)
      
      # Load datasets
      sql <- glue::glue_sql("SELECT * FROM datasets", .con = r$db)
      r$datasets_wide <- DBI::dbGetQuery(r$db, sql)
      
      # Load plugins
      sql <- glue::glue_sql("SELECT * FROM plugins", .con = r$db)
      r$plugins_wide <- DBI::dbGetQuery(r$db, sql)
      
      # Load vocabularies
      sql <- glue::glue_sql("SELECT * FROM vocabulary", .con = m$db)
      r$vocabulary_wide <- DBI::dbGetQuery(m$db, sql)
      
      # Load users names
      sql <- glue::glue_sql("SELECT id, CONCAT(firstname, ' ', lastname) AS name, CONCAT(SUBSTRING(firstname, 1, 1), SUBSTRING(lastname, 1, 1)) AS initials FROM users", .con = r$db)
      r$users <- DBI::dbGetQuery(r$db, sql)
      
      # Retro-compatibility : delete all insertions with DELETED IS TRUE
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
    })
    
    # Secure the app with shinymanager
    
    # if (debug) cat(paste0(c"\n", now(), " - server - shinyManager"))
    
    # res_auth <- shinymanager::secure_server(check_credentials = function(user, password) {
    #   password <- rlang::hash(password)
    #   
    #   res <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM users WHERE username = '", user, "' AND password = '", password, "' AND deleted IS FALSE"))
    #   
    #   if (nrow(res) > 0) list(result = TRUE, user_info = list(user = user, id = res$id))
    #   else list(result = FALSE)
    # })
    # res_auth <- shinymanager::secure_server(check_credentials = shinymanager::check_credentials(credentials))
    
    # Get user ID
    
    # observeEvent(res_auth, {
      # if (debug) cat(paste0("\n", now(), " - server - observer res_auth"))
      # req(length(reactiveValuesToList(res_auth)$id) > 0)
      # user_id <- as.integer(reactiveValuesToList(res_auth)$id)
      user_id <- 1L
      r$user_id <- user_id
      m$user_id <- user_id
      
      # # add_log_entry(r = r, category = trad$session, name = trad$session_starts, value = "")
      # id_row <- get_last_row(db_local_main, "log") + 1
      # sql <- glue::glue_sql("INSERT INTO log(id, category, name, value, creator_id, datetime) SELECT {id_row}, {trad$session}, {trad$session_starts}, '', {user_id}, {now()}", .con = db_local_main)
      # query <- DBI::dbSendStatement(db_local_main, sql)
      # DBI::dbClearResult(query)
    # })
    # When r$user_id loaded, load user_accesses
    
    observeEvent(r$user_id, {
      if (debug) cat(paste0("\n", now(), " - server - observer r$user_id"))
      
      # req(r$user_id)
      
      # onStop(function() {
      #   if (debug) cat(paste0("\n", now(), " - server - observer onStop"))
      #   add_log_entry(r = isolate(r), category = trad$session, name = trad$session_ends, value = "")
      #   
      #   # Close duckdb connections
      #   DBI::dbDisconnect(isolate(r$db))
      #   if (length(isolate(r$duckdb_drv)) > 0) sapply(isolate(r$duckdb_drv), duckdb::duckdb_shutdown)
      #   
      #   # Close spark connections
      #   sparklyr::spark_disconnect_all()
      # })
      
      # user_access_id <- r$users %>% dplyr::filter(id == r$user_id) %>% dplyr::pull(user_access_id)
      
      # Get user accesses
      # r$user_accesses <- r$options %>% dplyr::filter(category == "users_accesses" & link_id == user_access_id & value_num == 1) %>% dplyr::pull(name)
      # m$user_accesses <- r$user_accesses
      
      # Show username on top of the page
      # sql <- glue::glue_sql("SELECT CONCAT(firstname, ' ', lastname) AS name, CONCAT(SUBSTRING(firstname, 1, 1), SUBSTRING(lastname, 1, 1)) AS initials FROM users WHERE id = {r$user_id}", .con = r$db)
      # r$user <- DBI::dbGetQuery(r$db, sql)
      
      # Clear temp dir
      if (debug) cat(paste0("\n", now(), " - server - clear temp_files"))
      
      temp_files_folder <- paste0(app_folder, "/temp_files/", r$user_id)
      unlink(temp_files_folder, recursive = TRUE, force = TRUE)
      dir.create(temp_files_folder)
      
      for (folder in c("markdowns", "plugins", "data_cleaning_scripts", "vocabularies", "datasets", "studies", "app_db", "git_repos")){
        sub_folder <- paste0(app_folder, "/temp_files/", r$user_id, "/", folder)
        unlink(sub_folder, recursive = TRUE, force = TRUE)
        if (!dir.exists(sub_folder)) dir.create(sub_folder)
      }
    })
    
    # Route pages
    if (debug) cat(paste0("\n", now(), " - server - shiny.router"))
    shiny.router::router_server()
    
    # Keep trace of loaded observers (not to have multiple identical observers)
    # r$loaded_observers <- ""
    
    # Load pages
    
    r$loaded_pages <- list()
    
    sapply(pages, function(page){
      
      if (page == "/") page <- "home"
      
      observeEvent(shiny.router::get_page(), {
        if (shiny.router::get_page() == "/") current_page <- "home"
        else if (shiny.router::get_page() == "data"){
          if (length(shiny.router::get_query_param()$type) > 0) r$data_page <- shiny.router::get_query_param()$type
          else r$data_page <- "patient_lvl"
          current_page <- "data"
        }
        else current_page <- shiny.router::get_page()
        
        if (current_page == page & length(r$loaded_pages[[page]]) == 0) r$load_page <- page
      })
    })
    
    observeEvent(r$load_page, {
      if (debug) cat(paste0("\n", now(), " - server - observer r$load_page"))
      
      page <- r$load_page
      
      # Data pages are loaded from mod_home (when a project is selected)
      if (page == "data"){
        
        # sapply(c("patient_level_data", "aggregated_data"), function(page){
        mod_data_server("data", r, d, m, language, i18n, debug)
        mod_page_sidenav_server("data", r, d, m, language, i18n, debug)
        mod_page_header_server("data", r, d, m, language, i18n, debug)
        r$loaded_pages$data <- TRUE
        # })
        
        # Also load concepts page
        mod_concepts_server("concepts", r, d, m, language, i18n, debug)
        mod_page_sidenav_server("concepts", r, d, m, language, i18n, debug)
        mod_page_header_server("concepts", r, d, m, language, i18n, debug)
        r$loaded_pages$concepts <- TRUE
        
        r$load_project_trigger <- now()
      }
      else {
        if (page == "users") args <- list(page, r, d, m, language, i18n, debug, users_accesses_toggles_options)
        else args <- list(page, r, d, m, language, i18n, debug)
        do.call(paste0("mod_", page, "_server"), args)
        
        mod_page_sidenav_server(page, r, d, m, language, i18n, debug)
        mod_page_header_server(page, r, d, m, language, i18n, debug)
        
        r$loaded_pages[[page]] <- TRUE
      }
    })
  }
}
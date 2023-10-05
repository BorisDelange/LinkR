#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#' @param router Router used with shiny.router library
#' @param language Default language to use in the App (character)
#' @param i18n shiny.i18n object for translations
#' @param app_folder Location of the application folder (character).
#' @param perf_monitoring Monitor app performances (logical)
#' @param debug Debug mode : steps and errors will by displayed in the console (logical)
#' @param local Run the app in local mode, do not load files on the internet (logical)
#' @param show_home_page Should the home page be loaded ? (logical)
#' @param users_accesses_toggles_options A tibble containing users accesses, to add in database if no internet access (tibble)
#' @import shiny
#' @noRd

app_server <- function(language = "en", languages = tibble::tibble(), i18n = character(), app_folder = character(), 
  perf_monitoring = FALSE, debug = FALSE, local = FALSE, show_home_page = TRUE, users_accesses_toggles_options = tibble::tibble()){
  function(input, output, session ) {
    
    if (debug) cat(paste0("\n", Sys.time(), " - server - init"))
    
    language <- tolower(language)
    
    if (debug) cat(paste0("\n", Sys.time(), " - server - reactive values"))
    # Create r reactive value, for the application processings
    r <- reactiveValues()
    
    # Create d reactive value, for dataset data
    d <- reactiveValues()
    main_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
      "observation", "death", "note", "note_nlp", "specimen", "fact_relationship", "payer_plan_period", "cost", 
      "drug_era", "dose_era", "condition_era", 
      "person", "observation_period", "visit_occurrence", "visit_detail",
      "location", "care_site", "provider")
    sapply(main_tables, function(table) d[[table]] <- tibble::tibble())
    
    # Create m reactive value, for plugins & tabs data
    m <- reactiveValues()
    
    # Create o reactive values, for observers inactivation
    o <- reactiveValues()
    
    # App version
    r$app_version <- "0.2.0"
    
    # Col types of database tables, to import and restore database
    
    db_col_types <- tibble::tribble(
      ~table, ~col_types, ~db,
      "users", "icccciicl", "main",
      "users_accesses", "icccl", "main",
      "users_statuses", "icccl", "main",
      "data_sources", "iccicl", "main",
      "datasets", "icciicl", "main",
      "studies", "iciiiiccl", "main",
      "plugins", "iciccl", "main",
      "scripts", "icccl", "main",
      "patient_lvl_tabs_groups", "iccicl", "main",
      "patient_lvl_tabs", "icciiiicl", "main",
      "patient_lvl_widgets", "iciiiicl", "main",
      "aggregated_tabs_groups", "iccicl", "main",
      "aggregated_tabs", "icciiiicl", "main",
      "aggregated_widgets", "iciiiicl", "main",
      "code", "icicicl", "main",
      "options", "iciccnicl", "main",
      "messages", "iiicccicl", "main",
      "conversations", "iccl", "main",
      "user_deleted_conversations", "iiic", "main",
      "inbox_messages", "iiilcl", "main",
      "log", "icccic", "main",
      "git_repos", "iccccicl", "main",
      "persons_options", "iiiiiiciccnicl", "public",
      "patient_lvl_widgets_options", "iiiicccnicl", "public",
      "aggregated_widgets_options", "iiiicccnicl", "public",
      "subsets", "icciicl", "public",
      "subset_persons", "iiiicl", "public",
      "concept", "iiccccccccc", "public",
      "concept_dataset", "iiciiii", "public",
      "concept_user", "iiiccc", "public",
      "vocabulary", "icccccciicl", "public",
      "domain", "icci", "public",
      "concept_class", "icci", "public",
      "concept_relationship", "iiicccc", "public",
      "concept_relationship_user", "iicic", "public",
      "concept_relationship_evals", "iiicc", "public",
      "relationship", "iccccci", "public",
      "concept_synonym", "iici", "public",
      "concept_ancestor", "iiiii", "public",
      "drug_strength", "iiinininiiccc", "public",
      "patient_lvl_widgets_concepts", "iiicccilicl", "public",
      "aggregated_widgets_concepts", "iiicccilicl", "public"
    )
    
    # Test internet connection
    # If local is TRUE, don't use internet connection
    if (debug) cat(paste0("\n", Sys.time(), " - server - has_internet"))
    if (local) has_internet <- FALSE
    else has_internet <- curl::has_internet()
    r$has_internet <- has_internet
    
    # Perf monitoring & debug
    if (debug) cat(paste0("\n", Sys.time(), " - server - perf_monitoring"))
    r$perf_monitoring <- perf_monitoring
    r$debug <- debug
    
    r$perf_monitoring_table <- tibble::tibble(task = character(), datetime_start = lubridate::ymd_hms(), datetime_stop = lubridate::ymd_hms())
    datetime_start <- Sys.time()
    datetime_stop <- Sys.time()
    
    # Create r$server_tabs_groups_loaded & r$ui_tabs_groups_loaded
    r$server_tabs_groups_loaded <- ""
    r$ui_tabs_groups_loaded <- ""
    
    # App folder
    if (debug) cat(paste0("\n", Sys.time(), " - server - app_folder"))
    r$app_folder <- app_folder
    m$app_folder <- app_folder
    
    # App db folder
    app_db_folder <- paste0(app_folder, "/app_database")
    # r$app_db_folder <- app_db_folder
    
    # Get translations
    
    if (debug) cat(paste0("\n", Sys.time(), " - server - translations"))
    
    # translations_path <- "inst/translations"
    # if (!dir.exists(translations_path)) translations_path <- paste0(find.package("linkr"), "/translations")
    # if (!dir.exists(translations_path)) print("Translations path not found")
    # 
    # i18n <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = translations_path))
    # i18n$set_translation_language(language)
    r$i18n <- i18n
    r$languages <- languages
    
    # Save currently opened toggles (used to reload cards when we load a page, restart reactivity)
    r$activated_toggles <- ""
    
    # Connection to database
    # If connection informations have been given in linkr() function, use these informations
    if (debug) cat(paste0("\n", Sys.time(), " - server - app_db"))
    r$local_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(app_db_folder, "/linkr_main"))
    m$local_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(app_db_folder, "/linkr_public"))
    
    get_db(r = r, m = m, app_db_folder = app_db_folder)
    
    # r$db <- get_db(r = r, app_db_folder = app_db_folder, type = "main")
    # m$db <- get_db(r = r, app_db_folder = app_db_folder, type = "plugins")
    
    # Close DB connection on exit
    # And restore initial working directory
    trad <- list()
    trad$session <- switch(language, "fr" = "Session", "en" = "Session")
    trad$session_starts <- switch(language, "fr" = "Début de la session", "en" = "Session starts")
    trad$session_ends <- switch(language, "fr" = "Fin de la session", "en" = "Session ends")
    
    onStop(function() {
      if (debug) cat(paste0("\n", Sys.time(), " - server - observer onStop"))
      add_log_entry(r = isolate(r), category = trad$session, name = trad$session_ends, value = "")
      
      # Close duckdb connections
      DBI::dbDisconnect(isolate(r$db))
      if (length(isolate(r$duckdb_drv)) > 0) sapply(isolate(r$duckdb_drv), duckdb::duckdb_shutdown)
      
      # Close spark connections
      sparklyr::spark_disconnect_all()
    })
    
    # Add default values in database if database is empty
    # Load all data from database
    # Don't load concept, load it only when a vocabulary is selected
    # Don't load cache table neither
    
    observeEvent(r$db, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - server - observer r$db"))
      
      # Add default values in database, if it is empty
      insert_default_data(output = output, r = r, m = m, i18n = i18n, language = language, db_col_types = db_col_types, 
        users_accesses_toggles_options = users_accesses_toggles_options)
      
      # Load database
      load_database(r = r, m = m, i18n = i18n)
      
    })


    # Secure the app with shinymanager

    if (debug) cat(paste0("\n", Sys.time(), " - server - shinyManager"))
    r$res_auth <- shinymanager::secure_server(check_credentials = check_authentification(r$db))

    # Get user ID

    observeEvent(r$res_auth, {
      if (debug) cat(paste0("\n", Sys.time(), " - server - observer r$res_auth"))
      user_id <- as.integer(reactiveValuesToList(r$res_auth)$id)
      r$user_id <- user_id
      m$user_id <- user_id
      add_log_entry(r = r, category = trad$session, name = trad$session_starts, value = "")
    })

    # When r$user_id loaded, load user_accesses

    observeEvent(r$user_id, {
      if (debug) cat(paste0("\n", Sys.time(), " - server - observer r$user_id"))
      
      req(r$user_id)

      user_access_id <- r$users %>% dplyr::filter(id == r$user_id) %>% dplyr::pull(user_access_id)

      # Get user accesses
      r$user_accesses <- r$options %>% dplyr::filter(category == "users_accesses" & link_id == user_access_id & value_num == 1) %>% dplyr::pull(name)
      m$user_accesses <- r$user_accesses
      
      # Show username on top of the page
      r$username <- r$users %>% dplyr::filter(id == r$user_id)
      r$username <- paste0(r$username$firstname, " ", r$username$lastname)
    })
    
    # Code style for help pages
    r$code_style <- paste0("display:block; padding: 9.5px; margin: 0 10px 10px 0px; font-size:13px; line-height:1.42857143; color: #333; ",
      "word-break: break-all; word-wrap: break-word; background-color:#f5f5f5; border: 1px solid #ccc; border-radius: 4px;",
      "position:relative;")

    # Route pages
    if (debug) cat(paste0("\n", Sys.time(), " - server - shiny.router"))
    shiny.router::router_server()
    # router$server(input, output, session)
    
    # Clear temp dir
    if (debug) cat(paste0("\n", Sys.time(), " - server - clear temp_files"))
    for (folder in c("markdowns", "plugins", "scripts", "vocabularies", "app_db")){
      unlink(paste0(app_folder, "/temp_files/", folder), recursive = TRUE, force = TRUE)
      if (!dir.exists(paste0(app_folder, "/temp_files/", folder))) dir.create(paste0(app_folder, "/temp_files/", folder))
    }
    # Do not clear git_repos folder in temp_files
    if (!dir.exists(paste0(app_folder, "/temp_files/git_repos"))) dir.create(paste0(app_folder, "/temp_files/git_repos"))
    
    # Keep trace of loaded observers (not to have multiple identical observers)
    r$loaded_observers <- ""

    # Load tabs
    # Don't load tabs user has no access to
    
    observeEvent(r$user_accesses, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - server - observer r$user_accesses"))
      
      # --- --- --- --- --- -- -
      # Get authorized data ----
      # --- --- --- --- --- -- -
      
      sapply(c("datasets", "plugins"), function(table){
        if (paste0(table, "_see_all_data") %not_in% r$user_accesses){
          if (nrow(r[[table]] > 0)){
            r[[table]] <- get_authorized_data(r = r, table = table)
            r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
          }
        }
      })

      # --- --- --- --- --- -- -
      # Load server tabs ----
      # --- --- --- --- --- -- -
    
      if (debug) cat(paste0("\n", Sys.time(), " - server - load server tabs"))
      if (perf_monitoring) monitor_perf(r = r, action = "start")

      if (debug) cat(paste0("\n", Sys.time(), " - server - load server tabs - home"))
      sapply(c("home", "home_get_started", "home_tutorials", "home_resources", "home_dev"), function(page){
        mod_home_server(page, r, language, i18n, perf_monitoring, debug, show_home_page)
        mod_page_header_server(page, r, language, i18n)
      })

      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server tabs - home")
      if (debug) cat(paste0("\n", Sys.time(), " - server - load server tabs - data"))

      sapply(c("patient_level_data", "aggregated_data"), function(page){
        mod_data_server(page, r, d, m, language, i18n, perf_monitoring, debug)
        mod_page_sidenav_server(page, r, d, m, i18n, language, perf_monitoring, debug)
        mod_page_header_server(page, r, language, i18n)
      })

      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server tabs - data")
      if (debug) cat(paste0("\n", Sys.time(), " - server - load server tabs - my_studies / my_subsets / vocabularies / scripts"))

      mod_my_studies_server("my_studies", r, d, m, i18n, language, perf_monitoring, debug)
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server tabs - my_studies")
      mod_my_subsets_server("my_subsets", r, d, m, i18n, language, perf_monitoring, debug)
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server tabs - my_subsets")
      mod_vocabularies_server("vocabularies", r, d, m, i18n, language, perf_monitoring, debug)
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server tabs - vocabularies")
      mod_scripts_server("scripts", r, d, m, language, i18n, perf_monitoring, debug)
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server tabs - scripts")

      sapply(c("my_studies", "my_subsets", "vocabularies", "scripts"), function(page){
        mod_page_sidenav_server(page, r, d, m, i18n, language, perf_monitoring, debug)
        mod_page_header_server(page, r, language, i18n)
      })

      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server tabs - my_studies / my_subsets / vocabularies / scripts - sidenav")
      if (debug) cat(paste0("\n", Sys.time(), " - server - load server tabs - messages"))
      
      mod_messages_server("messages", r, d, m, i18n, language, perf_monitoring, debug)
      mod_page_sidenav_server("messages", r, d, m, i18n, language, perf_monitoring, debug)
      mod_page_header_server("messages", r, language, i18n)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server tabs - messages")
      if (debug) cat(paste0("\n", Sys.time(), " - server - load server tabs - plugins"))

      sapply(c("plugins_patient_lvl", "plugins_aggregated"), function(page){
        mod_plugins_server(page, r, d, m, language, i18n, perf_monitoring, debug)
        mod_page_sidenav_server(page, r, d, m, i18n, language, perf_monitoring, debug)
        mod_page_header_server(page, r, language, i18n)
      })
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server tabs - plugins")
      if (debug) cat(paste0("\n", Sys.time(), " - server - load server tabs - general_settings"))

      mod_settings_general_server("settings_general_settings", r, i18n, perf_monitoring, debug)
      mod_page_sidenav_server("settings_general_settings", r, d, m, i18n, language, perf_monitoring, debug)
      mod_page_header_server("settings_general_settings", r, language, i18n)
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server tabs - general_settings")
      if (debug) cat(paste0("\n", Sys.time(), " - server - load server tabs - settings_app_db"))

      mod_settings_app_database_server("settings_app_db", r, m, i18n, language, db_col_types, app_folder, perf_monitoring, debug)
      mod_page_sidenav_server("settings_app_db", r, d, m, i18n, language, perf_monitoring, debug)
      mod_page_header_server("settings_app_db", r, language, i18n)
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server tabs - settings_app_db")
      if (debug) cat(paste0("\n", Sys.time(), " - server - load server tabs - settings_git"))

      mod_settings_git_server("settings_git", r, d, m, i18n, language, perf_monitoring, debug)
      mod_page_header_server("settings_git", r, language, i18n)
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server tabs - settings_git")
      if (debug) cat(paste0("\n", Sys.time(), " - server - load server tabs - settings_users"))

      mod_settings_users_server("settings_users", r, m, i18n, language, perf_monitoring, debug, users_accesses_toggles_options)
      mod_page_sidenav_server("settings_users", r, d, m, i18n, language, perf_monitoring, debug)
      mod_page_header_server("settings_users", r, language, i18n)

      sapply(c("users", "users_statuses", "users_accesses"), function(page){
        mod_settings_users_server(paste0("settings_users_", page, "_creation"), r, m, i18n, language, perf_monitoring, debug, users_accesses_toggles_options)
        mod_settings_users_server(paste0("settings_users_", page, "_management"), r, m, i18n, language, perf_monitoring, debug, users_accesses_toggles_options)
        mod_settings_users_server(paste0("settings_users_", page, "_options"), r, m, i18n, language, perf_monitoring, debug, users_accesses_toggles_options)
      })

      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server tabs - settings_users")
      if (debug) cat(paste0("\n", Sys.time(), " - server - load server tabs - settings_dev"))
      mod_settings_dev_server("settings_dev", r, d, m, i18n, language, perf_monitoring, debug)
      mod_page_sidenav_server("settings_dev", r, d, m, i18n, language, perf_monitoring, debug)
      mod_page_header_server("settings_dev", r, language, i18n)

      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server tabs - settings_dev")
      if (debug) cat(paste0("\n", Sys.time(), " - server - load server tabs - data_sources / datasets / vocabularies"))

      sapply(c("data_sources", "datasets", "vocabularies"), function(page){
        mod_settings_data_management_server(paste0("settings_", page), r, d, m, i18n, language, perf_monitoring, debug)
        mod_page_sidenav_server(paste0("settings_", page), r, d, m, i18n, language, perf_monitoring, debug)
        mod_page_header_server(paste0("settings_", page), r, language, i18n)
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("server - load server tabs - ", page))
      })

      if (debug) cat(paste0("\n", Sys.time(), " - server - load server tabs - settings_log"))
      mod_settings_log_server("settings_log", r, i18n, language, perf_monitoring, debug)
      mod_page_sidenav_server("settings_log", r, d, m, i18n, language, perf_monitoring, debug)
      mod_page_header_server("settings_log", r, language, i18n)
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server tabs - settings_log")
      if (debug) cat(paste0("\n", Sys.time(), " - server - load server tabs - end"))
      
      r$end_load_tabs <- TRUE
      
    })
  }
}

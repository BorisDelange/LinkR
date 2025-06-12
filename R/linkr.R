#' Run the LinkR Shiny Application
#'
#' @description 
#' Launches the LinkR Shiny application for health data science.
#' You can configure the interface language, authentication mode, runtime behavior, and logging preferences
#' 
#' Use `language` to set the application language (`"en"` or `"fr"`).
#' 
#' Use `app_folder` to define where LinkR will store application data and configuration files. 
#' If not specified, a `linkr` folder will be created in the user's home directory.
#' 
#' Use `username` to auto-connect as a specific user when `authentication = FALSE`. 
#' This allows you to bypass the login screen entirely during development or demo sessions.
#' 
#' Use `log_level` to control which log messages are displayed. It accepts a character vector containing any combination of:
#' `"info"` (standard information), `"error"` (errors only), and `"event"` (application lifecycle events).
#' To disable all logs, use `"none"` or leave the argument empty.
#'
#' Use `log_target` to define where logs are displayed: `"console"` (developer mode) or `"app"` (to display logs in the LinkR interface).
#'
#' @param language Language to use in the app (`"en"` or `"fr"`). (character)
#' @param app_folder Path to the folder where application files will be stored. (character)
#' @param authentication Should user authentication be enabled? (logical)
#' @param username Username to auto-connect with when `authentication = FALSE` (character). Ignored if authentication is enabled.
#' @param local If `TRUE`, runs the app in local mode without loading external files (e.g., from GitHub). (logical)
#' @param log_level Character vector of log levels to display. Can include `"info"`, `"error"`, `"event"`, or be set to `"none"` or an empty vector to disable logging.
#' @param log_target Destination for log messages: `"console"` or `"app"`. (character)
#' @param port Port used to run the Shiny app. (integer)
#' @param host Host address to run the app on. Default is `"0.0.0.0"`. (character)
#' @param loading_options A list of startup options (e.g., page, project, subset to load). Should include named elements like `page`, `project_id`, `load_data_page`, `subset_id`, `person_id`.
#'
#' @examples
#' \dontrun{
#' # Run LinkR in English with default settings
#' run_app()
#'
#' # Run LinkR in French
#' run_app(language = "fr")
#'
#' # Specify a custom folder to store configuration and user data
#' run_app(app_folder = "~/my_linkr_data")
#'
#' # Enable user authentication (will prompt login screen)
#' run_app(authentication = TRUE)
#'
#' # Disable authentication and auto-connect as a predefined user
#' run_app(authentication = FALSE, username = "admin")
#'
#' # Run the app in local mode (without loading from GitHub)
#' run_app(local = TRUE)
#'
#' # Customize logging: show only info and error messages in the console
#' run_app(log_level = c("info", "error"), log_target = "console")
#'
#' # Disable all logging
#' run_app(log_level = "none")
#'
#' # Run the app on a custom host and port (e.g., for deployment)
#' run_app(host = "127.0.0.1", port = 8080)
#'
#' # Automatically load a specific page or project at startup
#' # This is especially useful for online demos or when sharing a reproducible scenario
#' run_app(loading_options = list(
#'   page = "home",
#'   project_id = "project123",
#'   load_data_page = "patient_lvl",
#'   subset_id = "subset_A",
#'   person_id = "patient42"
#' ))
#' }
#' 
#' @export
#' @importFrom shiny shinyApp
#' @importFrom magrittr %>%

run_app <- function(
  language = "en",
  app_folder = character(),
  authentication = FALSE,
  username = "admin",
  local = FALSE,
  log_level = c("error"),
  log_target = "app",
  port = 3838,
  host = "0.0.0.0",
  loading_options = list()
) {
  
  app_version <- "0.3.1.9011"
  if ("event" %in% log_level) cat(paste0("[", now(), "] [EVENT] [page_id = linkr] init LinkR - v", app_version))
  
  # Loading options ----
  # - loading_options$page: load a specific page
  # - loading_options$project_id: load a project
  # - loading_options$load_data_page: load a data page ("patient_lvl" or "aggregated")
  # - loading_options$subset_id: load a subset_id the first time a project is loaded
  # - loading_options$person_id: load a person_id the first time a subset is loaded
  
  # Check version of shiny.fluent ----
  # shiny.fluent version has to be inferior to 0.4.0
  if ("event" %in% log_level) cat(paste0("\n[", now(), "] [EVENT] [page_id = linkr] check shiny.fluent version"))
  if (packageVersion("shiny.fluent") >= "0.4.0") stop("Invalid shiny.fluent version: version 0.3.0 is required. Install it with remotes::install_github('Appsilon/shiny.fluent', ref = 'dd1c956').")
  
  # Check arguments ----
  if ("event" %in% log_level) cat(paste0("\n[", now(), "] [EVENT] [page_id = linkr] check linkr arguments"))
  
  if (log_target %not_in% c("console", "app")) stop("'log_target' argument must be 'console' or 'app'")
  if (!is.logical(local)) stop("'local' argument is not of logical type")
  if (!is.logical(authentication)) stop("'authentication' argument is not of logical type")
  if (!authentication){
    error_username <- TRUE
    if (length(username) > 0) if (!is.na(username) & username != "") error_username <- FALSE
    if (error_username) stop("'username' argument is invalid")
  }
  
  # R options ----
  if ("event" %in% log_level) cat(paste0("\n[", now(), "] [EVENT] [page_id = linkr] set R options"))
  
  # Set umask 002 for files creation
  Sys.umask("002")
  
  # Maximum size for uploaded data (4096 MB)
  # Used to restore database and import vocabularies
  # shiny.launch.browser to automatically open browser
  
  options(shiny.maxRequestSize = 4096*1024^2, shiny.launch.browser = TRUE, shiny.port = port, shiny.host = host, digits.secs = 0)
  
  # App folder ----
  
  # Create app folder if it doesn't exist
  if ("event" %in% log_level) cat(paste0("\n[", now(), "] [EVENT] [page_id = linkr] create app_folder"))
  
  if (length(app_folder) == 0) app_folder <- paste0(path.expand("~"), "/linkr")
  
  if (!dir.exists(app_folder)){
    tryCatch(
      dir.create(app_folder),
      error = function(e) print(e)
    )
  }
  
  # Create app sub-dirs
  if ("event" %in% log_level) cat(paste0("\n[", now(), "] [EVENT] [page_id = linkr] create app sub-dirs"))
  
  sub_dirs <- c(
    "app_database",
    "data_cleaning",
    "datasets",
    "datasets_files",
    "log",
    "messages",
    "plugins",
    "projects",
    "projects_files",
    "subsets",
    "temp_files",
    "translations"
  )
  for (sub_dir in sub_dirs) if (!dir.exists(paste0(app_folder, "/", sub_dir))) dir.create(paste0(app_folder, "/", sub_dir))
  
  # Load translations ----
  
  if ("event" %in% log_level) cat(paste0("\n[", now(), "] [EVENT] [page_id = linkr] load translations"))
  
  if (language %not_in% get_languages(get_translations = FALSE)){
    cat("\n")
    stop(paste0(paste("Language is not valid. Valid languages are", paste(languages, collapse = ", ")), "."))
  }
  
  translations_path <- "inst/translations"
  if (!dir.exists(translations_path)) translations_path <- paste0(find.package("linkr"), "/translations")
  if (!dir.exists(translations_path)) print("Translations path not found")
  
  i18n <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = translations_path))
  i18n$set_translation_language(language)
  
  # Connection to database ----
  # If connection informations have been given in linkr() function, use these informations
  if ("event" %in% log_level) cat(paste0("\n[", now(), "] [EVENT] [page_id = linkr] load app database"))
  
  app_db_folder <- paste0(app_folder, "/app_database")
  main_local_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(app_db_folder, "/linkr_main"))
  public_local_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(app_db_folder, "/linkr_public"))
  db <- get_db()
  
  # Test internet connection ----
  if ("event" %in% log_level) cat(paste0("\n[", now(), "] [EVENT] [page_id = linkr] internet connection test"))
  
  # If local is TRUE, don't use internet connection
  if (local) has_internet <- FALSE
  else has_internet <- curl::has_internet()
  
  # Load Shiny app ----
  
  if ("event" %in% log_level) cat(paste0("\n[", now(), "] [EVENT] [page_id = linkr] load Shiny app"))
  
  variables_list <- c(
    "app_folder", "app_version", "authentication", "db", "has_internet", "i18n", "language",
    "loading_options","local", "log_level", "log_target", "username"
  )
  
  shinyApp(
    ui = app_ui(),
    server = app_server(),
    options = options
  )
}

#' @rdname run_app
#' @export
linkr <- run_app

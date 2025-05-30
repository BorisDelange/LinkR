#' Run the LinkR Shiny Application
#'
#' @description 
#' Launches the LinkR Shiny application for health data science. \cr
#' You can configure the interface language, authentication mode, runtime behavior, and logging preferences.\cr
#' 
#' Use `language` to set the application language (`"en"` or `"fr"`). \cr
#' Use `app_folder` to define where LinkR will store application data and configuration files. 
#' If not specified, a `linkr` folder will be created in the user's home directory. \cr
#' 
#' Use `log_level` to control which log messages are displayed. It accepts a character vector containing any combination of:
#' `"info"` (standard information), `"error"` (errors only), and `"event"` (application lifecycle events). \cr
#' To disable all logs, use `"none"` or leave the argument empty. \cr
#'
#' Use `log_target` to define where logs are displayed: `"console"` (developer mode) or `"app"` (to display logs in the LinkR interface).
#'
#' @param language Language to use in the app (`"en"` or `"fr"`). (character)
#' @param app_folder Path to the folder where application files will be stored. (character)
#' @param authentication Should user authentication be enabled? (logical)
#' @param username Default username used for authentication (only relevant if `authentication = FALSE`). (character)
#' @param local If `TRUE`, runs the app in local mode without loading external files (e.g., from GitHub). (logical)
#' @param log_level Character vector of log levels to display. Can include `"info"`, `"error"`, `"event"`, or be set to `"none"` or an empty vector to disable logging.
#' @param log_target Destination for log messages: `"console"` or `"app"`. (character)
#' @param port Port used to run the Shiny app. (integer)
#' @param host Host address to run the app on. Default is `"0.0.0.0"`. (character)
#' @param loading_options A list of startup options (e.g., page, project, subset to load). Should include named elements like `page`, `project_id`, `load_data_page`, `subset_id`, `person_id`.
#'
#' @examples 
#' \dontrun{
#' linkr(log_level = c("info", "event"), log_target = "console")
#' linkr(log_level = "none")
#' linkr(log_level = character(0))  # same as disabling all logs
#' }
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom magrittr %>%

linkr <- function(
  language = "en",
  app_folder = character(),
  authentication = FALSE,
  username = "admin",
  local = FALSE,
  log_level = c("event", "error"),
  log_target = "app",
  port = 3838,
  host = "0.0.0.0",
  loading_options = list()
) {
  
  if ("event" %in% log_level) cat(paste0("[", now(), "] [EVENT] [page_id = linkr] init LinkR - v0.3.1.9008"))
  
  # Loading options
  # - loading_options$page: load a specific page
  # - loading_options$project_id: load a project
  # - loading_options$load_data_page: load a data page ("patient_lvl" or "aggregated")
  # - loading_options$subset_id: load a subset_id the first time a project is loaded
  # - loading_options$person_id: load a person_id the first time a subset is loaded
  
  # Check version of shiny.fluent (has to be inferior to 0.4.0)
  if (packageVersion("shiny.fluent") >= "0.4.0") stop("Invalid shiny.fluent version: version 0.3.0 is required. Install it with remotes::install_github('Appsilon/shiny.fluent', ref = 'dd1c956').")
  
  # Check arguments
  if (log_target %not_in% c("console", "app")) stop("'log_target' argument must be 'console' or 'app'")
  
  # Set umask 002 for files creation
  Sys.umask("002")
  
  # Maximum size for uploaded data (4096 MB)
  # Used to restore database and import vocabularies
  # shiny.launch.browser to automatically open browser
  
  options(shiny.maxRequestSize = 4096*1024^2, shiny.launch.browser = TRUE, shiny.port = port, shiny.host = host, digits.secs = 0)
  
  if (!is.logical(local)) stop("'local' argument is not of logical type")
  if (!is.logical(authentication)) stop("'authentication' argument is not of logical type")
  
  if (!authentication){
    error_username <- TRUE
    if (length(username) > 0) if (!is.na(username) & username != "") error_username <- FALSE
    if (error_username) stop("'username' argument is invalid")
  }
  
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
  
  # Load translations
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
  
  # Load UI & server
  
  if ("event" %in% log_level) cat(paste0("\n[", now(), "] [EVENT] [page_id = linkr] load UI & server"))
  
  # Load Shiny App
  
  variables_list <- c("language", "i18n", "app_folder", "authentication", "username", "log_level", "log_target", "local", "loading_options")
  
  shinyApp(
    ui = app_ui(),
    server = app_server(),
    options = options
  )
}

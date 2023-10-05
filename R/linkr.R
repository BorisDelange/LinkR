#' Run the Shiny Application
#'
#' @description 
#' Runs the LinkR Shiny Application.\cr
#' Use language argument to choose language to use ("en" or "fr" available).\cr
#' Use app_folder argument to choose a folder where the files of the application will be saved. By default, 
#' a 'linkr' folder will be created in the folder returned by path.expand("~").\cr
#' @param language Default language to use in the App (character)
#' @param app_folder Location of the application folder (character).
#' @param local Run the app in local mode, do not load files on the internet (logical)
#' @param show_home_page Should the home page be loaded ? (logical)
#' @param perf_monitoring Monitor app performances (logical)
#' @param debug Debug mode : steps and errors will by displayed in the console (logical)
#' @examples 
#' \dontrun{
#' linkr(language = "en", perf_monitoring = FALSE, debug = FALSE, local = FALSE)
#' }
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options 
#' @importFrom magrittr %>%

linkr <- function(
  language = "en",
  app_folder = character(),
  local = FALSE,
  show_home_page = TRUE,
  perf_monitoring = FALSE,
  debug = FALSE
) {
  
  # Maximum size for uploaded data (4096 MB)
  # Used to restore database and import vocabularies
  # shiny.launch.browser to automatically open browser
  
  if (debug) cat(paste0(Sys.time(), " - linkr - init"))
  options(shiny.maxRequestSize = 4096*1024^2, shiny.launch.browser = TRUE)
  
  # suppressMessages(require(shinyTree))
  
  if (!is.logical(perf_monitoring) | !is.logical(debug) | !is.logical(local)) stop("perf_monitoring, debug or local are not logical")
  
  # Create app folder if it doesn't exist
  if (debug) cat(paste0("\n", Sys.time(), " - linkr - app_folder"))
  if (length(app_folder) == 0) app_folder <- paste0(path.expand("~"), "/linkr")
  
  if (!dir.exists(app_folder)){
    tryCatch(
      dir.create(app_folder),
      error = function(e) print(e)
    )
  }
  
  # Create app sub-dirs
  if (debug) cat(paste0("\n", Sys.time(), " - linkr - app sub-dirs"))
  sub_dirs <- c("app_database", "datasets", "datasets_files",
    "home", "home/fr", "home/en", 
    "home/fr/home", "home/fr/get_started", "home/fr/tutorials", "home/fr/resources",
    "home/en/home", "home/en/get_started", "home/en/tutorials", "home/en/resources",
    "messages", "plugins", "studies", "studies_files", "temp_files", "translations", "vocabularies")
  for (sub_dir in sub_dirs) if (!dir.exists(paste0(app_folder, "/", sub_dir))) dir.create(paste0(app_folder, "/", sub_dir))
  
  # Load translations
  if (debug) cat(paste0("\n", Sys.time(), " - linkr - translation"))
  
  languages <- c("en", "fr")
  if (language %not_in% languages) print("Valid languages are 'en' (english) and 'fr' (french)")
  
  translations_path <- "inst/translations"
  if (!dir.exists(translations_path)) translations_path <- paste0(find.package("linkr"), "/translations")
  if (!dir.exists(translations_path)) print("Translations path not found")
  
  i18n <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = translations_path))
  i18n$set_translation_language(language)
  
  languages <- tibble::tribble(
    ~code, ~language, 
    "en", i18n$t("english"),
    "fr", i18n$t("french")#,
    # "it", i18n$t("italian"),
    # "es", i18n$t("spanish")
  )
  
  options(digits.secs = 0)

  css <- "fluent_style.css"
  
  # Toggles for users accesses
  
  users_accesses_toggles_options <- tibble::tribble(
    ~name, ~toggles,
    "general_settings", "change_password_card",
    "app_db", c(
      "db_connection_infos_card",
      "db_datatable_card",
      "db_request_card",
      "db_save_card",
      "db_restore_card"),
    "remote_git_repos", c(
      "git_add_repo_card", 
      "git_repos_management_card",
      "git_repo_options_card",
      "git_edit_repo_card"),
    "users", c(
      "users_creation_card",
      "users_management_card",
      "users_accesses_management_card",
      "users_accesses_options_card",
      "users_statuses_management_card"),
    "dev", c(
      "dev_edit_code_card",
      "dev_perf_monitoring_card"),
    "data_sources", c(
      "data_sources_datatable_card"),
    "datasets", c(
      "datasets_see_all_data",
      "datasets_datatable_card",
      "datasets_options_card",
      "datasets_edit_code_card"),
    "studies", c(
      "studies_see_all_data",
      # "studies_messages_card",
      # "studies_description_card",
      "studies_datatable_card",
      "studies_options_card"),
    "subsets", c(
      "subsets_datatable_card",
      "subsets_edit_code_card",
      "subsets_persons_card"),
    "data", c(
      "data_console"),
    "vocabularies", c(
      "vocabularies_concepts_card",
      "vocabularies_mapping_card",
      "vocabularies_datatable_card",
      "vocabularies_vocabularies_tables_datatable_card",
      "vocabularies_edit_code_card",
      "vocabularies_import_vocabulary_card"),
    "messages", c(
      "study_messages_card"),
    "plugins", c(
      "plugins_see_all_data",
      "all_plugins_card",
      "plugins_datatable_card",
      "plugins_options_card",
      "plugins_edit_code_card",
      "import_plugin_card",
      "export_plugin_card"),
    "scripts", c(
      "dataset_scripts_card",
      "all_scripts_card",
      "scripts_datatable_card",
      "scripts_edit_code_card",
      "scripts_options_card",
      "import_script_card",
      "export_script_card"
    ),
    "log", c(
      "all_users",
      "only_me")
  )
  
  # Load UI & server
  
  if (debug) cat(paste0("\n", Sys.time(), " - linkr - load UI & server"))
  with_golem_options(
    app = shinyApp(
      ui = app_ui(css = css, language = language, languages = languages, i18n = i18n, users_accesses_toggles_options = users_accesses_toggles_options, debug = debug),
      server = app_server(language = language, languages = languages, i18n = i18n, app_folder = app_folder, 
        perf_monitoring = perf_monitoring, debug = debug, local = local, show_home_page = show_home_page,
        users_accesses_toggles_options = users_accesses_toggles_options),
      options = options
    ), 
    golem_opts = list()
  )
}
